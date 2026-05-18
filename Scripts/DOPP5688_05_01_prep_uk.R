# ==============================================================================
# PREP SCRIPT: UK 2024 Manifesto Processing
# Case Study: Labour vs. Conservatives (General Election 2024)
#
# WHAT THIS SCRIPT DOES:
#   This is a *preparation* script that I ran before class.
#   It downloads raw manifesto text, cleans it, runs two transformer models
#   (one for sentiment, one for embeddings), and saves the results as a single
#   tidy data file. In class, you will load that file and go straight to
#   analysis — no waiting for GPU inference.
#   However, you can run this at home as well. Just please don't run it in class!
#
# PARTY CODES (MARPOR):
#   Labour Party       -> party == 51320
#   Conservative Party -> party == 51620
#   Country: UK        -> country == 51
# ==============================================================================


# --- ENVIRONMENT SETUP --------------------------------------------------------

setwd("~/Dropbox/University/CEU/Teaching/DOPP5688 - NLP/")

# This class is a bit awkward because most of the transformer work is done in 
# Python and Rust. If we want to use the transformers, we need to have R speak 
# Python...
# reticulate is R's bridge to Python. We need it because the transformer models
# we use later (DistilBERT, MPNet) are Python libraries from HuggingFace.
# use_condaenv() tells reticulate which Python environment to activate — the
# one that was set up specifically for the 'text' package with textrpp_install().
# We wrap it in try(..., silent = TRUE) so the script doesn't crash if the
# environment is already active (which happens when re-running interactively).
library(reticulate)
try(use_condaenv("textrpp_condaenv", required = TRUE), silent = TRUE)

library(tidyverse)    # data wrangling and plotting
library(manifestoR)   # access to the Manifesto Project corpus and metadata
library(quanteda)     # not used directly here, but loaded as manifestoR depends on it
library(text)         # R interface to HuggingFace transformer models

# textrpp_initialize() starts the Python session and loads the required Python
# packages (torch, transformers, sentence-transformers) into R's memory.
# This must be called before any transformer model is used.
textrpp_initialize()

# The Manifesto Project API requires authentication
mp_setapikey("~/Dropbox/Research/R/manifesto_apikey.txt")


# --- STEP 1: DATA INGESTION ---------------------------------------------------
# The Manifesto Project (MARPOR) codes every sentence of party manifestos
# with a policy category. mp_maindataset() downloads the full metadata table
# covering hundreds of parties across decades. We then filter it down to just
# the two UK parties we care about and find the most recent election.

mp_data <- mp_maindataset()

uk_recent_meta <- mp_data |>
  # country == 51 is the UK in the MARPOR coding scheme
  # party %in% c(...) selects Labour and the Conservatives
  filter(country == 51, party %in% c(51320, 51620)) |>
  # Sort by election date descending so the most recent election is first
  arrange(desc(edate)) |>
  # Keep only rows from the single most recent election.
  # edate[1] is the most recent date after sorting, so this line keeps only
  # the two documents (one per party) from that election.
  filter(edate == edate[1])

# mp_corpus() uses the filtered metadata to download the actual manifesto texts
# from the Manifesto Project API. The result is a ManifestoCorpus object —
# a list-like structure where each element is one party's full manifesto.
manifesto_docs <- mp_corpus(uk_recent_meta)

# Clean up: we no longer need the large metadata table in memory.
rm(mp_data, uk_recent_meta)


# --- STEP 2: BUILD A FLAT DATA FRAME OF SENTENCES ----------------------------
# ManifestoCorpus objects are awkward to work with directly (they don't behave
# like standard R data frames). The safest approach is to extract the raw text
# immediately and build a plain tibble — one row per quasi-sentence.
#
# manifestoR splits manifestos into 'quasi-sentences': the unit of annotation
# used by human coders. These are roughly sentence-length but sometimes
# shorter (a clause) or longer (two fused sentences). They are the natural
# unit of analysis for this kind of text.
#
# map_dfr() loops over each document in the corpus, extracts its text as a
# plain character vector via content(), wraps it in a tibble, and row-binds
# all documents together into one flat data frame.

df_sentences <- map_dfr(names(manifesto_docs), function(doc_id) {
  tibble(
    doc_id     = doc_id,
    # The doc_id format is "PARTYCODE_YEAR" (e.g. "51320_202407").
    # str_extract pulls the numeric party code from the start of the string.
    party_code = as.integer(str_extract(doc_id, "^[0-9]+")),
    # content() returns the raw character vector of quasi-sentences
    text       = content(manifesto_docs[[doc_id]])
  )
}) |>
  # Convert the numeric party code to a readable label
  mutate(
    party_name = if_else(party_code == 51620, "Conservative", "Labour")
  ) |>
  # Remove noise: very short strings are usually headers or page numbers;
  # very long ones are formatting artefacts or merged paragraphs.
  # The thresholds (>20 and <800 characters) were chosen by inspection.
  filter(nchar(text) > 20, nchar(text) < 800) |>
  # Create a within-party sentence index so we can plot the manifesto as a
  # sequence (start to finish) for each party separately in the analysis.
  group_by(party_name) |>
  mutate(sentence_index = row_number()) |>
  ungroup()

cat("Sentences per party:\n")
print(count(df_sentences, party_name))


# --- STEP 3: SENTIMENT ANALYSIS (DistilBERT) ----------------------------------
# We use a transformer model fine-tuned for sentiment analysis to score every
# quasi-sentence on a scale from 0 (very negative) to 1 (very positive).
#
# MODEL CHOICE: distilbert-base-uncased-finetuned-sst-2-english
#   DistilBERT is a 'distilled' (compressed) version of BERT that is ~40%
#   smaller and ~60% faster with only a ~3% drop in accuracy. It was
#   fine-tuned on the Stanford Sentiment Treebank (SST-2), a large dataset
#   of movie reviews labelled as Positive or Negative. This makes it well-
#   suited for detecting the general positive/negative tone of political text.
#
# WHY NOT USE A POLITICAL SCIENCE-SPECIFIC MODEL?
#   Fine-tuned political sentiment models exist but are trained on narrow
#   domains. The SST-2 model generalises well to political language and its
#   outputs are easy to interpret.

# We access the model directly via HuggingFace's Python 'transformers' library
# using reticulate
transformers <- reticulate::import("transformers")

# transformers$pipeline() is HuggingFace's high-level inference API.
# It handles tokenisation, model inference, and decoding automatically.
# device = 0L sends computation to the first GPU (CUDA device 0).
# STUDENTS: Change to device = -1L to run on CPU if no GPU is available 
# Just note that this will be much slower.
sentiment_pipeline <- transformers$pipeline(
  "sentiment-analysis",
  model  = "distilbert-base-uncased-finetuned-sst-2-english",
  device = 0L
)

# We process sentences in batches of 64 rather than all at once.
# Sending thousands of sentences to the GPU simultaneously can exhaust VRAM.
# A batch size of 64 is a safe default for a ~6GB GPU; 
# STUDENTS: increase if you have  more VRAM, 
#           decrease if you get out-of-memory errors.
batch_size <- 64L
results     <- list()

for (i in seq(1, nrow(df_sentences), by = batch_size)) {
  batch <- df_sentences$text[i:min(i + batch_size - 1, nrow(df_sentences))]
  results[[length(results) + 1]] <- sentiment_pipeline(batch)
}

# The pipeline returns a nested list (one sub-list per sentence, each with
# $label and $score). We flatten and parse this into a tidy tibble.
# The model outputs a label ("POSITIVE" or "NEGATIVE") and a confidence score.
# The score reflects confidence in the predicted label, not a direct measure
# of positivity — so we need to convert it:
#   - If the label is POSITIVE: the score is already a positivity measure.
#   - If the label is NEGATIVE: the positivity score is 1 - score, because
#     high confidence in negativity means low positivity.
# This gives us a unified [0, 1] positivity scale across all sentences.
sent_res <- map_dfr(unlist(results, recursive = FALSE), function(r) {
  tibble(label = r$label, score = as.numeric(r$score))
}) |>
  mutate(
    positivity_score = if_else(
      str_detect(tolower(label), "pos"),
      score,       # POSITIVE sentence: keep the confidence score
      1 - score    # NEGATIVE sentence: flip the score
    )
  )

# Attach the positivity scores back to our sentence data frame.
# Note: this works because sent_res rows are in the same order as df_sentences.
# The stopifnot() guards against a silent length mismatch, which would
# misalign scores and sentences — a hard-to-detect but serious error.
stopifnot(nrow(sent_res) == nrow(df_sentences))
df_sentences$positivity_score <- sent_res$positivity_score


# --- STEP 4: SENTENCE EMBEDDINGS (all-mpnet-base-v2) -------------------------
# An embedding is a dense numeric vector that represents the *meaning* of a
# text. Sentences with similar meanings end up close together in this high-
# dimensional space. We will use these vectors in class for clustering,
# ideological scaling, and visualisation.
#
# MODEL CHOICE: sentence-transformers/all-mpnet-base-v2
#   This model was specifically trained to produce high-quality *sentence-level*
#   embeddings (as opposed to word-level). It is based on Microsoft's MPNet
#   architecture and fine-tuned on over 1 billion sentence pairs. As of 2024
#   it remains one of the strongest general-purpose English sentence embedding
#   models on the MTEB benchmark.
#
# layers = -2
#   Transformer models have multiple layers, each capturing different levels
#   of linguistic abstraction (syntax, semantics, discourse). The second-to-
#   last layer (-2) has been consistently shown in the research literature to
#   produce the most useful contextual representations for downstream tasks —
#   the final layer is often too task-specific from pre-training.
#   Using a named layer (-2) is also more robust across package versions than
#   specifying absolute layer numbers.
#
# aggregation_from_tokens_to_texts = "mean"
#   A transformer produces one vector per *token* (roughly per word-piece).
#   To get a single vector for the whole sentence, we average (mean-pool) all
#   token vectors. Mean pooling is the standard approach and works well in
#   combination with sentence-transformer models that were trained with it.

cat("Generating embeddings...\n")

embeddings_obj <- textEmbed(
  texts = df_sentences$text,
  model = "sentence-transformers/all-mpnet-base-v2",
  layers = -2,
  device = "cuda",
  aggregation_from_tokens_to_texts = "mean"
)

# textEmbed() returns a list. The $texts element contains the sentence-level
# embeddings as a data frame. [[1]] selects the first (and only) text column.
# We convert to a matrix for the geometric operations we'll do in class, and
# give each dimension a name (emb_1, emb_2, ...) for clarity.
# The model produces 768-dimensional vectors — each sentence becomes a point
# in a 768-dimensional semantic space.
names(embeddings_obj$texts)
m_emb <- as.matrix(embeddings_obj$texts$texts)
colnames(m_emb) <- paste0("emb_", seq_len(ncol(m_emb)))

# Bind the 768 embedding dimensions as new columns onto our sentence data frame.
# This keeps everything — text, metadata, sentiment, and embeddings — in one
# object, which makes the student analysis script much simpler to follow.
df_sentences <- bind_cols(df_sentences, as_tibble(m_emb))


# --- STEP 5: SAVE -------------------------------------------------------------
# We save the complete data frame as an RDS file (R's native binary format).
# RDS preserves column types exactly and loads faster than CSV.
# In class, you simply call readRDS("uk_manifesto_data.rds") and have
# everything you need — text, party labels, sentiment scores, and embeddings —
# in a single, ready-to-use tibble.

saveRDS(df_sentences, "uk_manifesto_data.rds")

