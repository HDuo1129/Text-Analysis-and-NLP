# ==============================================================================
# PREP SCRIPT: German 2021 Manifesto Processing
# Case Study: AfD vs. Greens (Federal Election 2021)
# PARTY CODES (MARPOR):
#   Greens (Bündnis 90/Die Grünen) -> party == 41113
#   AfD (Alternative für Deutschland) -> party == 41953
#   Country: Germany                 -> country == 41
# ==============================================================================

setwd("~/Dropbox/University/CEU/Teaching/DOPP5688 - NLP/")

library(reticulate)
try(use_condaenv("textrpp_condaenv", required = TRUE), silent = TRUE)
library(tidyverse)
library(manifestoR)
library(text)

textrpp_initialize()
mp_setapikey("~/Dropbox/Research/R/manifesto_apikey.txt")


# --- STEP 1: DATA INGESTION ---------------------------------------------------

mp_data <- mp_maindataset()

de_recent_meta <- mp_data |>
  filter(country == 41, party %in% c(41113, 41953)) |>
  arrange(desc(edate)) |>
  filter(edate == edate[1])         

manifesto_docs <- mp_corpus(de_recent_meta)
rm(mp_data, de_recent_meta)


# --- STEP 2: BUILD A FLAT DATA FRAME OF SENTENCES ----------------------------
# Identical approach to the UK script: extract raw character vectors from the
# ManifestoCorpus via content() and stack them into a plain tibble.
# manifestoR doc IDs follow the format "PARTYCODE_YEARMONTH" (e.g. "41113_202109").

df_sentences <- map_dfr(names(manifesto_docs), function(doc_id) {
  tibble(
    doc_id     = doc_id,
    party_code = as.integer(str_extract(doc_id, "^[0-9]+")),
    text_de    = content(manifesto_docs[[doc_id]])
  )
}) |>
  mutate(
    party_name = if_else(party_code == 41113, "Greens", "AfD")
  ) |>
  # Drop headers, page numbers, and noise.
  # Upper bound is slightly more generous than the UK script (1000 vs 800)
  # because German sentences tend to be longer.
  filter(nchar(text_de) > 20, nchar(text_de) < 1000) |>
  group_by(party_name) |>
  mutate(sentence_index = row_number()) |>
  ungroup()

cat("Sentences per party:\n")
print(count(df_sentences, party_name))


# --- STEP 3: TRANSLATION (DE -> EN) ------------------------------------------
# We need English text for the DistilBERT sentiment model (which was fine-tuned
# on English). We use the HuggingFace Helsinki-NLP/opus-mt-de-en model directly
# via reticulate — the same pattern we used for sentiment in the UK script —
# because textTranslate() in text v1.8.x has the same argument-matching 
# fragility as textClassify().
#
# MODEL CHOICE: Helsinki-NLP/opus-mt-de-en
# The OPUS-MT models are lightweight, fast MarianMT encoder-decoder translation
# models trained on millions of German-English sentence pairs. They are not as
# fluent as large generative models (GPT-4), but they are fast enough to run on
# a GPU in batch, and their quality is more than sufficient for sentiment scoring.
# For embeddings we will use the original German text with a multilingual model,
# so translation quality only needs to be good enough for sentiment.

cat("Starting batch translation (DE -> EN)...\n")

transformers <- reticulate::import("transformers")

translation_pipeline <- transformers$pipeline(
  "translation_de_to_en",
  model  = "Helsinki-NLP/opus-mt-de-en",
  device = 0L    # 0L = first GPU; use -1L for CPU
)

# Batch translation — same approach and reasoning as the UK sentiment batching.
# 64 sentences per batch is safe for a ~6GB GPU; reduce if you see OOM errors.
batch_size   <- 64L
trans_results <- list()

for (i in seq(1, nrow(df_sentences), by = batch_size)) {
  batch <- df_sentences$text_de[i:min(i + batch_size - 1, nrow(df_sentences))]
  trans_results[[length(trans_results) + 1]] <- translation_pipeline(batch)
}

# The pipeline returns a nested list: each element is a list with a
# 'translation_text' key. Flatten and extract.
df_sentences$text_en <- map_chr(
  unlist(trans_results, recursive = FALSE),
  ~ .x$translation_text
)

# Filter out any failed translations (empty strings or pipeline error strings)
n_before <- nrow(df_sentences)
df_sentences <- df_sentences |>
  filter(
    !is.na(text_en),
    nchar(text_en) > 5,
    !str_detect(text_en, regex("translation error", ignore_case = TRUE))
  )
cat("Removed", n_before - nrow(df_sentences), "failed translations.\n")
cat("Remaining sentences:", nrow(df_sentences), "\n")


# --- STEP 4: SENTIMENT ANALYSIS (DistilBERT on English translations) ----------
# We score sentiment on the *translated* English text, not the German original.
# This is the same DistilBERT SST-2 model used in the UK script.
# See UK prep script for full explanation of the model choice and score 
# construction logic.

cat("Scoring sentiment using DistilBERT...\n")

sentiment_pipeline <- transformers$pipeline(
  "sentiment-analysis",
  model  = "distilbert-base-uncased-finetuned-sst-2-english",
  device = 0L
)

sent_results <- list()

for (i in seq(1, nrow(df_sentences), by = batch_size)) {
  batch <- df_sentences$text_en[i:min(i + batch_size - 1, nrow(df_sentences))]
  sent_results[[length(sent_results) + 1]] <- sentiment_pipeline(batch)
}

sent_res <- map_dfr(unlist(sent_results, recursive = FALSE), function(r) {
  tibble(label = r$label, score = as.numeric(r$score))
}) |>
  mutate(
    positivity_score = if_else(
      str_detect(tolower(label), "pos"),
      score,
      1 - score
    )
  )

stopifnot(nrow(sent_res) == nrow(df_sentences))
df_sentences$positivity_score <- sent_res$positivity_score


# --- STEP 5: EMBEDDINGS (XLM-RoBERTa on original German text) ----------------
# We generate embeddings from the *original German* text, not the translation.
# This preserves the exact linguistic content and avoids compounding translation
# errors into the embedding space.
#
# MODEL CHOICE: xlm-roberta-base (XLM-R)
# XLM-R is a cross-lingual transformer pre-trained on 2.5TB of text in 100
# languages. Crucially, it was trained so that semantically equivalent sentences
# in different languages occupy the *same* coordinates in the 768-dimensional
# space. This means:
#   1. We can embed German text and compare across languages.
#   2. We can train a classifier on English data and apply it to German text
#      (cross-lingual transfer).
#
# This is why we use XLM-R for the German corpus instead of the English-only
# all-mpnet-base-v2 used in the UK script.
#
# layers = -2:
# Same reasoning as the UK script — the second-to-last layer reliably produces
# the richest contextual representations across transformer architectures and
# is stable across text package versions.

cat("Generating embeddings (xlm-roberta-base on original German text)...\n")

embeddings_obj <- textEmbed(
  texts = df_sentences$text_de,
  model = "xlm-roberta-base",
  layers = -2,
  device = "cuda",
  aggregation_from_tokens_to_texts = "mean"
)

# Pull embedding matrix and bind as columns.
# Check the structure first to use the correct accessor.
cat("Embedding object structure:", paste(names(embeddings_obj$texts), collapse = ", "), "\n")

# Use the correct accessor based on what textEmbed returns in your version:
# If names(embeddings_obj$texts) == "texts", use embeddings_obj$texts$texts
# If names(embeddings_obj$texts) == "text_de", use embeddings_obj$texts$text_de
# The line below uses "texts" — adjust if your version names it differently.
m_emb <- as.matrix(embeddings_obj$texts$texts)
colnames(m_emb) <- paste0("emb_", seq_len(ncol(m_emb)))
df_sentences <- bind_cols(df_sentences, as_tibble(m_emb))


# --- STEP 6: SAVE -------------------------------------------------------------

saveRDS(df_sentences, "de_manifesto_data.rds")
cat("\nSUCCESS: saved de_manifesto_data.rds\n")
cat("Columns:", paste(names(df_sentences), collapse = ", "), "\n")
cat("Total sentences saved:", nrow(df_sentences), "\n")