# ==============================================================================
# Day 5 Applied Lab: Semantic Spaces in German Politics
# Focus: Embeddings, Sentiment, Clustering, and Ideological Scaling
# Course: DOPP 5688 — Text as Data (Spring 2026)
# ==============================================================================

# UPDATED SCRIPT WITH ADDITIONAL EXPLANATIONS!

# --- METHODOLOGICAL OVERVIEW --------------------------------------------------
# Today we move from the Bag-of-Words (BoW) world into the world of transformers.
# In Day 4, we represented text as word frequency counts. The fundamental 
# problem with that approach is that it is completely blind to meaning:
#   - "The bank was steep" and "I went to the bank" look identical.
#   - "I love this" and "I adore this" look completely different.
#
# We use transformer-based sentence embeddings. Instead of counting words,
# we encode each sentence as a dense numeric vector (768 numbers) where the 
# *geometry* of the space captures *meaning*. Sentences that mean similar things
# end up close together. Sentences that mean different things end up far apart.
#
# This allows us to do things that are completely impossible with BoW:
#   1. Cluster sentences by topic without a vocabulary
#   2. Scale ideology geometrically without anchor texts
#   3. Measure sentiment continuously without a dictionary
#   4. Analyse German text using models trained on multiple languages (XLM-R)
#
# ALL of this analysis runs on a single pre-processed data file. The transformer
# inference (translation, sentiment, embeddings) was done before class on a GPU.
# What we have in de_manifesto_data.rds is a plain tibble with one row per
# quasi-sentence and columns for German text, English translation, metadata,
# sentiment scores, and 768 embedding dimensions — everything we need.
# ------------------------------------------------------------------------------


# 1. Setup and Package Loading -------------------------------------------------

library(tidyverse)   # data wrangling and ggplot2 for visualization
library(lsa)         # Linear Algebra tools for cosine similarity
library(plotly)      # interactive plots (hover to read text in Parts 5-6)
library(tidytext)    # unnest_tokens() for keyword extraction in Part 3
library(uwot)        # UMAP dimensionality reduction in Part 6

setwd("~/Dropbox/University/CEU/Teaching/DOPP5688 - NLP/")


# 2. Data Loading and Inspection -----------------------------------------------

# readRDS() loads R's native binary format. Unlike CSV, it preserves column 
# types exactly and loads much faster for large objects like our embedding matrix.
df <- readRDS("de_manifesto_data.rds")

# Always sanity-check your data before analysis. We want to confirm:
# (a) Both parties are present
# (b) Sentence counts are plausible (~1000-2000 per party)
# (c) All expected columns are there, including text_de, text_en, 
#     positivity_score, and emb_1 through emb_768
cat("Rows per party:\n")
print(count(df, party_name))
cat("\nColumn names:\n")
print(names(df))

# --- THE EMBEDDING MATRIX ---
# The embedding dimensions are stored as regular columns (emb_1, emb_2, ..., 
# emb_768) in our data frame. For the geometric operations in Parts 3-5, we 
# need them as a numeric matrix where rows = sentences and columns = dimensions.
# starts_with() must live directly inside select() — it is a tidyselect helper
# that only works inside a selecting function.
m_emb <- as.matrix(select(df, starts_with("emb_")))

cat("\nEmbedding matrix dimensions:", nrow(m_emb), "sentences x", ncol(m_emb), "dimensions\n")

# Unlike the UK script, we have two text columns:
#   text_de: the original German quasi-sentences from the manifesto
#   text_en: the Helsinki-NLP machine translation to English
#
# We use text_de for embeddings (XLM-R handles German natively).
# We use text_en for sentiment (DistilBERT was fine-tuned on English).
# We use text_en for keyword inspection (English stopwords and tidytext tools).
# When reading sentences in the interactive plots, text_en is more accessible.


# ==============================================================================
# PART 2: SENTIMENT ANALYSIS
# ==============================================================================

# --- METHODOLOGICAL EXPLANATION: TRANSFORMER SENTIMENT VS. DICTIONARY SENTIMENT ---
# In Day 4 we used the LSD2015 dictionary: a handcrafted list of ~3,000 positive 
# and negative words. It is fast and transparent, but it has real weaknesses:
#   - It cannot handle context ("not bad" requires manual negation handling)
#   - It misses words not in the dictionary
#   - It treats all positive words equally regardless of intensity
#
# Today's sentiment scores come from DistilBERT fine-tuned on SST-2 (Stanford
# Sentiment Treebank). This is a transformer model trained on 67,000 human-
# labelled sentences. Key advantages over dictionaries:
#   - It reads the whole sentence before scoring, so context matters
#   - Scores are continuous probabilities, not integer word counts
#
# IMPORTANT FOR THE GERMAN CASE: DistilBERT was trained on English text.
# We therefore score sentiment on the *translated* English text (text_en),
# not the original German. This introduces a potential source of error —
# translation quality affects sentiment scores — but it is preferable to
# applying an English dictionary directly to German text.
#
# The positivity_score is in [0, 1]:
#   1.0 = maximally positive language
#   0.0 = maximally negative language
#   0.5 = genuine ambiguity


# 2a. Sentiment Distribution ---------------------------------------------------
# Before looking at arcs over time, let's compare the overall distribution of 
# sentiment between the two parties. A boxplot shows the full distribution
# (median, spread, outliers), not just a single average number.

ggplot(df, aes(x = reorder(party_name, positivity_score),
               y = positivity_score,
               fill = party_name)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("AfD" = "#009EE0", "Greens" = "#46962b")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title    = "Transformer Sentiment Distribution by Party",
       subtitle = "DistilBERT positivity scores on translated English text",
       x        = "Party (ordered by median positivity)",
       y        = "Positivity Score (0 = Negative, 1 = Positive)")

# INTERPRETATION GUIDE:
# - Which party has a higher median positivity? Is this surprising given that
#   the AfD ran a largely grievance-based campaign in 2021?
# - Which party has a wider spread (more variable emotional register)?
# - Compare to the Day 4 LSD2015 results. Do the two methods agree?


# 2b. Sentiment Density --------------------------------------------------------
# A density plot shows the full shape of the distribution — useful for seeing
# whether the distributions are bimodal (two peaks: very positive and very 
# negative) which is expected from a binary classifier like DistilBERT.

ggplot(df, aes(x = positivity_score, fill = party_name)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("AfD" = "#009EE0", "Greens" = "#46962b")) +
  theme_minimal() +
  labs(title    = "Transformer Sentiment Density by Party",
       subtitle = "DistilBERT is a binary classifier — scores cluster near 0 and 1",
       x        = "Positivity Score (0 = Negative, 1 = Positive)",
       y        = "Density")

# NOTE ON THE BIMODAL SHAPE:
# DistilBERT outputs POSITIVE or NEGATIVE with a confidence score. When it is
# very confident (which it usually is), scores cluster near 0 or near 1.
# This bimodal pattern is normal. The overlap between parties near 0.5 is
# where the model is genuinely uncertain — these are the most interesting sentences.


# 2c. Narrative Arcs -----------------------------------------------------------
# How does sentiment evolve over the course of each manifesto?
# sentence_index counts from 1 (start of manifesto) to N (end).

ggplot(df, aes(x = sentence_index, y = positivity_score, color = party_name)) +
  geom_point(alpha = 0.05) +
  # LOESS smoother: a flexible local regression curve that captures the 
  # emotional arc as the manifesto progresses.
  # span = 0.2: smaller = more responsive to local variation.
  # se = FALSE: removes the confidence ribbon for a cleaner plot.
  geom_smooth(method = "loess", se = FALSE, span = 0.2, linewidth = 1.2) +
  facet_wrap(~party_name, scales = "free_x") +
  scale_color_manual(values = c("AfD" = "#009EE0", "Greens" = "#46962b")) +
  theme_minimal() +
  labs(title    = "Narrative Arcs: German Federal Election 2021",
       subtitle = "Smoothed Sentiment (Transformer-based DistilBERT on English translation)",
       x        = "Manifesto Sequence (Start to Finish)",
       y        = "Positivity Score (0 = Negative, 1 = Positive)")

# INTERPRETATION GUIDE:
# - Does the AfD start with grievances and end with promises, or is it
#   consistently negative throughout?
# - Does the Greens manifesto have a different arc shape — more consistently 
#   positive, or with a notable dip in the middle (listing problems before 
#   proposing solutions)?
# - Compare these arcs to what you found with LSD2015 on the Irish budget data
#   in Day 4. Do the shapes follow a similar pattern?


# ==============================================================================
# PART 3: SEMANTIC TOPIC CLUSTERING (K-MEANS ON EMBEDDINGS)
# ==============================================================================

# --- METHODOLOGICAL EXPLANATION: EMBEDDING CLUSTERING VS. TOPIC MODELING ---
# In Day 4, STM and Seeded LDA found topics by looking at word co-occurrence 
# patterns in a Document-Feature Matrix. Today we find topics geometrically:
# sentences that mean similar things are close together in embedding space, 
# so we cluster by proximity rather than vocabulary overlap.
#
# Key differences:
# - BoW topic models need large documents (sentences are often too sparse)
# - Embedding clustering works perfectly at the sentence level
# - Embedding clusters are *semantic* — "Klimaschutz" and "erneuerbare Energien"
#   (even after translation) end up in the same cluster, even if they share no
#   words, because XLM-R understands their meaning is related
# - But clusters are harder to label: we must inspect keywords after the fact
#
# NOTE: We cluster on m_emb which was built from the original German text
# (XLM-R embeddings). We then inspect clusters using the English translations
# so we can use English stopword lists and tidytext tools.

set.seed(1904)
km_res           <- kmeans(m_emb, centers = 3)
df$topic_cluster <- as.factor(km_res$cluster)


# 3a. Keyword Inspection (TF-IDF) ----------------------------------------------
# We use TF-IDF rather than raw frequency to surface words that are 
# *distinctive* to each cluster, not just common across all of them.
# This is the same logic as FREX in STM from Day 4.
#
# Extended stopword list: generic political words that appear in every cluster
# and drown out the substantive signal.
manifesto_stopwords <- c(
  stop_words$word,
  "greens", "afd", "government", "ensure", "support",
  "people", "will", "also", "including", "policy", "policies",
  "federal", "germany", "german", "national", "public", "new", "make"
)

cluster_keywords <- df |>
  select(text_en, topic_cluster) |>
  unnest_tokens(word, text_en) |>
  filter(
    !word %in% manifesto_stopwords,
    !str_detect(word, "^[0-9]+$"),
    nchar(word) > 3
  ) |>
  count(topic_cluster, word, sort = TRUE) |>
  group_by(topic_cluster) |>
  bind_tf_idf(word, topic_cluster, n) |>
  slice_max(tf_idf, n = 10)

# Print keywords to console — read these carefully before proceeding
for (i in 1:3) {
  cat("\n--- TOP WORDS FOR CLUSTER", i, "(TF-IDF) ---\n")
  print(filter(cluster_keywords, topic_cluster == i) |> pull(word))
}

# --- EXERCISE: THE "HUMAN-IN-THE-LOOP" LABELING ---
# Based on the keywords above, give each cluster a substantive name.
# Likely candidates for this corpus:
#   - "Climate & Energy" (renewable, emissions, energy transition)
#   - "Security & Migration" (border, asylum, crime, identity)
#   - "Social Justice & Economy" (wages, housing, healthcare, inequality)
#
# Replace the placeholder labels below with your own after inspecting:
df <- df |>
  mutate(cluster_label = case_when(
    topic_cluster == 1 ~ "Cluster 1 (label me)",
    topic_cluster == 2 ~ "Cluster 2 (label me)",
    topic_cluster == 3 ~ "Cluster 3 (label me)"
  ))


# 3b. Keyword Bar Chart --------------------------------------------------------
# Visualise the TF-IDF keywords as a bar chart — one panel per cluster.
# This is the embedding equivalent of STM's labelTopics() from Day 4.

cluster_keywords |>
  left_join(distinct(select(df, topic_cluster, cluster_label)), by = "topic_cluster") |>
  ggplot(aes(x = reorder_within(word, tf_idf, cluster_label),
             y = tf_idf,
             fill = cluster_label)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~cluster_label, scales = "free_y") +
  theme_minimal() +
  labs(title    = "Top Keywords per Semantic Cluster (TF-IDF)",
       subtitle = "TF-IDF surfaces distinctive words, not just frequent ones",
       x = NULL, y = "TF-IDF Score")


# 3c. Sentence Gallery ---------------------------------------------------------
# Keywords alone don't always reveal what a cluster is about. Reading actual
# sentences is essential — this is the "human-in-the-loop" step that BoW 
# models also require (recall inspecting STM output in Day 4).
# We sample 5 sentences per cluster from the English translations.

for (i in 1:3) {
  cat("\n\n=== CLUSTER", i, "— SENTENCE SAMPLE ===\n")
  sample_sents <- df |>
    filter(topic_cluster == i) |>
    slice_sample(n = 5) |>
    pull(text_en)
  cat(paste0(seq_along(sample_sents), ". ", sample_sents, "\n"), sep = "\n")
}


# 3d. Party Focus by Cluster ---------------------------------------------------
# Which party 'owns' which policy area? We calculate what proportion of each 
# party's manifesto falls into each semantic cluster.
# Using proportions rather than raw counts is essential — the two manifestos 
# have different lengths, so raw counts would be misleading.

df |>
  count(party_name, cluster_label) |>
  group_by(party_name) |>
  mutate(proportion = n / sum(n)) |>
  ggplot(aes(x = cluster_label, y = proportion, fill = party_name)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("AfD" = "#009EE0", "Greens" = "#46962b")) +
  theme_minimal() +
  labs(title    = "Rhetorical Focus: Who Owns Which Semantic Topic?",
       subtitle = "Proportion of each party's manifesto in each embedding cluster",
       x        = "Semantic Cluster",
       y        = "Proportion of Manifesto")

# INTERPRETATION GUIDE:
# - If the AfD is strongly over-represented in a security/migration cluster,
#   that confirms the known salience of that issue for the party.
# - If the Greens dominate a climate cluster, that is expected — but how large 
#   is the AfD presence in the same cluster? Do they engage with climate too, 
#   but with different framing?
# - Compare to the STM prevalence estimates from Day 4. Do the two approaches 
#   agree on which topics are party-specific vs. shared?


# ==============================================================================
# PART 4: IDEOLOGICAL SCALING (CENTROID PROJECTION)
# ==============================================================================

# --- METHODOLOGICAL EXPLANATION: EMBEDDING SCALING VS. WORDFISH ---
# In Day 4, Wordfish found ideology by modelling word frequency patterns.
# Today we take a completely different, geometric approach.
#
# THE KEY IDEA:
# We define each party's "ideal type" as the *centroid* (average position) of 
# all their sentences in 768-dimensional embedding space.
#
# We then measure every sentence's *cosine similarity* to each centroid.
# The ideology score is:
#   ideology = cosine_similarity_to_AfD - cosine_similarity_to_Greens
#
# Positive score = sentence is semantically closer to AfD language.
# Negative score = sentence is semantically closer to Green language.
# Near zero = consensus language — the parties speak alike here.
#
# NOTE ON THE SCALE:
# The absolute values may be very small (e.g., ±0.002). This is not a bug.
# Cosine similarities between centroids are close to each other when both 
# parties discuss largely the same policy terrain. The method is sensitive 
# enough to detect real but subtle differences. A narrow scale means the two
# parties' manifestos are semantically closer than their rhetoric suggests.

# Rescale so the midpoint between the two party means = 0
mean_afd    <- mean(df$ideology[df$party_name == "AfD"])
mean_greens <- mean(df$ideology[df$party_name == "Greens"])
midpoint    <- (mean_afd + mean_greens) / 2

df$ideology_centered <- df$ideology - midpoint

ggplot(df, aes(x = ideology_centered, y = party_name, color = party_name)) +
  geom_jitter(height = 0.2, alpha = 0.3, size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("AfD" = "#009EE0", "Greens" = "#46962b")) +
  theme_minimal() +
  labs(title    = "Semantic Ideological Scaling: Germany 2021",
       subtitle = "Projection onto the AfD–Greens Centroid Axis (centred at empirical midpoint)",
       x        = "<-- More Green-like  |  More AfD-like -->", y = NULL)
# INTERPRETATION GUIDE:
# - Well-separated clouds = the two parties use genuinely distinct semantic 
#   vocabularies. Overlap near 0 = consensus or crossover rhetoric.
# - HOW DOES THIS COMPARE TO WORDFISH? Both produce a single ideological 
#   dimension. Do they agree on which party is more ideologically consistent?


# 4b. Partisan Word Signals (mirrors Wordfish "Eiffel Tower" plot) -------------
# For each word in the corpus, we calculate its average ideology score based
# on the sentences it appears in. This is the embedding-space equivalent of 
# the Wordfish beta coefficients from Day 4.

manifesto_stopwords_words <- c(
  stop_words$word,
  "greens", "afd", "government", "ensure", "support", "people",
  "will", "also", "including", "federal", "germany", "german",
  "national", "public", "new", "make", "policy", "policies"
)

word_ideology <- df |>
  select(text_en, ideology) |>
  unnest_tokens(word, text_en) |>
  filter(
    !word %in% manifesto_stopwords_words,
    !str_detect(word, "^[0-9]+$"),
    nchar(word) > 3
  ) |>
  group_by(word) |>
  summarise(mean_ideology = mean(ideology), n = n()) |>
  filter(n >= 20) |>
  arrange(mean_ideology)

word_ideology |>
  slice(c(1:15, (n() - 14):n())) |>
  mutate(
    direction = if_else(mean_ideology > 0, "AfD-leaning", "Greens-leaning"),
    word      = reorder(word, mean_ideology)
  ) |>
  ggplot(aes(x = mean_ideology, y = word, fill = direction)) +
  geom_col() +
  scale_fill_manual(values = c("AfD-leaning"    = "#009EE0",
                               "Greens-leaning" = "#46962b")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title    = "Partisan Word Signals: The Semantic Eiffel Tower",
       subtitle = "Average ideology score of sentences containing each word (min. 20 appearances)",
       x        = "<-- Greens  |  AfD -->",
       y        = NULL,
       fill     = NULL)


# ==============================================================================
# PART 5: INTERACTIVE IDEOLOGY VS. SENTIMENT EXPLORATION
# ==============================================================================
# The most valuable part of embedding-based analysis is being able to read the 
# actual sentences that end up in unexpected positions.
# We use text_en (the English translation) for the tooltip so sentences are 
# readable without knowing German.
#
# The most interesting sentences to investigate:
#   - AfD sentences far LEFT of 0 (where the AfD sounds like the Greens)
#   - Green sentences far RIGHT of 0 (where the Greens sound like the AfD)
#   - High positivity + high AfD ideology: aspirational AfD promises
#   - Low positivity + low ideology (Green): Greens' critique of the status quo

p <- df |>
  mutate(text_wrapped = str_wrap(text_en, 40)) |>
  ggplot(aes(
    x     = ideology,
    y     = positivity_score,
    color = party_name,
    text  = text_wrapped
  )) +
  geom_point(alpha = 0.4) +
  scale_color_manual(values = c("AfD" = "#009EE0", "Greens" = "#46962b")) +
  theme_minimal() +
  labs(
    title    = "Ideology vs. Sentiment: Germany 2021",
    subtitle = "Hover over any dot to read the sentence (English translation)",
    x        = "<-- More Green-like  |  More AfD-like -->",
    y        = "Positivity Score"
  )

ggplotly(p, tooltip = "text")


# ==============================================================================
# PART 6: INTERACTIVE SEMANTIC MAP (UMAP)
# ==============================================================================

# --- WHAT IS UMAP AND WHY DO WE NEED IT? -------------------------------------
# Our embeddings live in 768 dimensions. Every analysis so far has either:
#   - Collapsed 768D to 1D (the ideology scale in Part 4), or
#   - Kept all 768D for clustering (K-Means in Part 3)
#
# UMAP (Uniform Manifold Approximation and Projection) is a dimensionality 
# reduction technique that squashes 768 dimensions down to 2 while trying to 
# preserve the *neighborhood structure* of the data. Points that were close 
# in 768D should still be close in 2D.
#
# This is different from PCA (which you may know): PCA finds the directions of
# maximum linear variance. UMAP finds a non-linear 2D layout that preserves 
# local clusters. For high-dimensional semantic data, UMAP almost always 
# produces more interpretable maps than PCA.
#
# KEY PARAMETERS:
# n_neighbors = 15: How many nearby sentences each point considers when 
#   learning the local structure. Larger values = more global structure 
#   preserved; smaller values = finer local clusters. 15 is a standard default.
# min_dist = 0.1: How tightly points are allowed to cluster in 2D.
#   Smaller = tighter, more separated clusters. Larger = more uniform spread.
#
# IMPORTANT CAVEAT: UMAP is stochastic and the 2D coordinates have no 
# interpretable meaning on their own (the x-axis is not "left-right ideology").
# The *distances between points* are what matter, not the absolute position.

set.seed(1904)
umap_coords <- umap(m_emb, n_neighbors = 15, min_dist = 0.1)

umap_df <- tibble(
  UMAP1        = umap_coords[, 1],
  UMAP2        = umap_coords[, 2],
  party_name   = df$party_name,
  text_wrapped = str_wrap(df$text_en, 40),
  cluster      = df$cluster_label,
  ideology     = df$ideology
)

# Interactive UMAP coloured by party
p_umap <- ggplot(umap_df, aes(
  x     = UMAP1,
  y     = UMAP2,
  color = party_name,
  text  = text_wrapped
)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_manual(values = c("AfD" = "#009EE0", "Greens" = "#46962b")) +
  theme_void() +
  labs(title = "Semantic Map of the 2021 German Manifestos",
       subtitle = "Hover to read individual sentences (English translation)")

ggplotly(p_umap, tooltip = "text")

# THINGS TO LOOK FOR:
# - Are the two parties clearly separated in 2D space, or do their sentence 
#   clouds overlap substantially?
# - Mixed-colour regions in the map correspond to the "consensus" language 
#   we identified near ideology = 0 in Part 4. What topics appear there?
# - Find a Green dot deep inside the AfD cluster (or vice versa). 
#   Is the model wrong, or is the party genuinely using the other side's 
#   semantic vocabulary? This "crossover rhetoric" is often politically 
#   significant — it may reflect strategic framing or issue ownership competition.
# - How do the UMAP clusters compare to the K-Means clusters from Part 3?
#   They should roughly align, but UMAP may reveal sub-structure within clusters
#   that K-Means missed.