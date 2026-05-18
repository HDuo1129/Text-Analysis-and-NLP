# ==============================================================================
# Day 5 Applied Lab: Semantic Spaces in UK Politics
# Focus: Embeddings, Sentiment, Clustering, and Ideological Scaling
# Course: DOPP 5688 — Text as Data (Spring 2026)
# ==============================================================================


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
#
# ALL of this analysis runs on a single pre-processed data file. The transformer
# inference (the slow, GPU-intensive part) was done in the prep script before 
# class. What we have in uk_manifesto_data.rds is a plain tibble with one row 
# per quasi-sentence and columns for text, metadata, sentiment scores, and 768 
# embedding dimensions — everything we need, ready to go.
# ------------------------------------------------------------------------------


# 1. Setup and Package Loading -------------------------------------------------

library(tidyverse)   # data wrangling and ggplot2 for visualization
library(lsa)         # Linear Algebra tools for cosine similarity
library(plotly)      # interactive plots (hover to read text in Part 5)
library(tidytext)    # unnest_tokens() for keyword extraction in Part 3

setwd("~/Desktop/NLP/")


# 2. Data Loading and Inspection -----------------------------------------------

# readRDS() loads R's native binary format. Unlike CSV, it preserves column 
# types exactly (numeric stays numeric, factor stays factor, etc.) and loads
# much faster for large objects like our 768-column embedding matrix.
df <- readRDS("uk_manifesto_data.rds")

# Always sanity-check your data before analysis. We want to confirm:
# (a) Both parties are present
# (b) Sentence counts are plausible (~1000-2000 per party)
# (c) All expected columns are there, including emb_1 through emb_768
cat("Rows per party:\n")
print(count(df, party_name))
cat("\nColumn names:\n")
print(names(df))

# --- THE EMBEDDING MATRIX ---
# The embedding dimensions are stored as regular columns (emb_1, emb_2, ..., 
# emb_769) in our data frame. For the geometric operations in Parts 3-5, we 
# need them as a numeric matrix where rows = sentences and columns = dimensions.
# starts_with() must live directly inside select() — it is a tidyselect helper
# that only works inside a selecting function.
m_emb <- as.matrix(select(df, starts_with("emb_")))

cat("\nEmbedding matrix dimensions:", nrow(m_emb), "sentences x", ncol(m_emb), "dimensions\n")


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
#   - "We will not fail our NHS" scores very differently from "We will fail our NHS"
#   - Scores are continuous probabilities, not integer word counts
#
# The score we stored is a *positivity score* in [0, 1]:
#   1.0 = the model is maximally confident this is positive language
#   0.0 = the model is maximally confident this is negative language
#   0.5 = genuine ambiguity
#
# NOTE ON THE BIMODAL DISTRIBUTION YOU CAN SEE IN THE RAW DOTS:
# DistilBERT is a binary classifier — it outputs POSITIVE or NEGATIVE with a 
# confidence score. When the model is very sure (which it usually is), scores 
# cluster near 0 or near 1. This is normal and expected. The LOESS trend line 
# is what matters — it smooths over this and shows us the emotional arc.

# 2a. Sentiment Distribution (mirrors Day 4 boxplot) --------------------------
# Before looking at arcs over time, let's compare the overall distribution of 
# sentiment between the two parties — the same question we asked with LSD2015.
# A boxplot shows us the full distribution (median, spread, outliers), not just 
# a single average number.

ggplot(df, aes(x = reorder(party_name, positivity_score), 
               y = positivity_score, 
               fill = party_name)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("Conservative" = "#0087DC", "Labour" = "#E4003B")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title    = "Transformer Sentiment Distribution by Party",
       subtitle = "DistilBERT positivity scores — compare to Day 4 LSD2015 boxplot",
       x        = "Party (ordered by median positivity)",
       y        = "Positivity Score (0 = Negative, 1 = Positive)")

# INTERPRETATION?

# 2b. Narrative Arcs -----------------------------------------------------------
# Now let's look at how sentiment evolves over the course of each manifesto.
# sentence_index was created in the prep script using row_number() within each 
# party group — it tells us the position of each sentence from start to finish.

ggplot(df, aes(x = sentence_index, y = positivity_score, color = party_name)) +
  # Raw sentence scores as very faint dots — background texture, not the focus
  geom_point(alpha = 0.05) +
  
  # THE MAIN INSIGHT COMES FROM THIS LINE: geom_smooth with method = "loess"
  # fits a local polynomial regression — a flexible curve that captures the 
  # emotional arc of the manifesto as it progresses from start to finish.
  # span = 0.2 controls smoothness: smaller = more wiggly (captures local 
  # variation); larger = smoother (shows overall trend).
  # se = FALSE removes the confidence ribbon so the plot stays clean.
  geom_smooth(method = "loess", se = FALSE, span = 0.2, linewidth = 1.2) +
  
  # facet_wrap gives each party its own panel. scales = "free_x" lets the 
  # x-axis scale independently — the two manifestos are different lengths.
  facet_wrap(~party_name, scales = "free_x") +
  scale_color_manual(values = c("Conservative" = "#0087DC", "Labour" = "#E4003B")) +
  theme_minimal() +
  labs(title    = "Narrative Arcs: UK General Election 2024",
       subtitle = "Smoothed Sentiment (Transformer-based DistilBERT)",
       x        = "Manifesto Sequence (Start to Finish)",
       y        = "Positivity Score (0 = Negative, 1 = Positive)")

# INTERPRETATION?

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
# - Embedding clusters are *semantic* — "cut public spending" and "reduce the 
#   deficit" end up in the same cluster, even though they share no words
# - But clusters are harder to label: we must inspect keywords after the fact
#
# K-MEANS ALGORITHM:
# kmeans() partitions sentences into K groups such that each sentence belongs 
# to the cluster whose center (centroid) it is closest to. It iterates until 
# the assignments stabilize. We set K = 3 for classroom clarity — in research 
# you would test multiple values of K and use fit diagnostics (like the elbow 
# method on within-cluster sum of squares) to choose.
#
# set.seed() makes the result reproducible. K-means starts with random 
# centroids, so without a seed every run gives different cluster numbers.
# Schalke got promoted!

set.seed(1904)
km_res           <- kmeans(m_emb, centers = 3)
df$topic_cluster <- as.factor(km_res$cluster)

# 3a. Keyword Inspection -------------------------------------------------------
# K-means gives us numbered clusters (1, 2, 3) but no names. To understand 
# what each cluster is *about*, we look at the most frequent non-stopword 
# words in each cluster. This mirrors the labelTopics() output from STM.

# Extended stopwords specific to this corpus
manifesto_stopwords <- c(
  stop_words$word,
  "labour", "conservative", "government", "ensure", "support",
  "people", "uk", "will", "also", "including", "plan", "deliver",
  "national", "country", "public", "new", "work", "make"
)

# Re-extract keywords with the extended list
cluster_keywords <- df |>
  select(text, topic_cluster) |>
  unnest_tokens(word, text) |>
  filter(
    !word %in% manifesto_stopwords,
    !str_detect(word, "^[0-9]+$"),
    nchar(word) > 3
  ) |>
  count(topic_cluster, word, sort = TRUE) |>
  group_by(topic_cluster) |>
  # Final step by me:
  # Switch from raw frequency to TF-IDF so we surface words that are 
  # distinctive to a cluster, not just common across all of them
  bind_tf_idf(word, topic_cluster, n) |>
  slice_max(tf_idf, n = 10)

# Print to console first so you can name the clusters before plotting
for (i in 1:3) {
  cat("\n--- TOP WORDS FOR CLUSTER", i, "---\n")
  print(filter(cluster_keywords, topic_cluster == i) |> pull(word))
}

# 3b. Keyword Bar Chart (mirrors STM labelTopics visualisation) ----------------
# Rather than just printing keywords to the console, we visualise them as a 
# bar chart — one panel per cluster. This is the embedding equivalent of the 
# top-word plots from Day 4's topic models.
# After running the loop above, replace the cluster labels in the mutate() call
# with your own substantive names based on what you see in the keywords.

cluster_keywords |>
  # REPLACE THESE LABELS with your own after inspecting the keywords above!
  mutate(cluster_label = case_when(
    topic_cluster == 1 ~ "Cluster 1 (label me)",
    topic_cluster == 2 ~ "Cluster 2 (label me)",
    topic_cluster == 3 ~ "Cluster 3 (label me)"
  )) |>
  ggplot(aes(x = reorder_within(word, n, cluster_label), y = n, fill = cluster_label)) +
  geom_col(show.legend = FALSE) +
  # reorder_within + scale_x_reordered: tidytext trick to sort bars 
  # independently within each facet panel
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~cluster_label, scales = "free_y") +
  theme_minimal() +
  labs(title = "Top Keywords per Semantic Cluster",
       subtitle = "Replace cluster labels above after inspecting keywords",
       x = NULL, y = "Word Frequency")

# 3c. Party Focus by Cluster ---------------------------------------------------
# Which party 'owns' which policy area? We calculate what proportion of each 
# party's manifesto falls into each semantic cluster.
# Using proportions rather than raw counts is essential — the two manifestos 
# have different lengths, so raw counts would be misleading.
#
# NOTE: If the bars look nearly equal across all clusters (as you may have seen),
# this is a substantively interesting finding — it means both parties engage 
# with the same broad semantic themes but differ in HOW they discuss them 
# (which is what sentiment and ideology scaling will show us). It does NOT mean
# the clustering failed.

df |>
  count(party_name, topic_cluster) |>
  group_by(party_name) |>
  mutate(proportion = n / sum(n)) |>
  ggplot(aes(x = topic_cluster, y = proportion, fill = party_name)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Conservative" = "#0087DC", "Labour" = "#E4003B")) +
  theme_minimal() +
  labs(title    = "Policy Focus: Who Owns Which Semantic Topic?",
       subtitle = "Proportion of each party's manifesto in each embedding cluster",
       x        = "Semantic Cluster (label after inspecting keywords above)",
       y        = "Proportion of Manifesto")


# ==============================================================================
# PART 4: IDEOLOGICAL SCALING (CENTROID PROJECTION)
# ==============================================================================

# --- METHODOLOGICAL EXPLANATION: EMBEDDING SCALING VS. WORDFISH ---
# In Day 4, Wordfish found ideology by modelling word frequency patterns: words 
# used disproportionately by one side of politics get a high beta score.
# Today we take a completely different, geometric approach.
#
# THE KEY IDEA: 
# We define each party's "ideal type" as the *centroid* (average position) of 
# all their sentences in 768-dimensional embedding space. Think of it as the 
# centre of gravity of their semantic cloud.
#
# We then measure every sentence's *cosine similarity* to each centroid.
# Cosine similarity measures the angle between two vectors:
#   - Score =  1.0: the sentence points in exactly the same direction as the 
#                   centroid (highly representative of that party's language)
#   - Score =  0.0: the sentence is at a 90-degree angle (unrelated)
#   - Score = -1.0: the sentence points in the opposite direction
#
# The ideology score for each sentence is:
#   ideology = cosine_similarity_to_Labour - cosine_similarity_to_Conservative
#
# A positive score means the sentence is semantically closer to Labour.
# A negative score means it is closer to Conservative language.
#
# NOTE ON THE NARROW X-AXIS SCALE (±0.002):
# You may notice the ideology scores are very small numbers. This is because 
# cosine similarities between the two centroids are very close to each other —
# both manifestos cover largely similar policy terrain (NHS, economy, housing).
# The *differences* are real and meaningful, but they are subtle. This is itself
# a finding: Labour and Conservative manifestos in 2024 occupy very similar 
# semantic space, and the scaling method is sensitive enough to detect the 
# small but consistent differences that do exist.

# Step 1: Calculate the centroid (mean vector) for each party
c_con <- colMeans(m_emb[df$party_name == "Conservative", ])
c_lab <- colMeans(m_emb[df$party_name == "Labour", ])

# Step 2: Compute cosine similarity of every sentence to each centroid.
# cos(θ) = (A · B) / (||A|| * ||B||)
sim_lab <- (m_emb %*% c_lab) / (sqrt(rowSums(m_emb^2)) * sqrt(sum(c_lab^2)))
sim_con <- (m_emb %*% c_con) / (sqrt(rowSums(m_emb^2)) * sqrt(sum(c_con^2)))
df$ideology <- as.numeric(sim_lab - sim_con)

# 4a. Ideological Spectrum Plot ------------------------------------------------
ggplot(df, aes(x = ideology, y = party_name, color = party_name)) +
  geom_jitter(height = 0.2, alpha = 0.3, size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Conservative" = "#0087DC", "Labour" = "#E4003B")) +
  theme_minimal() +
  labs(title    = "Semantic Ideological Scaling: UK 2024",
       subtitle = "Projection onto the Labour–Conservative Centroid Axis",
       x        = "<-- More Conservative  |  More Labour -->", y = NULL)

# INTERPRETATION GUIDE:
# - Well-separated clouds = the two parties use genuinely distinct semantic 
#   vocabularies. Overlap near 0 = consensus or crossover rhetoric.
# - Compare the *width* of each distribution. A wide spread means the party 
#   covers a broader semantic range.
# - HOW DOES THIS COMPARE TO WORDFISH? Both methods produce a single ideological
#   dimension, but Wordfish uses word frequencies while we use semantic geometry.
#   Do they agree on which party is more ideologically consistent (narrower)?


# 4b. Word-Level Partisan Signal (mirrors Wordfish "Eiffel Tower" plot) --------
# In Day 4, the Wordfish Eiffel Tower plot showed us WHICH WORDS pulled parties
# apart. We can produce an equivalent here: for each word in the corpus, we 
# calculate its average ideology score (based on the sentences it appears in).
# Words with high average ideology are semantically "Labour words";
# words with low average ideology are semantically "Conservative words".
# This is the embedding-space equivalent of the Wordfish beta coefficients.

word_ideology <- df |>
  select(text, ideology) |>
  unnest_tokens(word, text) |>
  filter(
    !word %in% stop_words$word,
    !str_detect(word, "^[0-9]+$"),
    nchar(word) > 3                      # drop very short words
  ) |>
  group_by(word) |>
  summarise(
    mean_ideology = mean(ideology),
    n             = n()
  ) |>
  # Only keep words that appear enough times to be reliable
  filter(n >= 20) |>
  arrange(mean_ideology)

# Plot the most partisan words at each end of the spectrum
# slice() takes the 15 most Conservative and 15 most Labour words
word_ideology |>
  slice(c(1:15, (n() - 14):n())) |>
  mutate(
    direction = if_else(mean_ideology > 0, "Labour-leaning", "Conservative-leaning"),
    word      = reorder(word, mean_ideology)
  ) |>
  ggplot(aes(x = mean_ideology, y = word, fill = direction)) +
  geom_col() +
  scale_fill_manual(values = c("Conservative-leaning" = "#0087DC", 
                               "Labour-leaning"       = "#E4003B")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title    = "Partisan Word Signals: The Semantic Eiffel Tower",
       subtitle = "Average ideology score of sentences containing each word (min. 20 appearances)",
       x        = "<-- Conservative  |  Labour -->",
       y        = NULL,
       fill     = NULL)

# INTERPRETATION GUIDE:
# - Compare these words to the Wordfish beta plot from Day 4. Do the same words 
#   appear as partisan signals in both methods?
# - Unlike Wordfish betas (which measure frequency asymmetry), these scores 
#   measure *semantic* association — a word can be a partisan signal here even 
#   if both parties use it, if they use it in very different semantic contexts.


# ==============================================================================
# PART 5: INTERACTIVE SEMANTIC EXPLORATION
# ==============================================================================

# --- WHY INTERACTIVE? ---
# The most valuable part of embedding-based analysis is being able to read the 
# actual sentences that end up in unexpected positions. 
#
# The most interesting sentences to investigate are:
#   - Conservative sentences far LEFT of 0: their most ideologically distinct language
#   - Conservative sentences to the RIGHT of 0: where they sound like Labour
#   - Labour sentences to the LEFT of 0: where they sound like Conservatives
#   - High positivity + high ideology: aspirational Labour promises
#   - Low positivity + low ideology: Conservative problem framing
#
# ggplotly() converts any ggplot into an interactive HTML widget.
# str_wrap(text, 40) breaks text into ~40-character lines for readable tooltips.

p <- df |>
  mutate(text_wrapped = str_wrap(text, 40)) |>
  ggplot(aes(
    x     = ideology,
    y     = positivity_score,
    color = party_name,
    text  = text_wrapped
  )) +
  geom_point(alpha = 0.4) +
  scale_color_manual(values = c("Conservative" = "#0087DC", "Labour" = "#E4003B")) +
  theme_minimal() +
  labs(
    title    = "Ideology vs. Sentiment: UK Manifestos 2024",
    subtitle = "Hover over any dot to read the sentence",
    x        = "<-- More Conservative  |  More Labour -->",
    y        = "Positivity Score"
  )

ggplotly(p, tooltip = "text")

# THINGS TO LOOK FOR:
# - TOP LEFT (Conservative, High Positivity): What does Tory optimism sound like?
# - BOTTOM RIGHT (Labour, Low Positivity): Labour's critique of the Conservative 
#   record — what issues does it focus on?
# - CENTRE (ideology near 0): "Consensus sentences" — where both parties agree.
#   What do they have in common? (Often: NHS, housing, economic stability)
# - OUTLIERS: Conservative sentences with ideology > 0 (sounds like Labour)?
#   Labour sentences with ideology < 0 (sounds like Conservatives)?
#   These crossover sentences are often the most politically interesting.