# ==============================================================================
# DOPP 5688 — Text as Data (Spring 2026)
# Assignment 3: Text Cleaning & Pre-processing with manifestoR
#
# Author: Huang Duo
# Course: Text Analysis and NLP for Public Policy
# Data: UK Labour Party manifesto, 2015 election, via Manifesto Project API
# ==============================================================================

# This script is designed to run from the NLP project folder:
# /Users/huangduo/Desktop/NLP
#
# It follows the homework template step by step, but keeps the final submission
# readable by using clearly named intermediate objects and short comments.


# 1. Setup and Package Loading -------------------------------------------------

library(tidyverse)
library(manifestoR)
library(quanteda)

# The API key file is already stored locally in the project data folder.
# The file should contain only the Manifesto Project API key.
mp_setapikey("Data/apikey.txt")

set.seed(1904)


# 2. Download the Raw Manifesto Data ------------------------------------------

# Identify the correct metadata row:
# country == 51 is the United Kingdom
# party == 51320 is the Labour Party
# edate == 2015-05-07 is the 2015 UK general election
mp_data <- mp_maindataset()

target_meta <- mp_data |>
  filter(country == 51,
         party == 51320,
         edate == as.Date("2015-05-07")) |>
  select(any_of(c("country", "countryname", "party", "partyname",
                  "edate", "date", "title")))

cat("\nTarget metadata row:\n")
print(target_meta)

# Check: this should print exactly 1 row.
stopifnot(nrow(target_meta) == 1)

# Download the actual manifesto text.
raw_docs <- mp_corpus(country == 51 &
                        party == 51320 &
                        edate == as.Date("2015-05-07"))


# 3. Extract Raw Text and Inspect It ------------------------------------------

# ManifestoCorpus objects are awkward to inspect directly, so I convert the
# corpus into a simple tibble with one row per quasi-sentence.
df_raw <- imap_dfr(raw_docs, function(doc, doc_id) {
  tibble(
    doc_id = doc_id,
    party_code = as.integer(str_extract(doc_id, "^[0-9]+")),
    election_date = str_extract(doc_id, "[0-9]{6}$"),
    text = content(doc)
  )
})

cat("\nNumber of raw quasi-sentences:", nrow(df_raw), "\n")
# Comment: the raw corpus is split into quasi-sentences rather than paragraphs.
# This is helpful for sentence-level analysis, but it also produces headings,
# page markers, and very short fragments that need cleaning.

df_raw |>
  mutate(nchar = nchar(text)) |>
  ggplot(aes(x = nchar)) +
  geom_histogram(bins = 80, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Raw Quasi-Sentence Lengths",
       x = "Number of Characters",
       y = "Count")
# Comment: most quasi-sentences are short to medium length, but there are both
# extremely short entries and a few long entries. Those extremes are likely to
# include headings, page numbers, or merged text blocks.

cat("\n20 shortest raw entries:\n")
df_raw |>
  mutate(nchar = nchar(text)) |>
  arrange(nchar) |>
  select(nchar, text) |>
  print(n = 20)
# Comment: the shortest entries include slogans, headings, and fragments. Some
# are real text, but one- or two-word entries are usually not useful for DFM
# construction because they add sparse noise.

cat("\n10 longest raw entries:\n")
df_raw |>
  mutate(nchar = nchar(text)) |>
  arrange(desc(nchar)) |>
  select(nchar, text) |>
  print(n = 10)
# Comment: the longest entries are more likely to be merged paragraphs or
# complex list items. Very long units violate the sentence-level assumption of
# this cleaning exercise.


# 4. Round 1 Cleaning: Length-Based Filtering ---------------------------------

# I keep quasi-sentences between 25 and 500 characters.
# Lower bound: removes page-like fragments and very short headings.
# Upper bound: removes unusually long merged blocks while keeping substantive
# manifesto sentences.
min_chars <- 25
max_chars <- 500

df_clean1 <- df_raw |>
  mutate(nchar = nchar(text)) |>
  filter(nchar >= min_chars,
         nchar <= max_chars) |>
  select(-nchar)

removed_round1 <- nrow(df_raw) - nrow(df_clean1)
cat("\nRows removed in round 1:", removed_round1, "\n")
cat("Proportion removed in round 1:",
    round(removed_round1 / nrow(df_raw), 3), "\n")
# Comment: this first pass removes only the most obvious length outliers. It is
# intentionally conservative, because content-based filters come next.

df_clean1 |>
  mutate(nchar = nchar(text)) |>
  ggplot(aes(x = nchar)) +
  geom_histogram(bins = 60, fill = "darkseagreen", color = "white") +
  theme_minimal() +
  labs(title = "Character Lengths After Round 1 Cleaning",
       x = "Number of Characters",
       y = "Count")
# Comment: the distribution is less extreme after length filtering, but some
# medium-length non-sentence material can still remain.


# 5. Round 2 Cleaning: Content-Based Filtering --------------------------------

cat("\nPotential non-sentence entries after round 1:\n")
df_clean1 |>
  filter(
    str_detect(text, "^[0-9\\s\\.]+$") |
      str_detect(text, "^[0-9]+\\.?\\s{0,3}\\w{0,10}$") |
      str_detect(text, "^[A-Z\\s\\-\\']+$")
  ) |>
  select(text) |>
  print(n = 30)
# Comment: these checks are meant to catch page numbers, short numbered section
# markers, and all-caps section headings. They are formatting artifacts rather
# than substantive manifesto statements.

df_clean2 <- df_clean1 |>
  filter(
    # Remove page numbers or pure numeric section markers.
    !str_detect(text, "^[0-9\\s\\.]+$"),
    # Remove short numeric labels such as "1." or "2 A".
    !str_detect(text, "^[0-9]+\\.?\\s{0,3}\\w{0,10}$"),
    # Remove all-caps section headings.
    !str_detect(text, "^[A-Z\\s\\-\\']+$"),
    # Remove repeated title/header boilerplate.
    !str_detect(str_to_lower(text), "^the labour party manifesto 2015$")
  )

removed_round2 <- nrow(df_clean1) - nrow(df_clean2)
cat("\nRows removed in round 2:", removed_round2, "\n")
# Comment: content filtering removes the entries that length rules cannot catch:
# especially headings and metadata-like text.


# 6. Round 3 Cleaning: String-Level Cleaning ----------------------------------

leading_or_trailing <- df_clean2 |>
  filter(text != str_squish(text)) |>
  nrow()

newline_count <- df_clean2 |>
  filter(str_detect(text, "\n")) |>
  nrow()

non_ascii_examples <- df_clean2 |>
  filter(str_detect(text, "[^\\x01-\\x7E]")) |>
  select(text) |>
  head(20)

number_profile <- df_clean2 |>
  summarise(
    embedded_numbers = sum(str_detect(text, "[A-Za-z][^\\n]*[0-9]|[0-9][^\\n]*[A-Za-z]")),
    number_only = sum(str_detect(text, "^[0-9\\s\\.]+$"))
  )

cat("\nEntries with whitespace issues:", leading_or_trailing, "\n")
cat("Entries with newline characters:", newline_count, "\n")
cat("\nExamples with non-ASCII characters:\n")
print(non_ascii_examples)
cat("\nNumber profile:\n")
print(number_profile)
# Comment: numbers embedded in real sentences should be kept because manifesto
# claims often mention years, budgets, and quantities. Pure numeric entries were
# already removed. Non-ASCII punctuation is mainly typographic formatting.

df_clean3 <- df_clean2 |>
  mutate(
    # Replace newlines before squishing so broken lines become normal spaces.
    text = str_replace_all(text, "\\n+", " "),
    # Replace common typographic punctuation with ASCII equivalents.
    text = str_replace_all(text, "[\u2018\u2019]", "'"),
    text = str_replace_all(text, "[\u201C\u201D]", "\""),
    text = str_replace_all(text, "[\u2013\u2014]", "-"),
    # Remove remaining non-ASCII symbols such as bullets.
    text = str_replace_all(text, "[^\\x01-\\x7E]", " "),
    # Standardize internal, leading, and trailing whitespace.
    text = str_squish(text)
  )

verification <- tibble(
  remaining_whitespace_issues = sum(df_clean3$text != str_squish(df_clean3$text)),
  remaining_newlines = sum(str_detect(df_clean3$text, "\n")),
  remaining_non_ascii = sum(str_detect(df_clean3$text, "[^\\x01-\\x7E]"))
)

cat("\nString-cleaning verification:\n")
print(verification)
# Comment: after round 3, the remaining rows should be easier to tokenize
# because whitespace and typographic punctuation have been standardized.


# 7. Assessment: What Did We Lose? --------------------------------------------

cleaning_summary <- tibble(
  stage = c("Original raw data",
            "After length filtering",
            "After content filtering",
            "After string cleaning"),
  rows = c(nrow(df_raw),
           nrow(df_clean1),
           nrow(df_clean2),
           nrow(df_clean3))
) |>
  mutate(
    removed_from_original = nrow(df_raw) - rows,
    retained_share = rows / nrow(df_raw)
  )

cat("\nCleaning summary:\n")
print(cleaning_summary)

cat("\n20 random cleaned sentences:\n")
df_clean3 |>
  slice_sample(n = 20) |>
  select(text) |>
  print(n = 20)
# Comment: the sampled rows read like real manifesto claims. In a larger final
# project I would do a second validation pass by manually coding a small random
# sample as "keep" or "remove" to estimate cleaning error.

df_clean3 |>
  mutate(nchar = nchar(text)) |>
  ggplot(aes(x = nchar)) +
  geom_histogram(bins = 60, fill = "mediumpurple", color = "white") +
  theme_minimal() +
  labs(title = "Final Character Length Distribution",
       subtitle = "UK Labour Party manifesto, 2015",
       x = "Number of Characters",
       y = "Count")
# Comment: compared with the raw distribution, the final version removes
# extreme length outliers and produces a more analysis-ready sentence corpus.


# 8. Final Pipeline ------------------------------------------------------------

# This is the clean, single-pipe version of the workflow. It starts from the raw
# ManifestoCorpus object and produces the final analysis-ready tibble.
df_final <- imap_dfr(raw_docs, function(doc, doc_id) {
  tibble(
    doc_id = doc_id,
    party_code = as.integer(str_extract(doc_id, "^[0-9]+")),
    election_date = str_extract(doc_id, "[0-9]{6}$"),
    text = content(doc)
  )
}) |>
  mutate(nchar = nchar(text)) |>
  filter(nchar >= min_chars,
         nchar <= max_chars) |>
  filter(
    !str_detect(text, "^[0-9\\s\\.]+$"),
    !str_detect(text, "^[0-9]+\\.?\\s{0,3}\\w{0,10}$"),
    !str_detect(text, "^[A-Z\\s\\-\\']+$"),
    !str_detect(str_to_lower(text), "^the labour party manifesto 2015$")
  ) |>
  mutate(
    text = str_replace_all(text, "\\n+", " "),
    text = str_replace_all(text, "[\u2018\u2019]", "'"),
    text = str_replace_all(text, "[\u201C\u201D]", "\""),
    text = str_replace_all(text, "[\u2013\u2014]", "-"),
    text = str_replace_all(text, "[^\\x01-\\x7E]", " "),
    text = str_squish(text)
  ) |>
  group_by(party_code) |>
  mutate(sentence_index = row_number()) |>
  ungroup() |>
  select(doc_id, party_code, election_date, sentence_index, text)

cat("\nRows in df_final:", nrow(df_final), "\n")
cat("\nSample of 10 final sentences:\n")
df_final |>
  slice_sample(n = 10) |>
  select(sentence_index, text) |>
  print(n = 10)

stopifnot(nrow(df_final) == nrow(df_clean3))


# 9. Bonus Visualization -------------------------------------------------------

# A simple top-word plot helps check whether the cleaned text contains
# substantive policy vocabulary rather than formatting artifacts.
top_words_plot <- df_final |>
  corpus(text_field = "text") |>
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_symbols = TRUE,
         remove_url = TRUE) |>
  tokens_tolower() |>
  tokens_remove(stopwords("en")) |>
  tokens_select(min_nchar = 3) |>
  dfm() |>
  topfeatures(n = 20) |>
  enframe(name = "word", value = "frequency") |>
  mutate(word = fct_reorder(word, frequency)) |>
  ggplot(aes(x = frequency, y = word)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Most Frequent Non-Stopwords in the Cleaned Manifesto",
       subtitle = "UK Labour Party manifesto, 2015",
       x = "Frequency",
       y = NULL)

print(top_words_plot)


# ==============================================================================
# Submission checklist:
# - Author filled in
# - Loads packages at top
# - Downloads data through manifestoR
# - Shows iterative cleaning and final single pipeline
# - Includes comments interpreting the checks
# ==============================================================================
