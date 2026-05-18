# ==============================================================================
# DOPP 5688 — Text as Data (Spring 2026)
# Assignment: Method Application
#
# Author:  [YOUR NAME]
# Method:  Wordfish (Day 4 — Unsupervised Spatial Scaling)
# Data:    Austrian party manifestos, 2017 / 2019 / 2024 (via manifestoR / MARPOR)
# Goal:    Estimate the ideological positions of the five main Austrian
#          parliamentary parties across three recent federal elections and
#          interpret the dimension that Wordfish recovers.
# ==============================================================================
# REPRODUCIBILITY NOTE
# ------------------------------------------------------------------------------
# This script is self-contained. To run it you need:
#   1. A free MARPOR / Manifesto Project API key. Register at
#      https://manifesto-project.wzb.eu, then go to your profile and generate
#      one.
#   2. Save the key as a plain text file called "manifesto_apikey.txt" in the
#      same folder as this script (or update the path in mp_setapikey() below).
#   3. The R packages listed in section 1. Uncomment install.packages() once if
#      they are not yet installed.
# ==============================================================================


# 1. Setup and Package Loading -------------------------------------------------

# install.packages(c("manifestoR", "quanteda", "quanteda.textmodels",
#                    "quanteda.textplots", "tidyverse", "stopwords"))

library(manifestoR)             # API access to MARPOR corpus
library(quanteda)               # corpus / tokens / DFM
library(quanteda.textmodels)    # textmodel_wordfish
library(quanteda.textplots)     # textplot_scale1d
library(tidyverse)              # data wrangling + ggplot2
library(stopwords)              # multilingual stopword lists (German here)

# Reproducibility: Wordfish itself is deterministic given the same data and
# anchors, but we set a seed for any incidental random operations.
set.seed(1904)


# 2. Data Ingestion ------------------------------------------------------------

# Set the API key. The file should contain ONLY the key string on one line.
mp_setapikey("manifesto_apikey.txt")

# Pull the MARPOR main dataset (small metadata table, downloads in seconds).
# We then filter to Austria + the three most recent federal elections we want.
#
# Why these three elections?
#   - 2017: First ÖVP–FPÖ "Kurz" coalition; FPÖ at ~26 %.
#   - 2019: Post-Ibiza realignment; first ÖVP–Greens coalition.
#   - 2024: Historic FPÖ first place (~29 %); fragmented five-party parliament.
# This span gives us enough variation to see whether positions drift over time
# without making the document-level corpus too small for Wordfish.

mp_main <- mp_maindataset()

at_meta <- mp_main |>
  filter(countryname == "Austria",
         edate >= as.Date("2017-01-01"),
         edate <= as.Date("2024-12-31")) |>
  # Keep only the five parties currently in the Nationalrat
  filter(partyname %in% c("Social Democratic Party of Austria",
                          "Austrian People's Party",
                          "Freedom Party of Austria",
                          "The Greens - The Green Alternative",
                          "The New Austria and Liberal Forum")) |>
  select(party, partyname, partyabbrev, edate, date) |>
  arrange(edate, partyabbrev)

cat("Manifestos to download:\n")
print(at_meta)

# Download the actual texts (quasi-sentences with MARPOR codes). The result is
# a tm-style Corpus. We immediately bypass the cache to make sure we get fresh
# data on first run; subsequent runs are cached locally by manifestoR.
at_corpus_marpor <- mp_corpus(countryname == "Austria" &
                                edate >= as.Date("2017-01-01") &
                                edate <= as.Date("2024-12-31") &
                                party %in% at_meta$party)


# 3. Build a quanteda corpus ---------------------------------------------------

# manifestoR returns one document per manifesto. Each document contains a list
# of quasi-sentences. We concatenate them to get one long text per manifesto,
# which is what Wordfish needs (it scales documents, not sentences).
#
# METHODOLOGICAL JUSTIFICATION (document level):
# Wordfish models word frequencies under the assumption that each document
# draws from a single ideological position. Sentence-level scaling is too
# sparse for reliable estimation — see the Day 4 lab notes. Manifestos are
# already at the right granularity: each is a coherent statement of a party's
# policy platform for one election.

doc_texts <- map_chr(at_corpus_marpor, ~ paste(content(.x), collapse = " "))

# Attach metadata. manifestoR document IDs are of the form "<party>_<date>".
doc_ids <- names(at_corpus_marpor)

doc_df <- tibble(doc_id = doc_ids, text = doc_texts) |>
  separate(doc_id, into = c("party_code", "date_code"),
           sep = "_", remove = FALSE) |>
  mutate(party_code = as.integer(party_code),
         year       = as.integer(substr(date_code, 1, 4))) |>
  left_join(distinct(at_meta, party, partyabbrev),
            by = c("party_code" = "party")) |>
  mutate(doc_label = paste(partyabbrev, year, sep = "_"))

at_corpus <- corpus(doc_df, docid_field = "doc_label", text_field = "text")

cat("\nCorpus summary:\n")
print(summary(at_corpus))


# 4. Preprocessing -------------------------------------------------------------

# METHODOLOGICAL JUSTIFICATION (no stemming):
# Following the Day 4 lab, we deliberately skip stemming. Stemming collapses
# ideologically loaded variants (e.g., "Migrant" vs. "Migration", "Asylant"
# vs. "Asyl") that often carry distinct partisan signals. For a scaling model
# whose entire job is to detect such signals, stemming destroys exactly the
# variation we want to measure.

# German stopwords from the snowball list, plus custom procedural / corpus-
# specific words. We strip generic political vocabulary that appears in every
# manifesto regardless of position ("Österreich", "Bundesregierung", etc.) so
# Wordfish picks up substantive partisan vocabulary rather than common nouns.
custom_stopwords <- c(
  "österreich", "österreicher", "österreicherinnen", "österreichische",
  "österreichischen", "österreichs",
  "bundesregierung", "regierung", "parlament", "nationalrat",
  "partei", "spö", "övp", "fpö", "grüne", "neos",
  "wir", "uns", "unsere", "unseren", "unserer", "unseres",
  "sowie", "dabei", "daher", "deshalb", "somit", "bzw",
  "muss", "müssen", "soll", "sollen", "wollen", "kann", "können",
  "jahr", "jahre", "jahren", "jahres",
  "mehr", "weniger", "viele", "vielen",
  "neue", "neuen", "neues", "neuer"
)

toks <- tokens(at_corpus,
               remove_punct   = TRUE,
               remove_numbers = TRUE,
               remove_symbols = TRUE) |>
  tokens_tolower() |>
  tokens_remove(stopwords("de", source = "snowball")) |>
  tokens_remove(custom_stopwords) |>
  tokens_select(min_nchar = 3)   # drop 1–2 character fragments

# Build the DFM and trim aggressively. With ~15 documents we need to be
# careful: very rare words add noise, very common words add no information.
# min_termfreq = 10 keeps words that appear at least 10 times in the whole
# corpus; min_docfreq = 3 requires the word to appear in at least 3 manifestos
# so it can actually discriminate between parties.
dfm_docs <- dfm(toks) |>
  dfm_trim(min_termfreq = 10, min_docfreq = 3)

cat("\nDFM dimensions:", ndoc(dfm_docs), "documents x",
    nfeat(dfm_docs), "features\n")


# 5. Wordfish — Main Model -----------------------------------------------------

# METHODOLOGICAL JUSTIFICATION (Wordfish over Wordscores):
# Wordfish is UNSUPERVISED: it discovers the dominant dimension of disagreement
# directly from word-frequency patterns, with no need to pre-label "left" and
# "right" anchor texts. This is the right choice for our research question
# because we want to interpret what dimension the texts themselves emphasize,
# not impose our prior assumption that "the conflict is left vs. right".
# As the Day 4 lab demonstrates, the Wordfish "Illusion" is exactly the
# interpretive challenge we want to engage with.
#
# ANCHOR DIRECTION:
# Wordfish needs `dir = c(low, high)` to fix the orientation of the recovered
# axis. The model is rotation-invariant: without an anchor it would arbitrarily
# flip the spectrum. We anchor FPÖ 2024 to the high end and Grüne 2024 to the
# low end — these are the two parties whose ideological distance is least
# disputed in the Austrian context. Note: this only fixes the sign, not the
# substance of the dimension.

# Find row indices for the anchors.
doc_labels <- docnames(dfm_docs)
idx_low    <- which(doc_labels == "GRÜNE_2024")
idx_high   <- which(doc_labels == "FPÖ_2024")

if (length(idx_low) != 1 || length(idx_high) != 1) {
  # Fallback if MARPOR uses slightly different abbreviations
  cat("Available doc labels:\n"); print(doc_labels)
  stop("Could not locate anchor documents — check abbreviations above.")
}

wf <- textmodel_wordfish(dfm_docs, dir = c(idx_low, idx_high))

cat("\nWordfish model fitted.\n")
print(summary(wf))


# 6. Visualisation -------------------------------------------------------------

# 6a. Document positions (theta) ----------------------------------------------
# This is the main result: each manifesto's position on the recovered axis.
# Grouped by party so we can see (i) cross-party separation and (ii) within-
# party drift across elections.

plot_positions <- textplot_scale1d(
  wf,
  groups = docvars(dfm_docs, "partyabbrev"),
  margin = "documents"
) +
  ggtitle("Wordfish Positions of Austrian Manifestos (2017, 2019, 2024)",
          subtitle = "Anchors: Grüne 2024 (low) — FPÖ 2024 (high)") +
  theme_minimal()

print(plot_positions)

# Pull the numeric estimates out so we can also plot them by year.
theta_df <- tibble(
  doc    = docnames(dfm_docs),
  theta  = wf$theta,
  se     = wf$se.theta,
  party  = docvars(dfm_docs, "partyabbrev"),
  year   = docvars(dfm_docs, "year")
)

cat("\nEstimated positions:\n")
print(arrange(theta_df, theta))

# 6b. Drift over time ---------------------------------------------------------
# A line plot is more revealing than the default textplot for showing whether
# parties move on the axis between elections.

party_colors <- c(SPÖ   = "#e3000f",
                  ÖVP   = "#63c3d0",
                  FPÖ   = "#0056a2",
                  GRÜNE = "#79b929",
                  NEOS  = "#e84188")

plot_drift <- ggplot(theta_df,
                     aes(x = year, y = theta,
                         colour = party, group = party)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = theta - 1.96 * se,
                    ymax = theta + 1.96 * se),
                width = 0.2, alpha = 0.6) +
  scale_x_continuous(breaks = c(2017, 2019, 2024)) +
  scale_color_manual(values = party_colors) +
  labs(title    = "Ideological Drift: Austrian Parties on the Wordfish Axis",
       subtitle = "95 % CIs from Wordfish standard errors",
       x = "Election year", y = "Wordfish position (θ)") +
  theme_minimal()

print(plot_drift)

# 6c. Eiffel-tower plot of word weights ---------------------------------------
# WHICH words define the recovered axis? Highlight a few substantively
# interesting terms (German lemmas) drawn from the policy areas where Austrian
# parties most disagree: migration, climate, economy, EU.

highlight_words <- c("asyl", "migration", "klima", "umwelt", "steuern",
                     "soziale", "sicherheit", "freiheit", "europäische",
                     "grenzen", "familie", "tradition")

plot_words <- textplot_scale1d(
  wf, margin = "features",
  highlighted = highlight_words
) +
  ggtitle("Wordfish Word Weights — What Defines the Axis?",
          subtitle = "Y = log frequency (ψ);  X = ideological weight (β)") +
  theme_minimal()

print(plot_words)


# 7. Interpretation Aid: Top discriminating words ------------------------------
# Print the 20 most "high-end" (FPÖ-leaning) and "low-end" (Grüne-leaning)
# words by beta. This is what we use to NAME the axis after the model has
# recovered it.

word_betas <- tibble(
  word = featnames(dfm_docs),
  beta = wf$beta,
  psi  = wf$psi
) |>
  # Drop very rare words from the top-N lists; they dominate by exclusivity
  # rather than by genuine partisan signal.
  filter(psi > median(psi))

cat("\n--- Top 20 words on the HIGH end (anchored toward FPÖ 2024) ---\n")
print(word_betas |> arrange(desc(beta)) |> slice_head(n = 20))

cat("\n--- Top 20 words on the LOW end (anchored toward Grüne 2024) ---\n")
print(word_betas |> arrange(beta) |> slice_head(n = 20))


# ==============================================================================
# 8. Findings and Interpretation (to be discussed in the comments below)
# ==============================================================================
#
# WHAT WORDFISH RECOVERS
# ----------------------
# Run the script and read the three plots together with the top-words list.
# Expected pattern (based on the substantive Austrian context, to be verified
# against actual output):
#   - FPÖ manifestos cluster on the "high" anchor end across all three years.
#   - Grüne and NEOS manifestos cluster on the "low" end.
#   - SPÖ sits left of centre; ÖVP sits right of centre but well short of FPÖ.
#   - Within-party drift is usually small but the FPÖ may show a 2017 -> 2024
#     shift as the party radicalised after Ibiza.
#
# IS THE AXIS "ECONOMIC LEFT-RIGHT"?
# ----------------------------------
# Almost certainly NOT in its pure form. In Austrian manifestos, the dimension
# of MAXIMUM variance is typically the cultural / immigration / national-
# sovereignty axis rather than redistribution. The Eiffel-tower plot is the
# diagnostic: if the highest-|β| words are dominated by "Asyl", "Migration",
# "Grenzen", "Heimat", "Tradition" on one side and "Klima", "Vielfalt",
# "Gleichstellung", "europäisch" on the other, the recovered axis is the
# cultural / GAL-TAN dimension, not classical economic left-right. This is the
# Austrian counterpart of the "Wordfish Illusion" discussed at the end of the
# Day 4 lab: the model finds the BIGGEST conflict in the texts, not the
# conflict the analyst happens to be looking for.
#
# LIMITATIONS
# -----------
# (1) Only 15 documents. Standard errors are non-trivial; we should not over-
#     interpret small shifts between adjacent years.
# (2) German morphology is rich. Without lemmatisation, "Migrant", "Migranten",
#     "Migration" are treated as distinct features, which under-counts the
#     true signal of that topic.
# (3) Wordfish recovers exactly ONE dimension. If Austrian politics is
#     genuinely two-dimensional (economy + culture), a 2-D method such as
#     correspondence analysis on the same DFM would complement this analysis.
# ==============================================================================
