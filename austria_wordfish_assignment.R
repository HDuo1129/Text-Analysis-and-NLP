# ==============================================================================
# DOPP 5688 — Text as Data (Spring 2026)
# Assignment: Method Application
# Author:  Huang Duo
# Email:   Huang_Duo@student.ceu.edu
# Method:  Wordfish (Unsupervised Spatial Scaling)
# Data:    Austrian party manifestos, 2013 / 2017 / 2019 (via manifestoR / MARPOR)
# Goal:    Estimate the ideological positions of the five main Austrian
#          parliamentary parties across three recent federal elections and
#          interpret the dimension that Wordfish recovers.
# ==============================================================================

# 1. Setup and Package Loading -------------------------------------------------
library(manifestoR)             # API access to MARPOR corpus
library(quanteda)               # corpus / tokens / DFM
library(quanteda.textmodels)    # textmodel_wordfish
library(quanteda.textplots)     # textplot_scale1d
library(tidyverse)              # data wrangling + ggplot2
set.seed(1904)


# 2. Data Ingestion ------------------------------------------------------------
mp_setapikey("/Users/huangduo/Desktop/NLP/manifesto_apikey.txt")

# Pull the MARPOR main dataset

# Why these three elections?
#   - 2013: NEOS entered parliament for the first time; SPÖ-ÖVP grand coalition continued.
#   - 2017: Sebastian Kurz led ÖVP to first place; formed right-wing coalition with FPÖ.
#   - 2019: Post-Ibiza snap election; ÖVP formed an unprecedented coalition with the Greens.
# This span covers a decade of significant realignment in Austrian politics and gives
# enough variation to see whether party positions drift over time.

mp_main <- mp_maindataset()

at_meta <- mp_main |>
  filter(countryname == "Austria",
         edate >= as.Date("2013-01-01"),
         edate <= as.Date("2019-12-31")) |>
  # Filter by partyabbrev (more stable than partyname across dataset versions).
  # NEOS ran under the same abbreviation in both 2013 ("The New Austria") and
  # 2017/2019 ("The New Austria and Liberal Forum").
  filter(partyabbrev %in% c("SPÖ", "ÖVP", "FPÖ", "GRÜNE", "NEOS")) |>
  select(party, partyname, partyabbrev, edate, date) |>
  arrange(edate, partyabbrev)

cat("Manifestos to download:\n")
print(at_meta)

# Download the actual texts
at_corpus_marpor <- mp_corpus(countryname == "Austria" &
                                edate >= as.Date("2013-01-01") &
                                edate <= as.Date("2019-12-31") &
                                party %in% at_meta$party)

# 3. Build a quanteda corpus ---------------------------------------------------

doc_ids   <- names(at_corpus_marpor)
doc_texts <- map_chr(doc_ids, function(doc_id) {
  paste(content(at_corpus_marpor[[doc_id]]), collapse = " ")
})

doc_df <- tibble(doc_id = doc_ids, text = doc_texts) |>
  mutate(
    party_code = as.integer(str_extract(doc_id, "^[0-9]+")),       # everything before "_"
    year       = as.integer(str_extract(doc_id, "(?<=_)[0-9]{4}")) # first 4 digits after "_"
  ) |>
  left_join(distinct(at_meta, party, partyabbrev),
            by = c("party_code" = "party")) |>
  mutate(doc_label = paste(partyabbrev, year, sep = "_"))

at_corpus <- corpus(doc_df, docid_field = "doc_label", text_field = "text")

cat("\nCorpus summary:\n")
print(summary(at_corpus))


# 4. Preprocessing -------------------------------------------------------------

# METHODOLOGICAL JUSTIFICATION (no stemming):
# Following the last step, we deliberately skip stemming. Stemming collapses
# ideologically loaded variants (e.g., "Migrant" vs. "Migration", "Asylant"
# vs. "Asyl") that often carry distinct partisan signals. For a scaling model
# whose entire job is to detect such signals, stemming destroys exactly the
# variation we want to measure.

# German stopwords from quanteda's built-in list, plus custom procedural words.
# We strip generic political vocabulary that appears in every manifesto regardless
# of position ("Österreich", "Bundesregierung", etc.) so Wordfish picks up
# substantive partisan vocabulary rather than common nouns.
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
  tokens_remove(stopwords("german")) |> 
  tokens_remove(custom_stopwords) |>
  tokens_remove(pattern = "^.{1,2}$", valuetype = "regex")  # drop 1-2 character fragments

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

# Find row indices for the anchors.
# Print all labels first so you can verify what MARPOR actually returned.
doc_labels <- docnames(dfm_docs)
cat("Available document labels:\n"); print(doc_labels)

# Robust detection: match "gr" (catches Grüne / GRÜNE) and "fp" (catches FPÖ / fpö)
# combined with the year. We use the most recent available election (2019) as anchors.
idx_low  <- which(str_detect(tolower(doc_labels), "gr") & str_detect(doc_labels, "2019"))[1]
idx_high <- which(str_detect(tolower(doc_labels), "fp") & str_detect(doc_labels, "2019"))[1]

if (is.na(idx_low) || is.na(idx_high)) {
  stop("Could not locate anchor documents — check the labels printed above.")
}

wf <- textmodel_wordfish(dfm_docs, dir = c(idx_low, idx_high))

cat("\nWordfish model fitted.\n")
print(summary(wf))


# 6. Visualisation -------------------------------------------------------------

# 6a. Document positions (theta) ----------------------------------------------
# Grouped by party so we can see (i) cross-party separation and (ii) within-
# party drift across elections.

plot_positions <- textplot_scale1d(
  wf,
  groups = docvars(dfm_docs, "partyabbrev"),
  margin = "documents"
) +
  ggtitle("Wordfish Positions of Austrian Manifestos (2013, 2017, 2019)",
          subtitle = "Anchors: Grüne 2019 (low) — FPÖ 2019 (high)") +
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
  scale_x_continuous(breaks = c(2013, 2017, 2019)) +
  scale_color_manual(values = party_colors) +
  labs(title    = "Ideological Drift: Austrian Parties on the Wordfish Axis (2013-2019)",
       subtitle = "95 % CIs from Wordfish standard errors",
       x = "Election year", y = "Wordfish position (θ)") +
  theme_minimal()

print(plot_drift)

# 7. Interpretation Aid: Top discriminating words ------------------------------
# Print the 20 most "high-end" (FPÖ-leaning) and "low-end" (Grüne-leaning)
# words by beta. This is what we use to NAME the axis after the model has
# recovered it.

word_betas <- tibble(
  word = featnames(dfm_docs),
  beta = wf$beta,
  psi  = wf$psi
) |>
  filter(psi > median(psi))

cat("\n--- Top 20 words on the HIGH end (anchored toward FPÖ 2019) ---\n")
print(word_betas |> arrange(desc(beta)) |> slice_head(n = 20))

cat("\n--- Top 20 words on the LOW end (anchored toward Grüne 2019) ---\n")
print(word_betas |> arrange(beta) |> slice_head(n = 20))


# ==============================================================================
# 8. Findings and Interpretation
# ==============================================================================

# --- METHOD JUSTIFICATION -----------------------------------------------------
# Wordfish is the appropriate method for this research question for three
# reasons. First, the goal is to discover the dominant dimension of
# disagreement across manifestos without imposing a prior assumption about
# what that dimension is (e.g. left vs. right). Wordfish is unsupervised:
# it recovers the axis from word-frequency patterns alone. Second, the unit
# of analysis is whole manifestos — one document per party per election —
# which is exactly the granularity Wordfish requires. Sentence-level data
# would be too sparse for reliable estimation. Third, the corpus spans three
# elections, making it possible to track positional drift over time, which is
# a core advantage of fitting Wordfish to a multi-election corpus.

# --- WHAT DIMENSION DOES WORDFISH RECOVER? ------------------------------------
# The top discriminating words from Section 7 identify the axis clearly.
#
# The NEGATIVE end is anchored by GRÜNE's vocabulary: "menschenrechte"
# (human rights), "gleichstellung" (gender equality), "zivilgesellschaft"
# (civil society), "ökologische" (ecological), "fossilen" (fossil fuels),
# "klimaschutz" (climate protection), "tierschutz" (animal welfare),
# "datenschutz" (data privacy). These are textbook Green-Alternative-
# Libertarian (GAL) values focused on rights, ecology, and civil freedoms.
#
# The POSITIVE end is defined by a mix of two things. The first is
# economic-conservative policy vocabulary concentrated in ÖVP's 2017 manifesto:
# "schulden" (debt), "bürokratie" (bureaucracy), "lohnnebenkosten"
# (non-wage labour costs), "progression" (tax bracket reform). The second is
# rhetorical style: words like "natürlich", "deswegen", "nämlich", and
# "eigentlich" are conversational discourse markers characteristic of Kurz's
# deliberately informal campaign communication. Wordfish is sensitive to HOW
# parties write, not only WHAT they write about.
#
# The recovered axis is therefore not classical economic left–right. It
# reflects a GAL–TAN cultural dimension overlaid with the distinctive
# rhetorical register of ÖVP's 2017 campaign. This is an instance of the
# Wordfish "Illusion" discussed in the Day 4 lab: the model finds the single
# biggest source of textual variance, which here is a combination of ideology
# and rhetorical style, not the dimension the analyst might have expected.

# --- MAIN FINDINGS ------------------------------------------------------------
# (1) ÖVP's shift in 2017 (θ: +0.16 → +1.65) is the most striking result.
#     Under Kurz, ÖVP's manifesto scored higher on this axis than FPÖ itself —
#     meaning its text was more linguistically extreme than the party it was
#     competing with for right-wing voters. This reflects the documented
#     strategy of absorbing FPÖ's cultural and security agenda while adding a
#     layer of economic-reform and school-system vocabulary that FPÖ lacked.
#
# (2) FPÖ shifted toward the centre in relative terms (θ: +1.01 → +0.27 →
#     +0.43). This does not mean FPÖ moderated its politics. It means ÖVP
#     occupied the high end of the linguistic space, pushing FPÖ's relative
#     position downward. This is a fundamental property of unsupervised
#     scaling: positions are relative, not absolute.
#
# (3) GRÜNE drifted further from all other parties across all three elections
#     (θ: −1.47 → −1.56 → −1.91). Its vocabulary became increasingly
#     distinctive, reflecting a sharpening of its environmental and rights-
#     based platform over time, particularly as migration politics dominated
#     the 2017 and 2019 campaigns and GRÜNE chose not to follow that frame.
#
# (4) SPÖ shows an unusual rightward jump in 2017 (θ: −0.22 → +0.59), then
#     returns near the centre in 2019 (θ: +0.05). This suggests that even the
#     centre-left party incorporated more security-oriented vocabulary during
#     the 2017 migration debate, before reverting to a more typical social-
#     democratic register in 2019.

# --- LIMITATIONS --------------------------------------------------------------
# (1) Small corpus: 15 documents total. Standard errors are considerably
#     larger for shorter manifestos (e.g. FPÖ_2013, SE = 0.13 vs. ÖVP_2017,
#     SE = 0.02), so year-to-year shifts for smaller parties should be
#     interpreted with caution.
# (2) No lemmatisation: German inflectional forms such as "Migrant",
#     "Migranten", and "Migration" are treated as separate features. This
#     underestimates the true frequency of that topic across the corpus.
# (3) Single dimension: Wordfish recovers exactly one axis. Austrian politics
#     plausibly has a second economic dimension (redistribution, welfare state)
#     that this model cannot separate from the cultural one.
# ==============================================================================
# ==============================================================================
