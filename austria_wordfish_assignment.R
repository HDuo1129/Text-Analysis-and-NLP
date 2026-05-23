# ==============================================================================
# DOPP 5688 — Text as Data (Spring 2026)
# Assignment: Method Application
#
# Author:  Huang Duo
# Method:  Wordfish (Day 4 — Unsupervised Spatial Scaling)
# Data:    Austrian party manifestos, 2013 / 2017 / 2019 (via manifestoR / MARPOR)
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
#                    "quanteda.textplots", "tidyverse"))

library(manifestoR)             # API access to MARPOR corpus
library(quanteda)               # corpus / tokens / DFM
library(quanteda.textmodels)    # textmodel_wordfish
library(quanteda.textplots)     # textplot_scale1d
library(tidyverse)              # data wrangling + ggplot2
set.seed(1904)


# 2. Data Ingestion ------------------------------------------------------------

# Set the API key. The file should contain ONLY the key string on one line.
mp_setapikey("/Users/huangduo/Desktop/NLP/manifesto_apikey.txt")

# Pull the MARPOR main dataset (small metadata table, downloads in seconds).
# We then filter to Austria + the three most recent federal elections we want.
#
# Why these three elections?
#   - 2013: NEOS entered parliament for the first time; SPÖ-ÖVP grand coalition continued.
#   - 2017: Sebastian Kurz led ÖVP to first place; formed right-wing coalition with FPÖ.
#   - 2019: Post-Ibiza snap election; ÖVP formed an unprecedented coalition with the Greens.
# This span covers a decade of significant realignment in Austrian politics and gives
# enough variation to see whether party positions drift over time.
# NOTE: MARPOR does not yet include the 2024 Austrian election (coded with a typical
# lag of 1-2 years after the election date).

mp_main <- mp_maindataset()

at_meta <- mp_main |>
  filter(countryname == "Austria",
         edate >= as.Date("2013-01-01"),
         edate <= as.Date("2019-12-31")) |>
  # Filter by partyabbrev (more stable than partyname across dataset versions).
  # NEOS ran under the same abbreviation in both 2013 ("The New Austria") and
  # 2017/2019 ("The New Austria and Liberal Forum"), so one filter catches all.
  filter(partyabbrev %in% c("SPÖ", "ÖVP", "FPÖ", "GRÜNE", "NEOS")) |>
  select(party, partyname, partyabbrev, edate, date) |>
  arrange(edate, partyabbrev)

cat("Manifestos to download:\n")
print(at_meta)

# Download the actual texts (quasi-sentences with MARPOR codes). The result is
# a tm-style Corpus. We immediately bypass the cache to make sure we get fresh
# data on first run; subsequent runs are cached locally by manifestoR.
at_corpus_marpor <- mp_corpus(countryname == "Austria" &
                                edate >= as.Date("2013-01-01") &
                                edate <= as.Date("2019-12-31") &
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

# Same pattern as HW2: iterate over names(), access each document with [[doc_id]],
# then call content() on that single ManifestoDocument object.
# map_chr() returns one string per manifesto (the full text collapsed into one value),
# while map_dfr() in HW2 returned one row per quasi-sentence.
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
# Following the Day 4 lab, we deliberately skip stemming. Stemming collapses
# ideologically loaded variants (e.g., "Migrant" vs. "Migration", "Asylant"
# vs. "Asyl") that often carry distinct partisan signals. For a scaling model
# whose entire job is to detect such signals, stemming destroys exactly the
# variation we want to measure.

# German stopwords from quanteda's built-in list (same principle as stopwords("english")
# used in the Day 4 lab), plus custom procedural / corpus-specific words.
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
  tokens_remove(stopwords("german")) |>        # same pattern as stopwords("english") in Day 4
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
# flip the spectrum. We anchor FPÖ 2019 to the high end and Grüne 2019 to the
# low end — these are the two parties whose ideological distance is least
# disputed in the Austrian context. Note: this only fixes the sign, not the
# substance of the dimension.

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
# This is the main result: each manifesto's position on the recovered axis.
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

cat("\n--- Top 20 words on the HIGH end (anchored toward FPÖ 2019) ---\n")
print(word_betas |> arrange(desc(beta)) |> slice_head(n = 20))

cat("\n--- Top 20 words on the LOW end (anchored toward Grüne 2019) ---\n")
print(word_betas |> arrange(beta) |> slice_head(n = 20))


# ==============================================================================
# 8. Findings and Interpretation
# ==============================================================================
#
# WHAT DIMENSION DOES WORDFISH RECOVER?
# --------------------------------------
# The top discriminating words (Section 7) reveal a more complex dimension
# than a simple cultural axis.
#
# The NEGATIVE end (GRÜNE-leaning) is clearly defined by progressive and
# rights-based vocabulary: "menschenrechte" (human rights), "gleichstellung"
# (gender equality), "zivilgesellschaft" (civil society), "ökologische"
# (ecological), "fossilen" (fossil fuels), "klimaschutz" (climate protection),
# "tierschutz" (animal welfare), "datenschutz" (data privacy). These are
# textbook Green-Alternative-Libertarian (GAL) values.
#
# The POSITIVE end (ÖVP/FPÖ-leaning) is more mixed. It contains:
#   (a) Economic-conservative policy words: "schulden" (debt reduction),
#       "bürokratie" (cutting bureaucracy), "lohnnebenkosten" (reducing
#       non-wage labour costs), "gebühren" (fees), "progression" (tax
#       bracket reform) — all key ÖVP 2017 campaign promises.
#   (b) Public service reform words: "lehrer" (teachers), "schulsystem"
#       (school system), "patienten" (patients).
#   (c) Rhetorical style markers: "natürlich", "deswegen", "nämlich",
#       "eigentlich" — conversational connectors characteristic of Kurz's
#       direct, informal campaign communication style.
#
# This means Wordfish captures not only ideological content but also rhetorical
# style. ÖVP_2017 scored the highest not only because of its policy agenda but
# also because of a distinctively casual, direct speaking style that set it
# apart from the more formal language of other parties.
# This is exactly the "Wordfish Illusion" from the Day 4 lab: the axis reflects
# the single biggest source of textual variance, which here is a combination of
# ideology, policy priorities, and rhetorical register.
#
# MAIN FINDINGS FROM THE DRIFT PLOT
# -----------------------------------
# (1) ÖVP's dramatic shift in 2017 (θ: +0.16 → +1.65) is the most striking
#     result. Under Sebastian Kurz, ÖVP deliberately adopted the nationalist
#     and security-focused language of FPÖ — so much so that its manifesto
#     text scored MORE extreme than FPÖ's own manifesto on this axis. This
#     reflects the well-documented "Kurz strategy" of stealing FPÖ's agenda
#     on migration and national identity to win the election.
#
# (2) FPÖ became relatively more centrist (θ: +1.01 → +0.27 → +0.43). This
#     does not mean FPÖ moderated its actual policy positions. Rather, once
#     ÖVP occupied the extreme right of the linguistic space, FPÖ's relative
#     position shifted toward the centre. This is a known limitation of
#     unsupervised scaling: it measures relative position, not absolute
#     radicalism.
#
# (3) GRÜNE drifted further left across all three elections (-1.47 → -1.91),
#     indicating that its manifesto language became increasingly distinctive
#     and separated from all other parties. This reflects a progressive
#     sharpening of their environmental and identity-based platform.
#
# (4) SPÖ shows an unusual rightward jump in 2017 (+0.59), suggesting that
#     even the centre-left party adopted more security and nation-related
#     vocabulary under pressure from the migration debate that dominated
#     that election campaign, before returning near the centre in 2019.
#
# LIMITATIONS
# -----------
# (1) Only 15 documents total. The standard errors for smaller manifestos
#     (e.g. FPÖ_2013, SE = 0.13) are considerably larger, so we should be
#     cautious about over-interpreting year-to-year changes for those parties.
# (2) Without lemmatisation, German word forms like "Migrant", "Migranten",
#     and "Migration" are counted as separate features, which under-counts
#     the true frequency of that topic across the corpus.
# (3) Wordfish extracts exactly one dimension. Austrian politics likely has
#     a second economic dimension (redistribution, welfare state) that this
#     model cannot capture. A two-dimensional method would be needed to
#     disentangle cultural from economic conflicts.
# ==============================================================================
