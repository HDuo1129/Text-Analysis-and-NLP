## -----------------------------------------------------------------------------
## Title:       Day 3 - Preprocessing Text Data 
## Course:      DOPP 5688: Text as Data (Spring 2026)
## Author:      Daniel Weitzel
## Email:       weitzeld@ceu.edu
## Institution: Central European University
## Description: An absolute beginner's guide to processing text data 
## -----------------------------------------------------------------------------

# 1. Setup and Library Loading -------------------------------------------------
# Set your working directory to the folder containing your 'data' folder.
setwd("/Users/huangduo/Desktop/NLP/Data")

# Install packages if not already present:
# install.packages(c("manifestoR", "quanteda", "quanteda.textstats", 
#                    "quanteda.textplots", "tidyverse"))
library(manifestoR)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidyverse)

# Set the Manifesto Project API Key
# Students will need to register at https://manifesto-project.wzb.eu/ to get a key.
mp_setapikey("/Users/huangduo/Desktop/NLP/Data/apikey.txt")

# 2. Data Acquisition ----------------------------------------------------------

# We want the UK (country == 51)
# Conservative Party (party == 51620) and Labour Party (party == 51320)
mp_data <- mp_maindataset()

# 3. Converting to Quanteda Corpus ---------------------------------------------
uk_target_parties <- mp_data |>
  filter(country == 51,
         party %in% c(51320,51620)) |>
  arrange(desc(edate))

last_two_elections <- unique(uk_target_parties$edate)[1:2]

uk_recent_meta <- uk_target_parties |>
  filter(edate %in% last_two_elections)

manifesto_docs <- mp_corpus(uk_recent_meta) 

rm(mp_data, uk_target_parties, last_two_elections, uk_recent_meta)

df_mainfesto <- as_tibble(manifesto_docs)

q_corpus <- corpus(manifesto_docs)
print(summary(q_corpus))

docvars(q_corpus, "party_name") <- ifelse(docvars(q_corpus, "party") == 51620,
                                          "Conservatives", "labour")

docvars(q_corpus, "election_year") <- as.numeric(substr(as.character(docvars(q_corpus, "date")), 1, 4))

corpus_latest <- corpus_subset(q_corpus, election_year == 2024)

# 4. Text Preprocessing: Tokenization & Inspection -----------------------------
toks <- tokens(q_corpus,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE,
               remove_url = TRUE)

toks <- tokens_tolower(toks)

labour_variations <- c("labour's", "labor's", "labors", "labours", "labor")
toks <- tokens_replace(toks,
                       pattern = labour_variations,
                       replacement = rep("labour", length((labour_variations))))

tax_context <- kwic(toks, pattern = "tax", window = 5)
print(head(tax_context,10))

mainfesto_collocs <- textstat_collocations(toks, size = 2, min_count = 10)
print(head(mainfesto_collocs,10))

target_phrases <- phrase(c("downing street",
                           "labour government",
                           "public service",
                           "prime minister",
                           "green politics",
                           "national health service"))

toks <- tokens_compound(toks, pattern = target_phrases)

custom_noise_words <- c("no", "can", "h", "also", "make")
toks <- tokens_remove(toks, pattern = c(stopwords("en"), custom_noise_words))

toks_stemmed <- tokens_wordstem(toks, language = "en")
toks_stemmed[[1]]

# 5. Creating the Document-Feature Matrix (DFM) --------------------------------
mainfrsto_dfm <- dfm(toks)


# 6. Descriptive Statistics ----------------------------------------------------

# A. Top Features (Most frequent words across the whole corpus)
top_words <- topfeatures(mainfrsto_dfm)

# B. Lexical Diversity
lex_div <- textstat_lexdiv(mainfrsto_dfm, measure = "TTR")
print(lex_div)

# C. Readability
readability_score <- textstat_readability(q_corpus, measure = "Flesch.Kincaid")
print(readability_score)

# D. TF-IDF Weighting
mainfesto_tfidf <- dfm_tfidf(mainfrsto_dfm) 
print(topfeatures(mainfesto_tfidf, 10))

# E. Dictionary Analysis
policy_dict <- dictionary(list(
  economy = c("tax", "gdp", "growth", "fiscal", "investment"),
  environment = c("climate", "green", "carbon", "energy", "nature", "tree")))

dict_dfm <- dfm(tokens_lookup(toks, dictionary = policy_dict))

dict_dfm_party <- dfm_group(dict_dfm, groups = party_name)

# 7. Visualizations ------------------------------------------------------------
# Plot 1: Wordcloud BOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


# Plot 2: Frequency Plot of Top 20 Words using ggplot2

# Plot 3: Keyness (Comparing Labour vs. Conservative)


# ==============================================================================
# Fin
# ==============================================================================