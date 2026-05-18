# ==============================================================================
# Day 4 Applied Lab: Discovering Themes, Sentiment, and Ideology
# Focus: Topic Modeling (STM & Seeded LDA), Sentiment Analysis, Scaling
# ==============================================================================

# 1. Setup and Package Loading -------------------------------------------------
# Install missing packages if necessary: 
#install.packages(c("quanteda", "quanteda.textmodels", "quanteda.textstats", 
#                    "quanteda.textplots", "stm", "tidyverse", "seededlda"))

library(quanteda)
library(quanteda.textmodels) # For Wordscores, Wordfish, and the dataset
library(quanteda.textstats)  # For text statistics
library(quanteda.textplots)  # For plotting models
library(stm)                 # For Structural Topic Models
library(seededlda)           # For semi-supervised Topic Modeling
library(tidyverse)           # For data manipulation and visualization

# 2. Data Ingestion ------------------------------------------------------------
# We are using the 2010 Irish Budget debate corpus. 
# It includes 14 speeches with metadata: speaker name, party, and year.
data("data_corpus_irishbudget2010", package = "quanteda.textmodels")

# Inspect the corpus and document variables (metadata)
summary(data_corpus_irishbudget2010)
head(docvars(data_corpus_irishbudget2010))
# 3. Preprocessing -------------------------------------------------------------
# Creating a Document-Feature Matrix (DFM) using standard text cleaning steps.
toks <- tokens(data_corpus_irishbudget2010,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE) |>
  tokens_tolower() |>
  tokens_remove(stopwords("english")) |>
  tokens_wordstem()

## Inspect the performance 
# Create a temporary DFM from your current tokens
dfm_check <- dfm(toks)

# Print the top 40 most frequent words in the entire corpus
topfeatures(dfm_check, 40)

# Now check the frequency by document
# Calculate detailed frequencies
terms_freqs <- textstat_frequency(dfm_check)

# Look at the top words based on how many documents they appear in (docfreq)
head(terms_freqs, 20)

# We can now define your domain-specific stopwords based on our investigation
custom_stopwords <- c("mr", "minister", "deputy", "taoiseach", "house", 
                      "can", "will", "just", "also", "now", "year", "need", "us")

# We run the pipeline with the additional removal step
toks <- tokens(data_corpus_irishbudget2010,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE) |>
  tokens_tolower() |>
  tokens_remove(stopwords("english")) |>
  tokens_remove(custom_stopwords) |>
  tokens_wordstem()

# 3. Proceed to create your final DFM
# Create the DFM and trim rare words to improve model stability
dfm_budget <- dfm(toks) |>
  dfm_trim(min_termfreq = 5, min_docfreq = 2)

# ==============================================================================
# 4. TOPIC MODELING 
# ==============================================================================

# 4a. Unsupervised Topic Modeling: Structural Topic Model (STM) ----------------
# STM allows us to include document-level covariates (like 'party').

# Convert the quanteda DFM into a format the 'stm' package can read
stm_data <- convert(dfm_budget, to = "stm")

# Fit an STM with 5 topics, using 'party' as a prevalence covariate.
# (We use a small K=5 for the lab to ensure it runs quickly for students)
set.seed(1904) # Schalke just got promoted to the 1. Bundesliga!
model_stm <- stm(documents = stm_data$documents,
                 vocab = stm_data$vocab,
                 K = 5,
                 prevalence = ~party,
                 data = stm_data$meta,
                 init.type = "Spectral",
                 verbose = FALSE)

# Explore the topics (Top words per topic)
labelTopics(model_stm)

# Highest Prob: These are the words that occur most frequently in the topic. 
# The problem: They are usually dominated by common words (like "peopl" or 
#              "budget"), making topics look identical
# FREX (Frequency + Exclusivity): This is the most useful metric for naming a 
#                                topic. 
# It finds words that are frequent in this topic and relatively rare in other 
# topics. It balances frequency with uniqueness.
# Lift & Score: These give heavier weight to words that are highly exclusive to
# the topic, even if they aren't very frequent. You'll often see weird, highly 
# specific words here (e.g., "domicil", "fugit", "esri").

# Topic 1 (Taxation/Revenue): tax, increas, levi, level, million. 
# Topic 2 (Welfare & Banking): bank, cut, benefit, child. 
# Topic 3 (Opposition Rhetoric): fail, ask, confront, reform, tragedi. 
# Topic 4 (Fianna Fáil): fianna, fáil, enterpris, support. 
# Topic 5 (Sinn Féin / Macroeconomy): state, stimulus, govern, taxat.

# Define a custom list of political stopwords based on our first model run
custom_stopwords <- c(
  "budget", "peopl", "year", "public", "govern", "one", "us", "need", 
  "must", "can", "will", "new", "increas", "make", "also", "say", "said",
  "fianna", "fáil", "sinn", "féin", "lenihan", "deputi", "minister"
)

# What's next? 
# More stopwords to remove 
# More topics to estimate

# Estimate covariate effects: Does topic prevalence differ by party?
effects <- estimateEffect(~party, model_stm, metadata = stm_data$meta)
summary(effects, topics = 1:5)

# The dependent variable (Y) is the proportion of a document dedicated to a 
# specific topic, and the independent variables (X) are the party affiliations.
# FF is the intercept (the baseline).
# Every other coefficient tells you how much more or less a specific party talks
# about a topic compared to Fianna Fáil.

# Plot the topic proportions
plot(model_stm, type = "summary", main = "STM: Topic Proportions in Budget Debate")

# Plot the expected topic proportions by party
plot(effect, covariate = "party", topics = 1, 2, 5)

# 4b. Semi-Supervised Topic Modeling: Seeded LDA -------------------------------
# Seeded LDA allows us to "anchor" topics based on theoretical expectations.

# Define a dictionary of seed words for the topics we expect in a budget debate
dict_seeds <- dictionary(list(
  economy = c("economy", "growth", "recovery", "banks", "debt", "deficit"),
  taxation = c("tax", "income", "vat", "revenue", "levy", "burden"),
  welfare = c("social", "welfare", "health", "education", "families", "vulnerable")
))

# Fit the Seeded LDA model
# Setting residual = TRUE creates an "other" topic for unrelated words.
set.seed(1904)


# View the top 10 terms driving each topic (seeded + the residual "other" topic)

# Assign the dominant topic to each document and add it to our metadata

# Convert to a data frame to visualize which party focuses on which seeded topic


# Plot the topical focus by party


# ==============================================================================
# 5. AUTOMATED SENTIMENT ANALYSIS (Dictionary Approach)
# ==============================================================================
# We use the Lexicoder Sentiment Dictionary (LSD2015), built into quanteda 
# and designed specifically for political text (Young & Soroka).
# This dictionary is powerful because it includes "negations" (e.g., "not good" 
# is flagged as 'neg_positive', not 'positive').

# Apply the dictionary to our tokens

# Convert to a DFM to count the sentiment words per speech

# Convert to a data frame for tidyverse manipulation


# INTERPRETATION: 
# A score of 0 means the speech is perfectly balanced. 
# Negative scores indicate a pessimistic or critical tone (very common for opposition).
# Positive scores indicate an optimistic or defensive tone (common for government).

# Plot Sentiment by Party


# 5b. What words are driving the sentiment? ------------------------------------
# It is never enough to just look at the scores. We need to look under the hood 
# to see WHICH negative or positive words the politicians are actually using.

# Let's extract all the words that the dictionary flagged as "negative"

# Print the top 15 most frequent negative words in the corpus
# INTERPRETATION: If the top negative words are "debt", "deficit", "cuts", 
# the sentiment is driven by policy substance. If they are "fail", "disaster", 
# "shame", it is driven by political rhetoric.



# ==============================================================================
# 6. SPATIAL SCALING
# ==============================================================================

# 6a. Wordscores (Supervised) --------------------------------------------------
# Wordscores requires reference texts with known ideological positions.
# We will set the governing party (Fianna Fáil) to 1 (Right/Gov), 
# and the main opposition (Fine Gael) to -1 (Left/Opp).
# Other speeches get NA and will be placed on this continuum.


# Fit the Wordscores model

# Predict positions for the undefined texts (Virgin texts)
# CRUCIAL STEP: Raw Wordscores suffer from "shrinkage" (they cluster around 0).
# We use rescaling = "lbg" (Laver, Benoit, and Garry 2003) to stretch the 
# virgin texts back onto our original -1 to 1 ideological scale.

# Plot the predicted positions

# INTERPRETATION: 
# Did the un-scored parties (Labour, Sinn Féin, Greens) fall where we 
# theoretically expect them to on the Left-Right/Gov-Opp spectrum?

# 6a.2. Inspecting the Word Scores
# We can extract the specific scores assigned to each word.
# Words near 1 are highly "Fianna Fáil", words near -1 are highly "Fine Gael".


# 6b. Wordfish (Unsupervised) --------------------------------------------------
# Wordfish estimates a single dimension WITHOUT reference texts.
# It assumes variation in word frequency is driven by a single latent ideology.
# We use 'dir = c(6, 5)' to anchor the left/right direction so the model 
# doesn't arbitrarily flip the axis backwards.

# Fit the Wordfish model

# Plot the estimated document positions (Theta)
# What this shows: Where each speech/politician falls on the latent ideological 
# dimension extracted by the model.

# How to read it: 
# - The X-axis represents the latent dimension (Theta).
# - The dots are the individual documents (speeches), grouped by party.
# - The horizontal lines through the dots are confidence intervals (uncertainty).
# - Actors on opposite ends of this axis use systematically different vocabularies.

# Plot the word weights (Beta) to see which words drive the dimension
# What this shows: The math under the hood. It plots every word's baseline 
# frequency against its ideological discrimination weight.

# How to read it:
# - Y-axis (Psi): How often is the word used overall? Words at the top are 
#   universal (e.g., "government", "people"). Words at the bottom are rare.
# - X-axis (Beta): How partisan/polarized is the word? Words at 0 are neutral. 
#   Words far to the left or right are heavily associated with one end of the 
#   ideological spectrum.
# - The 'wings' of the tower show us the most distinguishing vocabulary.

# 6b.2. Extracting Wordfish Parameters (Under the Hood)
# The "Eiffel Tower" plot is nice, but as researchers, we want to see the math.
# Let's extract the Beta (Ideological weight) and Psi (Baseline frequency) 
# for every single word in the corpus.



# Sort to find the words that push documents the furthest to the "Right"

# Sort to find the words that push documents the furthest to the "Left"

# INTERPRETATION: 
# Look at these extreme beta words. Do they represent genuine ideological 
# differences (e.g., "taxes" vs "welfare"), or did the model just pick up 
# on the names of local constituencies or specific politicians? 
# If it's just names, we need to go back and add them to our stopword list!
