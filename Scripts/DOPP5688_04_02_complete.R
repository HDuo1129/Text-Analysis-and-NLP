# ==============================================================================
# Day 4 Applied Lab: Discovering Themes, Sentiment, and Ideology
# Focus: Topic Modeling, Sentiment Analysis, Scaling (REVISED PIPELINE)
# ==============================================================================

# UPDATED SCRIPT WITH ADDITIONAL EXPLANATIONS!

# 1. Setup and Package Loading -------------------------------------------------
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(stm)
library(seededlda)
library(tidyverse)

# 2. Data Ingestion ------------------------------------------------------------
data("data_corpus_irishbudget2010", package = "quanteda.textmodels")

# 3. Preprocessing (The Two-Track Approach) ------------------------------------
# --- METHODOLOGICAL EXPLANATION: SENTENCES VS. DOCUMENTS ---
# We are creating two separate corpora. 
# 1. Sentence-level: Best for Topic Modeling and Sentiment. It isolates distinct 
#    thoughts and captures the emotional arc of a speech.
# 2. Document-level: Best for Wordscores/Wordfish. Scaling algorithms need as 
#    much vocabulary as possible per actor to reliably map their ideology. 
#    Sentences are too sparse for spatial scaling.

# Track A: Sentence-Level Corpus
# THIS IS NEW! Here we generate a second corpus that is sentence based
corpus_sents <- corpus_reshape(data_corpus_irishbudget2010, to = "sentences")

# Track B: Document-Level Corpus (Original)
corpus_docs <- data_corpus_irishbudget2010

# BE AWARE THAT WE NOW HAVE TWO CORPUS OBJECTS AND NEED TO HANDLE THOSE INDIVIDUALLY

# Define our unstemmed custom stopwords 
# (Since we removed stemming, we must include full word forms!)
custom_stopwords <- c(
  "mr", "minister", "deputy", "deputies", "taoiseach", "house", 
  "can", "will", "just", "also", "now", "budget", "people", "year", 
  "years", "public", "government", "one", "us", "need", "must", 
  "new", "increase", "increased", "make", "say", "said",
  "fianna", "fáil", "sinn", "féin", "lenihan"
)

# --- METHODOLOGICAL EXPLANATION: NO STEMMING ---
# We have entirely removed `tokens_wordstem()` from this pipeline.
# 1. Stemming ruins pre-built sentiment dictionaries (like LSD2015), which look 
#    for specific word endings and wildcards.
# 2. Stemming destroys ideological nuance in scaling models (e.g., "migrants" 
#    vs "migration" might be heavily polarized, but stemming collapses them).
# Apply cleaning to sentence level corpus
toks_sents <-  tokens(corpus_sents,  # NOTE THAT THIS HAS corpus_sents for sentences
                      remove_punct = TRUE, 
                      remove_numbers = TRUE, 
                      remove_symbols = TRUE) |>
  tokens_tolower() |>
  tokens_remove(stopwords("english")) |>
  tokens_remove(custom_stopwords)

# Apply cleaning to document level corpus
toks_docs <-  tokens(corpus_docs, # NOTE THAT THIS HAS corpus_docs for documents
                     remove_punct = TRUE, 
                     remove_numbers = TRUE, 
                     remove_symbols = TRUE) |>
  tokens_tolower() |>
  tokens_remove(stopwords("english")) |>
  tokens_remove(custom_stopwords)

# Create DFMs
# For sentences, trimming might create completely empty rows. We must remove 
# empty documents after trimming so STM doesn't crash!
dfm_sents <- dfm(toks_sents) |>
  dfm_trim(min_termfreq = 5, min_docfreq = 2) |>
  dfm_subset(ntoken(dfm(toks_sents)) > 0) 

dfm_docs <- dfm(toks_docs) |>
  dfm_trim(min_termfreq = 5, min_docfreq = 2)

# ==============================================================================
# 4. TOPIC MODELING (Using Sentence-Level DFM)
# ==============================================================================

# 4a. Structural Topic Model (STM) ---------------------------------------------
stm_data <- convert(dfm_sents, to = "stm")

# --- METHODOLOGICAL EXPLANATION: SPECTRAL VS. LDA INITIALIZATION ---
# We use init.type = "Spectral" and here is why:
# "LDA" initialization relies on random sampling to find a starting point. If you 
# change the seed, your topics do in fact change completely. It is unstable and often gets 
# stuck in mediocre "local optima."
# "Spectral" initialization is deterministic. It uses a method of moments based 
# on the word co-occurrence matrix. It starts in the exact same mathematical 
# place every time, vastly improving stability, reproducibility, and model fit.
set.seed(1904) 
model_stm <- stm(documents = stm_data$documents, 
                 vocab = stm_data$vocab, 
                 K = 5, 
                 prevalence = ~ party, 
                 data = stm_data$meta, 
                 init.type = "Spectral", 
                 verbose = FALSE)
# ------------------------------------------------------------------------------
# STM MODEL PARAMETER EXPLANATIONS
# ------------------------------------------------------------------------------
#
# K = 5 (Number of Topics): 
# K is a hyperparameter chosen by the researcher. We are using K=5 because it 
# runs fast in a classroom setting. In a real research paper, you would likely 
# test models with various settings of K, or use K = 0 to let the algorithm 
# guess the optimal number (which takes much longer to run).
#
# prevalence = ~ party: 
# This is the magic of the Structural Topic Model. Traditional models assume 
# all documents pull from topics equally. By adding a prevalence covariate, 
# we are telling the model: "Hey, a politician's party affiliation probably 
# influences how much they talk about a specific topic."
#
# Why didn't we use a content covariate? 
# STM also allows for `content = ~ party`, which assumes different parties use 
# entirely different vocabularies when discussing the same topic (e.g., the Left 
# says "undocumented", the Right says "illegal"). We omitted this because it 
# makes the model computationally heavy and the output much harder to interpret 
# for beginners.
#
# init.type = "Spectral": 
# Models need a starting point. Random starting points (like traditional LDA) 
# mean the model looks different every time you run it. (This was news to me) 
# "Spectral" uses a math  trick (matrix decomposition) to start in the exact same, 
# optimized place every time, ensuring every student gets the same result.
# ------------------------------------------------------------------------------

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

# ESTIMATE COVARIATE EFFECTS
# Now that we have topics, we want to prove mathematically if parties use them differently.
# estimateEffect() runs a regression where Y = Topic Proportion and X = Party.
# It is vital to use this function because it correctly accounts for the mathematical 
# uncertainty generated by the topic model.
effect <- estimateEffect(~ party, model_stm, metadata = stm_data$meta)
summary(effect)

# PLOT THE EFFECTS
# method = "pointestimate" plots the expected proportion of a topic by party.
# HOW TO READ THIS GRAPH:
# - The Dot: The model's best guess of how much a party talks about the topic (e.g., 10%).
# - The Lines: The 95% Confidence Interval (the margin of error).
# - THE RULE: If the lines for two parties OVERLAP horizontally, there is NO statistical 
#             difference in their attention to that topic. If they DO NOT overlap, 
#             the difference is statistically significant.
plot(effect, covariate = "party", topics = c(1, 2, 5), 
     model = model_stm, method = "pointestimate", 
     main = "Topic Prevalence by Party (Sentence Level)")


# 4b. Semi-Supervised Topic Modeling: Seeded LDA -------------------------------
dict_seeds <- dictionary(list(
  economy = c("economy", "growth", "recovery", "banks", "debt", "deficit"),
  taxation = c("tax", "income", "vat", "revenue", "levy", "burden"),
  welfare = c("social", "welfare", "health", "education", "families", "vulnerable")
))

set.seed(1904)
model_slda <- textmodel_seededlda(dfm_sents, 
                                  dictionary = dict_seeds, 
                                  residual = TRUE)

# --- METHODOLOGICAL EXPLANATION: WHY OUR ASSIGNMENT WAS CAUSING A BUG ---
# Older tutorials use `dfm$topic <- topics(model)`. This is dangerous because a 
# DFM is an S4 object, not a dataframe. Using the `$` operator can fail silently 
# or corrupt the object. Always use `docvars()` to append metadata to a DFM safely.

# THIS WAS ME WRITING CODE THAT HAD A HIGH PROB OF FAILING AND THEN IT DID
# I just relied on my old knowledge of quanted abehavior but updates have changed. 
# Let this be a warning that you always need to read the docs
docvars(dfm_sents, "seeded_topic") <- topics(model_slda)

# Convert to data frame to visualize
# Extract just the metadata (which now includes your assigned seeded_topic)
df_topics <- docvars(dfm_sents) |>
  # Count the occurrences of each topic per party
  count(party, seeded_topic) |>
  # Group by party to calculate the internal proportions
  group_by(party) |>
  mutate(proportion = n / sum(n))

# Visualize the proportion of topics the two speakers of each political party 
# talk about 
ggplot(df_topics, aes(x = party, y = proportion, fill = seeded_topic)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Seeded LDA: Topic Proportions by Party (Sentence Level)",
       x = "Political Party", y = "Proportion of Sentences", fill = "Topic")


# ==============================================================================
# 5. AUTOMATED SENTIMENT ANALYSIS (Sentence-Level)
# ==============================================================================
# We use the unstemmed sentence tokens here so the LSD2015 dictionary matches properly.
toks_sentiment <- tokens_lookup(toks_sents, dictionary = data_dictionary_LSD2015)
dfm_sentiment <- dfm(toks_sentiment)

# Convert the sentiment DFM to a data frame
# I have added a lot of comments into the pipe to explain the code to you
# 1. Extract the raw sentiment word counts from our DFM and turn it into a 
# standard data frame so we can use tidyverse tools (like mutate and group_by).
df_sentiment <- convert(dfm_sentiment, to = "data.frame") |>
  # 2. Attach our metadata (like which politician is speaking and their party).
  bind_cols(docvars(toks_sents)) |> 
  mutate(
    # 3. Handle Negations! 
    # A naive dictionary counts "not good" as a positive word ("good"). 
    # The LSD2015 dictionary is smart enough to flag "not good" as 'neg_positive'.
    # We subtract these negated words to find the TRUE number of positive words.
    true_pos = positive - neg_positive,
    
    # Same logic for negative words (e.g., "not bad" shouldn't count as negative).
    true_neg = negative - neg_negative,
    
    # 4. Count the total words in the sentence. We need this for normalization.
    total_words = ntoken(toks_sents),
    
    # 5. Calculate the Net Sentiment Score.
    # Formula: (True Positive - True Negative) / Total Words
    # WHY DIVIDE BY TOTAL WORDS? If we don't, a 50-word sentence will look 
    # 5 times more emotional than a 10-word sentence just because it's longer.
    # The 'ifelse' statement protects us from a math error (dividing by zero) 
    # if a sentence is completely empty.
    net_sentiment = ifelse(total_words > 0, (true_pos - true_neg) / total_words, 0)
  ) |>
  
  # 6. Aggregate up to the Party level.
  # This calculates the average (mean) sentiment score for each party across 
  # all of their individual sentences.
  group_by(party) |>
  mutate(mean_party_sentiment = mean(net_sentiment, na.rm = TRUE))

# ==============================================================================
# VISUALIZING THE SENTIMENT
# ==============================================================================

# We use a boxplot because it shows us the full *distribution* of a party's 
# rhetoric, not just a single average number. 
# aes(x = reorder(...)): This automatically sorts the parties on the x-axis 
# from most negative to most positive, making the graph much easier to read!
ggplot(df_sentiment, aes(x = reorder(party, net_sentiment), y = net_sentiment, fill = party)) +
  
  # outlier.alpha = 0.3 makes the extreme sentences more transparent so they 
  # don't completely overwhelm the visual plot.
  geom_boxplot(outlier.alpha = 0.3) +
  theme_minimal() +
  labs(title = "Net Sentiment Distributions by Party (Sentence-Level)",
       subtitle = "Calculated per sentence, accounting for negations",
       x = "Party", y = "Sentence Net Sentiment Score")


# ==============================================================================
# 5c. NARRATIVE ARCS: Sentiment Over Time
# ==============================================================================
# Additional plot because I talked about it in class. Here we can now see sentence
# based sentiment scores. How does the net sentiment develop OVER the course of a speech

# We need to reate a "Timeline" Variable
# We need to know the exact order the sentences were spoken. 
# By grouping by party and using row_number(), we create a timeline from 
# sentence 1 to their final sentence.
df_timeline <- df_sentiment |>
  group_by(party) |>
  mutate(speech_timeline = row_number()) |>
  ungroup()

# Plot the Narrative Arcs
# We use facet_wrap to give each party its own mini-graph. If we put all the 
# lines on a single graph, it becomes an unreadable "spaghetti monster."
ggplot(df_timeline, aes(x = speech_timeline, y = net_sentiment, color = party)) +
  # Plot the raw sentences as very transparent, tiny dots. 
  # We want to see them faintly in the background, but they aren't the main focus.
  geom_point(alpha = 0.15, size = 1) +
  
  # THE MAGIC STEP: geom_smooth adds a trend line.
  # method = "loess" creates a local polynomial regression (a flexible, curvy line).
  # se = FALSE removes the confidence interval ribbons so the graph stays clean.
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
  facet_wrap(~ party, scales = "free_x") +
  theme_minimal() +
  theme(legend.position = "none") + 
  labs(title = "Narrative Arcs: Sentiment Over the Course of Speeches",
       subtitle = "Lines represent smoothed trend (LOESS) of sentence-level sentiment",
       x = "Sentence Sequence (Start to Finish)",
       y = "Net Sentiment Score")

# INTERPRETATION:
# - Look at the shape of the lines. 
# - A downward slope means the speech grew progressively more pessimistic or angry.
# - Does the governing party (FF) have a fundamentally different arc than the 
#   opposition parties?

# ==============================================================================
# 6. SPATIAL SCALING (Using Document-Level DFM)
# ==============================================================================
# We switch back to the document-level DFM (`dfm_docs`) here. 
# WHY? Spatial scaling algorithms (like Wordscores and Wordfish) need massive 
# "bags of words" to reliably map a politician's ideology. Sentences are too 
# sparse (mostly zeroes), so we analyze full speeches instead.

# ==============================================================================
# 6a. Wordscores (Supervised Spatial Scaling)
# ==============================================================================

# STEP 1: SETTING THE ANCHORS (Reference Scores)
# Wordscores is a supervised model. It needs to be "trained" on known ideological 
# positions so it can predict unknown ones. 
# We start by creating a list of empty scores (NA) for every document.
reference_scores <- rep(NA, ndoc(dfm_docs))

# Now, we manually set the "ground truth" for the two main rivals.
# We assign the Government (Fianna Fáil) a score of 1.
reference_scores[docvars(dfm_docs, "party") == "FF"] <- 1  
# We assign the main Opposition (Fine Gael) a score of -1.
reference_scores[docvars(dfm_docs, "party") == "FG"] <- -1 

# The other parties (Labour, Greens, Sinn Féin) remain NA. These are our 
# "virgin texts". The algorithm will figure out where they belong based on 
# how closely their vocabulary matches the anchors.

# STEP 2: FIT THE MODEL
# The algorithm calculates an ideological score for EVERY SINGLE WORD in the 
# vocabulary based on whether it is used more by the '1' party or the '-1' party.
# smooth = 1: This is "Laplace smoothing." It gently pulls the scores of extremely 
# rare words toward the center (0) so they don't completely derail the math.
ws_model <- textmodel_wordscores(dfm_docs, y = reference_scores, smooth = 1)


# STEP 3: PREDICT THE POSITIONS OF THE UNKNOWN PARTIES
# We now ask the model to look at the vocabulary of the 'NA' parties and place 
# them on the spectrum based on the word scores it just calculated.
#
# CRUCIAL ARGUMENT: rescaling = "lbg" 
# Raw Wordscores suffer from mathematical "shrinkage" (they clump tightly around 0). 
# The "LBG" method (Laver, Benoit, and Garry) stretches the predictions back out 
# onto our original -1 to 1 scale so we can actually interpret and compare them.
ws_predict <- predict(ws_model, se.fit = TRUE, rescaling = "lbg")

# STEP 4: PLOT THE IDEOLOGICAL SPECTRUM
textplot_scale1d(ws_predict, 
                 groups = docvars(dfm_docs, "party"), 
                 margin = "documents") +
  ggtitle("Wordscores: Estimated Ideological Positions (LBG Rescaled)")

# ==============================================================================
# 6b. Wordfish (Unsupervised Spatial Scaling)
# ==============================================================================

# STEP 1: FIT THE MODEL
# Wordfish is completely unsupervised. We do NOT give it anchor scores. 
# It simply looks at the vocabulary and finds the single biggest underlying 
# dimension of disagreement on its own.
#
# However, because the algorithm is blind, it doesn't know which way is 
# "Left" or "Right". 
# It might arbitrarily draw the spectrum backwards. 
# Let's look at the data to decide how to orient the graph:
# Row 6 is Enda Kenny (Opposition Leader). Row 5 is Brian Cowen (Prime Minister).
docvars(dfm_docs)[c(6, 5), ]
# 
# With the 'dir' argument we can force the orientation of the plot 
# We tell it the polarity. We are telling it: "Whatever dimension you find, make sure 
# Document 6 (Opposition Leader) is placed to the left of Document 5 (Government Leader)." 
wf_model <- textmodel_wordfish(dfm_docs, dir = c(6, 5))


# STEP 2: PLOT THE DOCUMENT POSITIONS (Theta)
# This graph shows WHERE each politician falls on the underlying spectrum that 
# the model discovered.
# 
# HOW TO READ IT:
# Notice how Fianna Fáil is firmly on the right, and the Opposition (FG, LAB, SF) 
# are all clustered on the left. The Greens (the junior coalition partner) are 
# caught right in the middle. The algorithm successfully grouped them by their 
# stance on the budget without us ever telling it their party affiliations!
textplot_scale1d(wf_model, 
                 groups = docvars(dfm_docs, "party"), 
                 margin = "documents") +
  ggtitle("Wordfish: Unsupervised Scaling of Party Positions")


# STEP 3: PLOT THE WORD WEIGHTS (The "Eiffel Tower" Plot)
# This graph shows us exactly WHICH words the model used to pull the politicians 
# apart.
# - Y-Axis (Frequency/Psi): High up means the word is used constantly by everyone. 
# - X-Axis (Weight/Beta): This is the political polarization of the word! 
#                         Words pushed far to the left or right are highly 
#                         partisan signals.
textplot_scale1d(wf_model, margin = "features", 
                 highlighted = c("tax", "deficit", "cuts", "families","jobs")) +
  ggtitle("Wordfish: Word Weights (The Eiffel Tower Plot)")


# ==============================================================================
# 6c. THE WORDFISH ILLUSION?: Comparing Anchors
# ==============================================================================
# Let's test the limits of Wordfish by trying to force it to show us two 
# different political dimensions: "Gov vs. Opp" and "Left vs. Right".

library(patchwork) # (Optional) Great for putting plots side-by-side

# ------------------------------------------------------------------------------
# MODEL 1: Government vs. Opposition (as we just did)
# ------------------------------------------------------------------------------
# We know the primary fight in this room is defending vs attacking the budget.
# Left Anchor (Row 6): Enda Kenny (Fine Gael) -> Opposition Leader
# Right Anchor (Row 5): Brian Cowen (Fianna Fáil) -> Prime Minister (Government)

wf_gov_opp <- textmodel_wordfish(dfm_docs, dir = c(6, 5))

plot_gov_opp <- textplot_scale1d(wf_gov_opp, 
                                 groups = docvars(dfm_docs, "party"), 
                                 margin = "documents") +
  ggtitle("Model 1: Gov vs. Opp Anchors", 
          subtitle = "dir = c(6 (Kenny/Opp), 5 (Cowen/Gov))") +
  theme_bw()

plot_gov_opp
# We already generated this graph before
# This plot perfectly captures the political reality. The Government (FF) is on 
# one side, the Opposition is grouped tightly on the other, and the reluctant 
# Green coalition partners are caught in the middle.

# ------------------------------------------------------------------------------
# MODEL 2: Left vs. Right
# ------------------------------------------------------------------------------
# Let's try to force the model to show us traditional economic ideology.
# Left Anchor (Row 4): Arthur Morgan (Sinn Féin) -> Far-Left Party
# Right Anchor (Row 2): Richard Bruton (Fine Gael) -> Center-Right Party

wf_left_right <- textmodel_wordfish(dfm_docs, dir = c(4, 2))

plot_left_right <- textplot_scale1d(wf_left_right, 
                                    groups = docvars(dfm_docs, "party"), 
                                    margin = "documents") +
  ggtitle("Model 2: Left vs. Right Anchors", 
          subtitle = "dir = c(4 (Morgan/Left), 2 (Bruton/Right))") +
  theme_bw()

plot_left_right
# Notice how messy and inverted this looks compared to Model 1. Fianna Fáil is 
# now on the far left! The model did NOT find a new economic dimension.
# We told it to anchor SF on the left and FG on the right. This did not happen.

# ------------------------------------------------------------------------------
# UNDERSTANDING THE WORDFISH ILLUSION
# ------------------------------------------------------------------------------
# 1. Did Model 2 actually find a new "Left vs. Right" dimension? 
#    Answer: No! FF and the Greens did not separate from FG in a meaningful 
#    ideological way. The opposition parties are still clustered together.
# 
# 2. What actually happened? 
#    Because Wordfish only extracts the dimension of *maximum mathematical variance*, 
#    it found the Gov/Opp dimension again. But, to satisfy our strict `dir = c(4, 2)` 
#    rule, it simply took the entire spectrum and flipped it backwards! 
# 
# 3. What does this tell us about unsupervised models? 
#    You cannot force an unsupervised model to find your theoretical dimensions 
#    if the text itself is dominated by a different conflict. In this debate, 
#    "Left vs. Right" vocabulary was completely eclipsed by "Defending vs. Attacking".