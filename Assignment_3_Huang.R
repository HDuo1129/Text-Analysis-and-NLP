## -----------------------------------------------------------------------------
## Title:       Homework 2: Text Cleaning & Pre-processing with manifestoR
## Author:      Huang Duo
## Email:       Huang_Duo@student.ceu.edu
## Course:      DOPP 5688: Text as Data (Spring 2026)
## Description: Iterative cleaning of a UK party manifesto from the 2015 election
## -----------------------------------------------------------------------------

## INSTRUCTIONS:
## Add your name and email above. Rename the file to include your last name.
## Work through each step in order. Each step asks you to:
##   (a) Write code to clean or inspect the data
##   (b) Check whether the cleaning worked
##   (c) Reflect briefly in a comment on what you found
## At the very end (Step 7), you will combine all cleaning steps into one
## single, clean pipeline.
## Submit your completed script on Moodle.

## -----------------------------------------------------------------------------
## BACKGROUND: WHY DOES TEXT NEED CLEANING?
## -----------------------------------------------------------------------------
##
## Raw manifesto text downloaded from the Manifesto Project API contains:
##   - Page numbers and section headers mixed in with real sentences
##   - Very short fragments (single words, punctuation only)
##   - Very long run-on blocks (multiple paragraphs merged together)
##   - Inconsistent whitespace (double spaces, trailing spaces, newlines)
##   - Bullet symbols and other non-ASCII characters
##   - URLs and references (in some parties' documents)
##   - Repeated boilerplate text (e.g. "Labour Party Manifesto 2015")
##
## Each cleaning decision needs to be justified and checked. This homework
## trains you to think iteratively: clean, inspect, clean again.
## -----------------------------------------------------------------------------


## =============================================================================
## STEP 1: LOAD PACKAGES AND SET UP
## =============================================================================

## Load the packages you will need.
## You will need (as always): tidyverse, manifestoR, quanteda
## Add any others you find useful as you work through the homework.

## YOUR CODE HERE:
library(tidyverse)
library(manifestoR)
library(quanteda)

## Set your working directory if needed:
## setwd("~/your/path/here")

## Set your Manifesto Project API key:
## mp_setapikey("path/to/your/apikey.txt")
## OR if you have your key as a string:
## mp_setapikey(key = "YOUR_KEY_HERE")
mp_setapikey("/Users/huangduo/Desktop/NLP/manifesto_apikey.txt")

## =============================================================================
## STEP 2: DOWNLOAD THE RAW MANIFESTO DATA
## =============================================================================

## We will work with ONE manifesto: the UK Labour Party from the 2015 General
## Election. The MARPOR codes you need are:
##   Country: UK        -> country == 51
##   Labour Party       -> party  == 51320
##   Election date      -> edate  == as.Date("2015-05-07")
##
## TASK A: Download the main dataset and filter it to identify the correct
## metadata row for this document. Save it as 'target_meta'.
## Print the result so you can confirm you have exactly 1 row.
## Don't reinvent the wheel here, we have done this before. 

## YOUR CODE HERE:
target_meta <- mp_maindataset() |>
  filter(country == 51,
         party == 51320,
         edate == as.Date("2015-05-07"))

stopifnot(nrow(target_meta) == 1)
print(target_meta |> select(countryname, partyname, edate))

## TASK B: Download the actual manifesto text using mp_corpus().
## Save it as 'raw_docs'.

## YOUR CODE HERE:
raw_docs <- mp_corpus(target_meta)
stopifnot(length(raw_docs) == 1)

## =============================================================================
## STEP 3: EXTRACT RAW TEXT — FIRST LOOK
## =============================================================================

## As we learned in class, ManifestoCorpus objects are awkward to work with
## directly. Use the map_dfr() + content() approach from the Day 5 lab scripts
## to extract the text into a plain tibble called 'df_raw'.
## Your tibble should have at minimum: doc_id, party_code, text columns.
## Again, don't reinvent the wheel. 
## YOUR CODE HERE:
df_raw <- map_dfr(names(raw_docs), function(doc_id) {
  doc <- raw_docs[[doc_id]]
  tibble(
    doc_id     = doc_id,
    party_code = as.integer(str_extract(doc_id, "^[0-9]+")),
    text       = content(doc)
  )
})

## TASK: Inspect the raw data.
## Run each of the following and write a comment after each one describing
## what you observe:

## How many rows (quasi-sentences) do we have?
nrow(df_raw)
## YOUR COMMENT:
## There are 1067 quasi-sentences we have for one manifesto and it covers
## some empty sentences and irrelevant single words
## What does the distribution of text lengths look like?
df_raw %>%
  mutate(nchar = nchar(text)) %>%
  ggplot(aes(x = nchar)) +
  geom_histogram(bins = 80, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Raw Quasi-Sentence Lengths",
       x = "Number of Characters", y = "Count")
## YOUR COMMENT:
## All of the quasi-sentences have lower than 500 characters,
## the main part is around 100 characters
## What do the very shortest entries look like?
## I check them before are `` or -
## Print the 20 shortest entries (by character count):

## YOUR CODE HERE:
df_raw |>
  mutate(nchar = nchar(text)) |>
  slice_min(nchar, n = 20) |>
  select(nchar, text) |>
  print(n = 20)
## YOUR COMMENT:
## the results are similar with before in my memory, and others are
## only have one or two words, some of them are the start, some are
## the end

## What do the very longest entries look like?
## Print the 10 longest entries:

## YOUR CODE HERE:
df_raw |>
  mutate(nchar = nchar(text)) |>
  arrange(desc(nchar)) |>
  select(nchar, text) |>
  print(n = 10)
## YOUR COMMENT:
## There are normal sentence cover all the main information and some
## noise. such as foreword and Contents

## =============================================================================
## STEP 4: ROUND 1 OF CLEANING — LENGTH-BASED FILTERING
## =============================================================================

## Based on your inspection above, you should have identified that very short
## entries (headers, page numbers, single words) and very long entries
## (merged paragraphs, boilerplate blocks) are noise.

## TASK A: Decide on lower and upper character thresholds.
min_chars <- 25
max_chars <- 350
## Write a comment explaining why you chose the values you did.
## YOUR COMMENT ON THRESHOLDS:
## In Step 3, I found that the shorest words all are noise and the two
## longest words also so need to use them as a cut for this step

## TASK B: Create 'df_clean1' by filtering df_raw to only keep entries
## within your chosen length range.

## YOUR CODE HERE:
df_clean1 <- df_raw |>
  mutate(nchar = nchar(text)) |>
  filter(nchar >= min_chars, nchar <= max_chars)

## TASK C: Check your work.
## How many rows did you remove? What proportion of the original data is that?

## YOUR CODE HERE:
removed_round1 <- nrow(df_raw) - nrow(df_clean1)
cat("Rows removed in round 1:", removed_round1,
    sprintf("(%.1f%% of original)\n",
            100 * removed_round1 / nrow(df_raw)))

## YOUR COMMENT:
## I have removed 48 sentences, and it covered 4.5%

## TASK D: Re-plot the character length distribution for df_clean1.
## Does it look better? Are there still obvious outliers?

## YOUR CODE HERE:
df_clean1 |>
  ggplot(aes(x = nchar)) +
  geom_histogram(bins = 60, fill = "darkseagreen", color = "white") +
  theme_minimal() +
  labs(title = "Character Lengths After Round 1 Cleaning",
       x = "Number of Characters", y = "Count")

## YOUR COMMENT:
## the result looks better, but I find that only small number of 
## sentence larger than 250

## =============================================================================
## STEP 5: ROUND 2 OF CLEANING — CONTENT-BASED FILTERING
## =============================================================================

## Length alone is not enough. Even medium-length entries can be noise.
## In this step you will look at the *content* of the text and identify
## patterns worth removing.

## TASK A: Look for entries that are clearly not real manifesto sentences.
## Run the code below, then inspect the output carefully.

df_clean1 %>%
  filter(
    ## entries that are only numbers (page numbers, section numbers)
    str_detect(text, "^[0-9\\s\\.]+$") |
      ## entries that start with a number followed by very few words
      str_detect(text, "^[0-9]+\\.?\\s{0,3}\\w{0,10}$") |
      ## entries in ALL CAPS (usually section headers)
      str_detect(text, "^[A-Z\\s\\-\\']+$")
  ) %>%
  select(text) %>%
  print(n = 30)

## YOUR COMMENT — what types of noise do you see here?
## It returns 0 rows, so from last step, it clean most of the noise
## such as title and numbers. So we can further do next step now

## TASK B: Create 'df_clean2' by adding filters to remove:
##   1. Entries that consist entirely of numbers (optionally with spaces/dots)
##   2. Entries that are entirely upper-case (section headers)
##   3. Any other pattern you identified in Task A
## Write a comment explaining each filter you add.
df_clean2 <- df_clean1 |>
  filter(
    !str_detect(text, "^[0-9\\s\\.]+$"),                                  # numeric-only (page #s)
    !str_detect(text, "^[0-9]+\\.?\\s{0,3}\\w{0,10}$"),                   # short numeric labels
    !str_detect(text, "^[A-Z\\s\\-\\']+$"),                               # ALL-CAPS headings
    !str_detect(str_to_lower(text), "^the labour party manifesto 2015$")  # boilerplate title
  )

## YOUR CODE HERE:
## Four content-based filters: numeric-only entries, short numeric
## labels, ALL-CAPS headings, and the boilerplate title

## TASK C: Check your work.
## How many additional rows did you remove in this round?

## YOUR CODE HERE:
removed_round2 <- nrow(df_clean1) - nrow(df_clean2)
cat("Rows removed in round 2:", removed_round2,
    sprintf("(%.1f%% of df_clean1)\n",
            100 * removed_round2 / nrow(df_clean1)))

## YOUR COMMENT:
## There are 3 more cleaned and covered 0.3% of the sample

## =============================================================================
## STEP 6: ROUND 3 OF CLEANING — STRING-LEVEL CLEANING
## =============================================================================

## Even the entries that pass the content filter may have messy internal
## formatting: extra whitespace, bullet characters, newlines embedded in text,
## etc. In this step you will clean *within* each string rather than removing
## rows entirely.

## TASK A: Investigate whitespace and special characters.
## Run the following lines and examine the output carefully:

## How many entries have leading or trailing whitespace?
df_clean2 %>%
  filter(text != str_squish(text)) %>%
  nrow()
## YOUR COMMENT:
## 10

## How many entries contain a newline character (\n)?
df_clean2 %>%
  filter(str_detect(text, "\n")) %>%
  nrow()
## YOUR COMMENT:
## 0

## How many entries contain non-ASCII characters (bullet points, em-dashes, etc)?
df_clean2 %>%
  filter(str_detect(text, "[^\x01-\x7E]")) %>%
  select(text) %>%
  head(20)

tibble(char = unlist(str_extract_all(df_clean2$text, "[^\x01-\x7E]"))) |>
  count(char, sort = TRUE)

df_clean2 |>
  filter(str_detect(text, "[^\x01-\x7E]")) |>
  nrow()
## YOUR COMMENT:
## 120

## How many entries contain a number embedded in the text
## (e.g. "1.2 million", "3 billion") vs. entries that ARE numbers?
## (Use str_detect and count — no need to remove these, just note them)

## YOUR CODE HERE:
df_clean2 |>
  mutate(category = case_when(
    str_detect(text, "^[0-9\\s\\.]+$") ~ "number_only",
    str_detect(text, "[0-9]")          ~ "embedded_number",
    TRUE                               ~ "no_number"
  )) |>
  count(category, sort = TRUE)
## YOUR COMMENT:
## no_number 918 and embedded_number 98, number only 0

## TASK B: Create 'df_clean3' by applying string-level cleaning.
## Use mutate() to fix the issues you found above. At minimum:
##   1. Apply str_squish() to remove leading, trailing, and double whitespace
##   2. Replace embedded newline characters (\n) with a single space
##   3. Remove or replace non-ASCII characters (use str_replace_all with a regex)
## Add any other cleaning you think is warranted. Comment each step.

## YOUR CODE HERE:
df_clean3 <- df_clean2 |>
  mutate(
    text = str_replace_all(text, "\n+", " "),
    text = str_replace_all(text, "[\u2018\u2019]", "'"),
    text = str_replace_all(text, "[\u201C\u201D]", "\""),
    text = str_replace_all(text, "[\u2013\u2014]", "-"),
    text = str_replace_all(text, "[^\x01-\x7E]", " "),
    text = str_squish(text)
  )

## TASK C: Verify the string-level cleaning worked.
## Re-run the checks from Task A on df_clean3.
## How many entries still have leading/trailing whitespace?
## How many still contain \n?

## YOUR CODE HERE:
df_clean3 |>
  summarise(
    whitespace_issues = sum(text != str_squish(text)),
    newlines          = sum(str_detect(text, "\n")),
    non_ascii         = sum(str_detect(text, "[^\x01-\x7E]"))
  )
## YOUR COMMENT:
## All show 0, which means it is mostly clean now 

## =============================================================================
## STEP 7: ASSESSMENT — WHAT DID WE LOSE AND WHAT DO WE HAVE?
## =============================================================================

## Before writing the final pipeline, take stock of the full cleaning process.

## TASK A: Fill in the table below in comments.
## Original row count (df_raw):
## After Round 1 — length filtering (df_clean1):
## After Round 2 — content filtering (df_clean2):
## After Round 3 — string cleaning (df_clean3, same row count as df_clean2):
## Total rows removed:
tibble(
  stage = c("df_raw (original)",
            "df_clean1 (length filter)",
            "df_clean2 (content filter)",
            "df_clean3 (string cleaning)"),
  rows  = c(nrow(df_raw), nrow(df_clean1), nrow(df_clean2), 
            nrow(df_clean3))
) |>
  mutate(retained_pct = round(100 * rows / nrow(df_raw), 1))

## Percentage of original data retained:
## After Step 1, we still have 95.5%, after step 2, reduce to 95.2%,
## and step 3 keeps stable

## TASK B: Sample 20 random sentences from df_clean3 and print them.
## Read through them. Do they look like real manifesto sentences?
## Note any remaining issues in a comment.

set.seed(1904)
## YOUR CODE HERE:
df_clean3 |>
  slice_sample(n = 20) |>
  pull(text)

## YOUR COMMENT — are there any remaining issues you would fix in a real project?
## the £ is missing in some useful sentence parts, so I think we need to
## do some work on protect such missing symbol problem

## TASK C: Plot the final character length distribution of df_clean3.
## Compare it visually to the original distribution from Step 3.
## What changed?

## YOUR CODE HERE:
bind_rows(
  df_raw    |> mutate(stage = "Raw (df_raw)"),
  df_clean3 |> mutate(stage = "Cleaned (df_clean3)")
) |>
  mutate(nchar = nchar(text),
         stage = factor(stage, levels = c("Raw (df_raw)", "Cleaned (df_clean3)"))) |>
  ggplot(aes(x = nchar, fill = stage)) +
  geom_histogram(bins = 50, position = "identity", alpha = 0.55,
                 color = "white") +
  coord_cartesian(xlim = c(0, 400)) +
  scale_fill_manual(values = c("steelblue", "mediumpurple")) +
  theme_minimal() +
  labs(title = "Character Length Distribution: Before vs After Cleaning",
       subtitle = "UK Labour Party 2015 manifesto. Outliers >400 chars omitted from view.",
       x = "Number of Characters", y = "Count", fill = NULL)

## YOUR COMMENT:
## In this graph, we clear see that in the lower than 50 parts, we
## cut off the most noise and other parts didn't change a lot.


## =============================================================================
## STEP 8: THE FINAL PIPELINE — COMBINING ALL STEPS
## =============================================================================

## Now that you have worked through the cleaning iteratively, write a single
## clean pipeline that goes from the raw ManifestoCorpus all the way to
## a final analysis-ready tibble in one code block.
##
## Your pipeline should:
##   - Start from 'raw_docs' (the ManifestoCorpus object from Step 2)
##   - Use map_dfr() + content() to extract text
##   - Apply ALL cleaning steps from Steps 4, 5, and 6 in a single pipe
##   - Add a 'sentence_index' column (row number within the party, as in the lab)
##   - Save the result as 'df_final'
##   - Print nrow(df_final) and a random sample of 10 sentences to confirm
##
## IMPORTANT: Do NOT copy-paste from your earlier steps one by one.
## Rewrite it as a coherent, well-commented single pipeline.
## A reader should be able to understand every decision from the comments alone.

## YOUR CODE HERE:
min_chars <- 25
max_chars <- 350 

df_final <- map_dfr(names(raw_docs), function(doc_id) {
  doc <- raw_docs[[doc_id]]
  tibble(
    doc_id     = doc_id,
    party_code = as.integer(str_extract(doc_id, "^[0-9]+")),
    text       = content(doc)
  )
}) |>
  ## Step 1
  mutate(nchar = nchar(text)) |>
  filter(nchar >= min_chars, nchar <= max_chars) |>
  select(-nchar) |>
  ## Step 2
  filter(
    !str_detect(text, "^[0-9\\s\\.]+$"),                                  # numeric-only
    !str_detect(text, "^[0-9]+\\.?\\s{0,3}\\w{0,10}$"),                   # short numeric labels
    !str_detect(text, "^[A-Z\\s\\-\\']+$"),                               # ALL-CAPS headings
    !str_detect(str_to_lower(text), "^the labour party manifesto 2015$")  # boilerplate title
  ) |>
  ## Step 3
  mutate(
    text = str_replace_all(text, "\n+", " "),              
    text = str_replace_all(text, "\u00A3", "GBP "),        # £ -> "GBP " (fiscal info)
    text = str_replace_all(text, "\u20AC", "EUR "),        # € -> "EUR "
    text = str_replace_all(text, "[\u2018\u2019]", "'"),
    text = str_replace_all(text, "[\u201C\u201D]", "\""),
    text = str_replace_all(text, "[\u2013\u2014]", "-"),
    text = str_replace_all(text, "[^\x01-\x7E]", " "),
    text = str_squish(text)
  ) |>
  
  group_by(party_code) |>
  mutate(sentence_index = row_number()) |>
  ungroup() |>
  select(doc_id, party_code, sentence_index, text)

cat("Rows in df_final:", nrow(df_final), "\n\n")

set.seed(1904)
df_final |>
  slice_sample(n = 10) |>
  select(sentence_index, text) |>
  print(n = Inf)

## =============================================================================
## STEP 9 (BONUS): VISUALIZE YOUR CLEAN DATA
## =============================================================================

## If you have time, create ONE meaningful visualization of the cleaned data.
## This could be:
##   - A bar chart of the most frequent non-stopword words
##   - A histogram of sentence lengths after cleaning
##   - A word cloud (if you install the 'wordcloud2' package)
##   - Any other visualization that tells you something about this manifesto
##
## Your plot must have a proper title, axis labels, and a clean theme.

## YOUR CODE HERE:


## =============================================================================
## SUBMISSION CHECKLIST
## =============================================================================
## Before submitting, make sure:
## [ ] Your name and email are filled in at the top
## [ ] The filename includes your last name
## [ ] Every task has code written below it
## [ ] Every inspection task has a comment describing what you found
## [ ] Step 8 contains a single coherent pipeline (not copy-pasted fragments)
## [ ] Your script runs from top to bottom without errors
## [ ] Submitted on Moodle before the deadline