## -----------------------------------------------------------------------------
## Title:       Homework 2: Text Cleaning & Pre-processing with manifestoR
## Author:      
## Email:
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


## Set your working directory if needed:
## setwd("~/your/path/here")

## Set your Manifesto Project API key:
## mp_setapikey("path/to/your/apikey.txt")
## OR if you have your key as a string:
## mp_setapikey(key = "YOUR_KEY_HERE")


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


## TASK B: Download the actual manifesto text using mp_corpus().
## Save it as 'raw_docs'.

## YOUR CODE HERE:


## =============================================================================
## STEP 3: EXTRACT RAW TEXT — FIRST LOOK
## =============================================================================

## As we learned in class, ManifestoCorpus objects are awkward to work with
## directly. Use the map_dfr() + content() approach from the Day 5 lab scripts
## to extract the text into a plain tibble called 'df_raw'.
## Your tibble should have at minimum: doc_id, party_code, text columns.
## Again, don't reinvent the wheel. 
## YOUR CODE HERE:


## TASK: Inspect the raw data.
## Run each of the following and write a comment after each one describing
## what you observe:

## How many rows (quasi-sentences) do we have?

## YOUR COMMENT:

## What does the distribution of text lengths look like?
df_raw %>%
  mutate(nchar = nchar(text)) %>%
  ggplot(aes(x = nchar)) +
  geom_histogram(bins = 80, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Raw Quasi-Sentence Lengths",
       x = "Number of Characters", y = "Count")
## YOUR COMMENT:

## What do the very shortest entries look like?
## Print the 20 shortest entries (by character count):

## YOUR CODE HERE:

## YOUR COMMENT:

## What do the very longest entries look like?
## Print the 10 longest entries:

## YOUR CODE HERE:

## YOUR COMMENT:


## =============================================================================
## STEP 4: ROUND 1 OF CLEANING — LENGTH-BASED FILTERING
## =============================================================================

## Based on your inspection above, you should have identified that very short
## entries (headers, page numbers, single words) and very long entries
## (merged paragraphs, boilerplate blocks) are noise.

## TASK A: Decide on lower and upper character thresholds.
## Write a comment explaining why you chose the values you did.
## YOUR COMMENT ON THRESHOLDS:

## TASK B: Create 'df_clean1' by filtering df_raw to only keep entries
## within your chosen length range.

## YOUR CODE HERE:


## TASK C: Check your work.
## How many rows did you remove? What proportion of the original data is that?

## YOUR CODE HERE:

## YOUR COMMENT:

## TASK D: Re-plot the character length distribution for df_clean1.
## Does it look better? Are there still obvious outliers?

## YOUR CODE HERE:

## YOUR COMMENT:


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


## TASK B: Create 'df_clean2' by adding filters to remove:
##   1. Entries that consist entirely of numbers (optionally with spaces/dots)
##   2. Entries that are entirely upper-case (section headers)
##   3. Any other pattern you identified in Task A
## Write a comment explaining each filter you add.

## YOUR CODE HERE:


## TASK C: Check your work.
## How many additional rows did you remove in this round?

## YOUR CODE HERE:

## YOUR COMMENT:


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

## How many entries contain a newline character (\n)?
df_clean2 %>%
  filter(str_detect(text, "\n")) %>%
  nrow()
## YOUR COMMENT:

## How many entries contain non-ASCII characters (bullet points, em-dashes, etc)?
df_clean2 %>%
  filter(str_detect(text, "[^\x01-\x7E]")) %>%
  select(text) %>%
  head(20)
## YOUR COMMENT:

## How many entries contain a number embedded in the text
## (e.g. "1.2 million", "3 billion") vs. entries that ARE numbers?
## (Use str_detect and count — no need to remove these, just note them)

## YOUR CODE HERE:

## YOUR COMMENT:


## TASK B: Create 'df_clean3' by applying string-level cleaning.
## Use mutate() to fix the issues you found above. At minimum:
##   1. Apply str_squish() to remove leading, trailing, and double whitespace
##   2. Replace embedded newline characters (\n) with a single space
##   3. Remove or replace non-ASCII characters (use str_replace_all with a regex)
## Add any other cleaning you think is warranted. Comment each step.

## YOUR CODE HERE:


## TASK C: Verify the string-level cleaning worked.
## Re-run the checks from Task A on df_clean3.
## How many entries still have leading/trailing whitespace?
## How many still contain \n?

## YOUR CODE HERE:

## YOUR COMMENT:


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
## Percentage of original data retained:

## TASK B: Sample 20 random sentences from df_clean3 and print them.
## Read through them. Do they look like real manifesto sentences?
## Note any remaining issues in a comment.

set.seed(1904)
## YOUR CODE HERE:

## YOUR COMMENT — are there any remaining issues you would fix in a real project?


## TASK C: Plot the final character length distribution of df_clean3.
## Compare it visually to the original distribution from Step 3.
## What changed?

## YOUR CODE HERE:

## YOUR COMMENT:


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