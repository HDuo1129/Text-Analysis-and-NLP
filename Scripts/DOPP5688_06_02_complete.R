## -----------------------------------------------------------------------------
## Title:       Day 6 Applied Lab: Text Analysis with manifestoR and tidyllm (OpenAI/ChatGPT)
## Course:      DOPP 5688: Text as Data (Spring 2026)
## Author:      Daniel Weitzel
## Email:       weitzeld@ceu.edu
## Institution: Central European University
## Description: Demonstrating LLM Coding of the UK Labour 2024 Manifesto
## -----------------------------------------------------------------------------

library(tidyverse)
library(manifestoR)
library(tidyllm)
library(jsonlite)

# ------------------------------------------------------------------------------
# 1. Setup and Authenticate
# ------------------------------------------------------------------------------
# Set your Manifesto Project API key (required to download data)
mp_setapikey("~/Dropbox/Research/R/manifesto_apikey.txt")

# Ensure your OpenAI API key is set for tidyllm to use ChatGPT
source("~/Dropbox/Research/R/openai_key.R")
# This line of code calls an R script that executes this code: 
# Sys.setenv(OPENAI_API_KEY = "your-key-here")

# ------------------------------------------------------------------------------
# 2. Fetch and Prepare the Data
# ------------------------------------------------------------------------------
labour_df <- mp_corpus_df(countryname == "United Kingdom" & 
                            date == 202407 & 
                            party == 51320)

labour_sentences <- labour_df |>
  select(text) |>
  drop_na(text) |>
  filter(nchar(str_trim(text)) > 15) |> 
  slice_sample(n = 20) |> 
  mutate(sentence_id = row_number())

# ------------------------------------------------------------------------------
# 3. Define the Explicit LLM Coding Function
# ------------------------------------------------------------------------------
code_manifesto_sentence <- function(sentence_text) {
  
  sys_prompt <- "You are an expert political science research assistant coding UK manifestos from the 2024 General Election.
  Read the provided sentence and return ONLY a valid JSON object with exactly these 5 keys:
  
  1. 'content': You MUST strictly classify the text into ONE of these 10 categories. If none fit, use 'Other':
     - 'NHS & Healthcare'
     - 'Economy & Inflation'
     - 'Immigration & Borders'
     - 'Climate Change & Green Energy'
     - 'Housing & Planning'
     - 'Education & Schools'
     - 'Taxation & Fiscal Policy'
     - 'Crime & Policing'
     - 'Defense & Foreign Policy'
     - 'Welfare & Pensions'
  2. 'sentiment': 'Positive', 'Neutral', or 'Negative'.
  3. 'ideology': 'Left', 'Center', 'Right', or 'None'.
  4. 'politicians': A comma-separated string of any 2024 political figures mentioned (e.g., Keir Starmer, Rishi Sunak, Ed Davey, Nigel Farage, Rachel Reeves). If none are mentioned, return 'None'.
  5. 'parties': A comma-separated string of any political parties mentioned (e.g., Labour, Conservative, Liberal Democrats, Reform UK, Green Party, SNP). If none are mentioned, return 'None'."
  
  # Send the message to OpenAI using tidyllm with gpt-5.4
  response_obj <- llm_message(sentence_text, .system_prompt = sys_prompt) |>
    chat(
      openai(
        .model = "gpt-5.4",                     
        .reasoning_effort = "medium"      
        )
    )
  
  # Pull the text reply
  reply_text <- get_reply(response_obj)
  
  return(reply_text)
}

# ------------------------------------------------------------------------------
# 4. Execute the Coding Pipeline
# ------------------------------------------------------------------------------
# HOLD UP
# Let's check things first!
test_sentence <- "Under the leadership of Keir we at Labour will invest heavily in green energy to create more jobs than Reform."
code_manifesto_sentence(test_sentence)


coded_results <- labour_sentences |>
  mutate(
    llm_raw_json = map_chr(
      text, 
      possibly(code_manifesto_sentence, otherwise = NA_character_),
      .progress = "Coding sentences with ChatGPT"
    )
  )

# ------------------------------------------------------------------------------
# 5. Parse JSON and Finalize Output
# ------------------------------------------------------------------------------
# Define a fallback row structure in case JSON parsing fails
error_fallback <- tibble(
  content = "Error", 
  sentiment = "Error", 
  ideology = "Error", 
  politicians = "Error", 
  parties = "Error"
)

final_df <- coded_results |>
  mutate(
    parsed_data = map(llm_raw_json, ~ {
      if (is.na(.x)) {
        return(tibble(content = NA, sentiment = NA, ideology = NA, politicians = NA, parties = NA))
      }
      
      tryCatch({
        clean_json <- str_remove_all(.x, "```json|
```") |> str_trim()
        fromJSON(clean_json) |> as_tibble()
      }, error = function(e) error_fallback)
    })
  ) |>
  unnest(parsed_data) |>
  select(sentence_id, text, content, sentiment, ideology, politicians, parties)

# View the structured results
print(final_df)