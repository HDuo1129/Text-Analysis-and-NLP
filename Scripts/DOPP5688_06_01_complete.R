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

set.seed(1904)
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
# Fetch the corpus directly into a dataframe.
# manifestoR stores texts inherently broken down into quasi-sentences.
# We'll work with those and not full sentences...
# Country: UK, Date: 202407 (July 2024), Party: 51320 (Labour)
# Maybe all of this is useful for you right now?!
labour_df <- mp_corpus_df(countryname == "United Kingdom" & 
                            date == 202407 & 
                            party == 51320)

# Clean and prepare the sentences
labour_sentences <- labour_df |>
  select(text) |>
  # Let's drop empty rows 
  drop_na(text) |>
  # Filter out very short/empty quasi-sentences
  filter(nchar(str_trim(text)) > 15) |> 
  # Taking a sample for demonstration
  slice_sample(n = 20) |> 
  mutate(sentence_id = row_number())

# ------------------------------------------------------------------------------
# 3. Define the LLM Coding Function
# ------------------------------------------------------------------------------
# Understanding the Architecture of this function
# 
# What this function needs to take (Inputs):
#   - sentence_text: A single character string (one manifesto sentence).
#
# What happens inside the function (Process):
#   1. System Prompting: We define a strict constraint (JSON) for the LLM.
#   2. API Call: It passes the sentence and prompt to `tidyllm::llm_message()`.
#   3. Execution: It queries OpenAI's API using `chat(openai(.model = "gpt-5.4"))`.
#      Models are available here: https://developers.openai.com/api/docs/models/all
#      GPT-5.4 is our frontier model for complex professional work. Learn more in our latest model guide. 
#      Reasoning.effort supports: none (default), low, medium, high and xhigh.
#      -> This controls how much "thinking time" the model is allowed to spend 
#         before it starts typing its final answer. Thinking COSTS MONEY!
#      1,050,000 context window
#     -> 1.05 million tokens is roughly 800,000 words. Instead of feeding the manifesto 
#         sentence-by-sentence, we could literally paste the entire UK Labour Manifesto and 
#         the Conservative Manifesto into a single prompt, and it 
#         would remember all of it at once.
#      128,000 max output tokens
#      Aug 31, 2025 knowledge cutoff
#      Reasoning token support
#   4. Extraction: It isolates just the text response from the API object.
#
# What this function returns (Outputs):
#   - A raw JSON string containing the three coded variables.

code_manifesto_sentence <- function(sentence_text) {
  
  sys_prompt <- "You are an expert political science research assistant. 
  Read the provided manifesto sentence and return ONLY a valid JSON object 
  with the following exactly three keys:
  - 'content': A concise 2-5 word description of the core policy issue.
  - 'sentiment': The tone of the sentence ('Positive', 'Neutral', or 'Negative').
  - 'ideology': The ideological leaning ('Left', 'Center', 'Right', or 'None')."
  
  # Send the message to OpenAI using tidyllm with gpt-5.4 hyperparameter
  response_obj <- llm_message(sentence_text, .system_prompt = sys_prompt) |>
    chat(
      openai(
        .model = "gpt-5.4",                     # Using a newish model here
        .reasoning_effort = "medium"            # Force it to think somewhar deeply
                                                # none (default), low, medium, high and xhigh
      )
    )
  
  # Extract the text reply from the tidyllm LLMMessage object
  reply_text <- get_reply(response_obj)
  
  return(reply_text)
}


# ------------------------------------------------------------------------------
# 4. Execute the Coding Pipeline
# ------------------------------------------------------------------------------
# HOLD UP
# Let's check things first!
#test_sentence <- "We will invest heavily in green energy to create new jobs."
#code_manifesto_sentence(test_sentence)

# Map the coding function over our manifesto sentences.
# We wrap it in purrr::possibly() to prevent the pipeline from failing 
# if the API times out or throws an error on a single sentence.
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
# Convert the raw JSON strings returned by the LLM into tidy columns
final_df <- coded_results |>
  mutate(
    parsed_data = map(llm_raw_json, ~ {
      # Handle cases where the API failed and returned NA
      if (is.na(.x)) return(tibble(content = NA, sentiment = NA, ideology = NA))
      
      # Safely parse the JSON string
      tryCatch({
        # Clean up any potential markdown formatting the LLM might have added
        clean_json <- str_remove_all(.x, "```json|
```") |> str_trim()
        fromJSON(clean_json) |> as_tibble()
      }, error = function(e) tibble(content = "Error", sentiment = "Error", ideology = "Error"))
    })
  ) |>
  unnest(parsed_data) |>
  select(sentence_id, text, content, sentiment, ideology)

# View the final structured dataset
print(final_df)
