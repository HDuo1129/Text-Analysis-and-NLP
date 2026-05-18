## -----------------------------------------------------------------------------
## Title:       Day 2 - Acquiring Text Data 
## Course:      DOPP 5688: Text as Data (Spring 2026)
## Author:      Daniel Weitzel
## Email:       weitzeld@ceu.edu
## Institution: Central European University
## Description: An absolute beginner's guide to acquiring text data 
##              This script demonstrates how to import text data from multiple 
##              sources (CSV, TXT, DOCX, APIs, Web Scraping, and PDFs) and 
##              clean them into standardized, tidy data frames.
## -----------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
# Set your working directory to the folder containing your 'data' folder.
setwd("~/Dropbox/University/CEU/Teaching/DOPP5688 - NLP/")

# Load necessary libraries
library(tidyverse)  # For data wrangling (dplyr, stringr, purrr) and reading CSVs
library(manifestoR) # For accessing the Manifesto Project API
library(rvest)      # For web scraping
library(officer)    # For reading Microsoft Word documents
library(pdftools)   # For reading PDF files

# ------------------------------------------------------------------------------
# 1. READ FROM A CSV (The Easiest Way)
# ------------------------------------------------------------------------------
# CSVs are already structured as data frames, so this is just one step.
df_csv <- read_csv("data/labour_2024.csv")


# ------------------------------------------------------------------------------
# 2. READ FROM A PLAIN TEXT FILE (.txt)
# ------------------------------------------------------------------------------
txt_path <- "data/Labour-Party-manifesto-2024.txt"

# readLines() reads the file line by line. 
# 'warn = FALSE' hides the annoying warning if the file doesn't end with an empty new line.
raw_txt <- readLines(txt_path, warn = FALSE)

# Convert the raw text vector into a proper data frame
df_txt <- data.frame(
  line_number = 1:length(raw_txt), 
  text = raw_txt,
  stringsAsFactors = FALSE
)

# Clean the text data using modern Tidyverse (dplyr + stringr)
df_txt_clean <- df_txt |> 
  # Filter out rows we don't need
  filter(
    text != "", # Remove perfectly empty rows
    # Remove rows that are just decorative dashes (e.g., "----------")
    # "^-{5,}$" is a Regular Expression (Regex) meaning:
    # ^ (start) followed by - (dash) at least 5 times {5,}, followed by $ (end)
    !str_detect(text, "^-{5,}$") 
  )

rm(txt_path, raw_txt)

# ------------------------------------------------------------------------------
# 3. READ FROM A WORD DOCUMENT (.docx)
# ------------------------------------------------------------------------------
docx_path <- "data/labour_2024.docx"

# First, we read the document into a special 'officer' object
doc_object <- read_docx(docx_path)

# Then, we extract the content into a data frame.
# This automatically separates paragraphs, tables, and headers!
df_docx <- docx_summary(doc_object) |> 
  as_tibble() # Convert to a modern tidyverse dataframe

rm(docx_path, doc_object)

# ------------------------------------------------------------------------------
# 4. READ VIA API (The Manifesto Project)
# ------------------------------------------------------------------------------
# APIs let computers talk directly to databases. You need a "key" to enter.
# IMPORTANT: YOUR API KEY IS LIKE YOUR BANK PASSWORD. Never share it anywhere!
mp_setapikey("~/Dropbox/Research/R/manifesto_apikey.txt")

# --- Example A: UK 2024 Labour ---
# We create a small dataframe to tell the API exactly what we want
wanted_labour_24 <- data.frame(
  party = 51421,   # The unique ID for UK Labour
  date  = 202407   # Format is YYYYMM (July 2024 election)
)

# Fetch the data
df_api_labour_24 <- mp_corpus_df(wanted_labour_24)

# Wrangle the dates to make them usable for time-series analysis
df_api_labour_24_clean <- 
  df_api_labour_24 |> 
  mutate(
    date_full = ymd(paste0(date, "01")), # Paste "01" to make it a full YYYY-MM-DD
    year = year(date_full),
    party_name = "Labour"
  )

# --- Example B: Labour History (2015, 2017, 2019) ---
wanted_labour_hist <- data.frame(
  party = rep(51421, 3), # Repeat the Labour ID 3 times
  date  = c(201505, 201709, 201912)
)

df_api_labour_hist <- mp_corpus_df(wanted_labour_hist) |> 
  mutate(
    date_full = ymd(paste0(date, "01")),
    year = year(date_full),
    party_name = "Labour"
  )

# --- Example C: Multiple Parties in 2024 ---
wanted_uk_24 <- data.frame(
  party = c(51421, 51620, 51320), # Labour, Conservatives, Lib Dems
  date  = rep(202407, 3) 
)

df_api_uk_24 <- mp_corpus_df(wanted_uk_24) |> 
  mutate(
    date_full = ymd(paste0(date, "01")),
    year = year(date_full),
    # case_when is a powerful alternative to writing multiple if/else statements!
    party_name = case_when(
      party == 51421 ~ "Labour",
      party == 51620 ~ "Conservatives",
      party == 51320 ~ "Lib Dems", 
      .default = NA_character_
    )
  )

rm(wanted_labour_24, df_api_labour_24,
   wanted_labour_hist, wanted_uk_24)
# ------------------------------------------------------------------------------
# 5. WEB SCRAPING (Reading directly from websites)
# ------------------------------------------------------------------------------
# The manifesto is available on this website https://labour.org.uk/change/
# You can see a very determined Keith Starmer and the text on the webpage
# --- Step 1: Proof of Concept (Scraping a single page) ---
url_1 <- "https://labour.org.uk/change/my-plan-for-change/"

# read in the html
page_html <- read_html(url_1)

# Extract just the paragraph text
single_page_text <- page_html |> 
  html_elements("p") |>  # Find all <p> (paragraph) tags in the HTML
  html_text(trim = TRUE) # Extract the text inside those tags and trim spaces

df_single_scrape <- as_tibble(single_page_text) 
# Tip: If the website has a footer you want to ignore, you can drop the last 
# few rows using slice(). E.g., slice(1:(n() - 5)) drops the last 5 rows.

rm(url_1, page_html, single_page_text)

# --- Step 2: The Smarter Way (Scraping multiple pages at once) ---
# Combine all the URLs we want to scrape into one vector
urls <- c(
  "https://labour.org.uk/change/my-plan-for-change/",
  "https://labour.org.uk/change/mission-driven-government/",
  "https://labour.org.uk/change/strong-foundations/",
  "https://labour.org.uk/change/kickstart-economic-growth/",
  "https://labour.org.uk/change/make-britain-a-clean-energy-superpower/",
  "https://labour.org.uk/change/take-back-our-streets/",
  "https://labour.org.uk/change/break-down-barriers-to-opportunity/",
  "https://labour.org.uk/change/build-an-nhs-fit-for-the-future/",
  "https://labour.org.uk/change/serving-the-country/",
  "https://labour.org.uk/change/britain-reconnected/",
  "https://labour.org.uk/change/labours-fiscal-plan/"
)

# Write a custom function that repeats our proof-of-concept steps for ANY url
scrape_page <- function(url) {
  page <- read_html(url)
  
  extracted_text <- page |> 
    html_elements("p") |> 
    html_text(trim = TRUE)
  
  # Return a dataframe containing the URL and the text
  data.frame(
    url = url,
    text = extracted_text,
    stringsAsFactors = FALSE
  )
}

# map_dfr runs our 'scrape_page' function on every URL in our list, 
# and binds the results together into one master dataframe. Super fast!
df_scrape_raw <- map_dfr(urls, scrape_page)

# Clean up the scraped text
df_scrape_clean <- df_scrape_raw |> 
  # If a paragraph has a hidden newline (\n), this splits it into two separate rows
  separate_rows(text, sep = "\n") |> 
  filter(text != "") |> 
  mutate(
    text = str_squish(text), # str_squish removes excess spaces inside sentences
    # Extract the chapter name directly from the URL to use as metadata
    chapter = str_remove_all(url, "https://labour.org.uk/change/"),
    chapter = str_remove_all(chapter, "/")
  )


# ------------------------------------------------------------------------------
# 6. PDF FORMAT: THE EASY WAY (Standard Single-Column PDFs)
# ------------------------------------------------------------------------------
pdf_path_easy <- "data/Change-Labour-Party-Manifesto-2024-large-print.pdf"

# pdf_text() returns a character vector where each element represents ONE PAGE
raw_pdf_text <- pdf_text(pdf_path_easy)

# We want a dataframe where each row is a PARAGRAPH.
df_pdf_easy_paragraphs <- raw_pdf_text |> 
  enframe(name = "page", value = "text") |> 
  # 1. Split text blocks by TWO OR MORE newlines (\n{2,}) to find true paragraphs
  mutate(text = str_split(text, "\n{2,}")) |>  
  unnest(text) |> 
  # 2. Replace any remaining single newlines inside the paragraph with a space
  mutate(text = str_replace_all(text, "\n", " ")) |> 
  # 3. Remove excess internal spaces
  mutate(text = str_squish(text)) |> 
  # 4. Apply our trusty filters
  filter(
    text != "",                                  
    !str_detect(text, "-{5,}"),                  
    !str_detect(text, "^[0-9]+$"),               # Remove standalone page numbers
    text != "Change Labour Party Manifesto 2024" # Remove repetitive headers
  )


# ------------------------------------------------------------------------------
# 7. PDF FORMAT: THE HARD WAY (Multi-Column Layouts)
# ------------------------------------------------------------------------------
# Standard pdf_text() reads directly across the page. If a PDF has two columns, 
# it will read the first line of the left column directly into the first line of 
# the right column, creating a franken-sentence. 
# To fix this, we use spatial data to map where the words physically are!

pdf_path_hard <- "data/Labour-Party-manifesto-2024.pdf"

# pdf_data() extracts the exact X and Y coordinates for every single word
raw_spatial_data <- pdf_data(pdf_path_hard)

# Create a custom function to untangle the columns on a single page using math
extract_columns <- function(page_df, page_num) {
  
  # Skip perfectly empty pages (like covers or blank inserts)
  if (nrow(page_df) == 0) return(tibble())
  
  # Find the horizontal center of the page
  midpoint <- max(page_df$x) / 2
  
  page_df |> 
    mutate(
      # Assign each word to Column 1 (Left) or Column 2 (Right) based on the midpoint
      column = if_else(x < midpoint, 1, 2),
      
      # Group words into lines by rounding the Y (vertical) coordinate.
      # We round by 5 pixels because letters on the same line can slightly differ in height.
      line_y = round(y / 5) * 5
    ) |> 
    # Sort logically: Read Column 1 top-to-bottom, then Column 2 top-to-bottom
    arrange(column, line_y, x) |> 
    # Paste words on the same line together
    group_by(column, line_y) |> 
    summarise(text = paste(text, collapse = " "), .groups = "drop") |> 
    # Attach the page number so we don't lose it
    mutate(page = page_num) |> 
    select(page, text)
}

# Apply our spatial function to every page in the PDF list
# (imap_dfr loops through the list: .x is the data, .y is the page number index)
df_pdf_hard_raw <- imap_dfr(raw_spatial_data, ~extract_columns(.x, .y))

# Apply your standard cleaning filters
df_pdf_hard_clean <- df_pdf_hard_raw |> 
  mutate(text = str_trim(text)) |> 
  filter(
    text != "",                                   
    !str_detect(text, "-{5,}"),                   
    !str_detect(text, "^[0-9]+$"),                
    !str_detect(text, "^Change$")                 
  )

# Preview the magic
head(df_pdf_hard_clean, 15)
