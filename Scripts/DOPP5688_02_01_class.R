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
setwd("Desktop/NLP/")
getwd()
list.files()
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
df_csv <- read.csv("Data/labour_2024.csv") |>
  mutate(section = ifelse(cmp_code == "H", text, NA)) |>
  fill(section, .direction = "down")

# ------------------------------------------------------------------------------
# 2. READ FROM A PLAIN TEXT FILE (.txt)
# ------------------------------------------------------------------------------
project
 - data
 - scripts
 - figures
 - tables
 - groveyord

txt_path <- "Data/Labour-Party-manifesto-2024.txt"

raw_text <- readLines(txt_path, warn = FALSE)

df_txt <- data.frame(
  line_number = 1:length(raw_text),
  text = raw_text,
  stringsAsFactors = F
)
#通过逐行读取获得结果

#clean the txt data
df_txt_clean  <- 
  df_txt |> #filter out what we dont need
  filter(text !=  "", #remove empty
         !str_detect(text, "^-{5,}$"))

rm(raw_text, txt_path, df_txt_clean)

# ------------------------------------------------------------------------------
# 3. READ FROM A WORD DOCUMENT (.docx)
# ------------------------------------------------------------------------------
docx_path <- "Data/labour_2024.docx"
#officer object to load the file
doc_object <- read_docx(docx_path)
df_docx <-
  docx_summary(doc_object)

# ------------------------------------------------------------------------------
# 4. READ VIA API (The Manifesto Project)
# ------------------------------------------------------------------------------
# APIs let computers talk directly to databases. You need a "key" to enter.
# IMPORTANT: YOUR API KEY IS LIKE YOUR BANK PASSWORD. Never share it anywhere!
mp_setapikey(key = "0fd298978c5ef2b5cce81caacfc2bcbf")

# --- Example A: UK 2024 Labour ---
wanted_labor24 <- data.frame(
  party = 51421,
  date = 202407
)

df_api_labor_24 <- mp_corpus_df(wanted_labor24)

# --- Example B: Labour History (2015, 2017, 2019) ---
wanted_labor_hist <- data.frame(
  party = rep(51421, 3),
  date = c(201505, 201706, 201912)
)

df_api_labor_hist <- mp_corpus_df(wanted_labor_hist)

# --- Example C: Multiple Parties in 2024 ---
wanted_uk_2024 <- data.frame(
  party = c(51421, 51620, 51320),
  date = rep(202407,3)
)

df_api_uk <- mp_corpus_df(wanted_uk_2024)

write_csv(df_api_uk, "Data/name.csv") # how to save as document

# ------------------------------------------------------------------------------
# 5. WEB SCRAPING (Reading directly from websites)
# ------------------------------------------------------------------------------
# The manifesto is available on this website https://labour.org.uk/change/
# You can see a very determined Keith Starmer and the text on the webpage
# --- Step 1: Proof of Concept (Scraping a single page) ---
url_1 <- "https://labour.org.uk/change/my-plan-for-change/"

page_html <- read_html(url_1)

single_page_text <-
  page_html |>
  html_element("p") |>
  html_text(trim = TRUE)

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

scrape_page <- function(url){
  page <- read_html(url)
  
  extracted_text <- page |>
    html_element("p") |>
    html_text(trim = TRUE)
  
  data.frame(
    url = url,
    text = extracted_text,
    stringsAsFactors = FALSE
  )
}

df_scrape_raw <- map_df(urls, scrape_page)

# ------------------------------------------------------------------------------
# 6. PDF FORMAT: THE EASY WAY (Standard Single-Column PDFs)
# ------------------------------------------------------------------------------
pdf_path_easy <- "Data/Change-Labour-Party-Manifesto-2024-large-print.pdf"
raw_pdf_text <- pdf_text(pdf_path_easy)

df_pdf_easy_paragraph <- 
  raw_pdf_text |>
  enframe(name = "page", value = "text") |>
  mutate(text = str_split(text, "\n{2,}")) |>
  unnest(text) |>
  mutate(text = str_squish(text)) |>
  filter(
    text != "",
    text != "Change Labour Party Manifesto 2024",
    as.character(page) != text,
    !str_detect(text, "^[0-9]+$")
  )

# ------------------------------------------------------------------------------
# 7. PDF FORMAT: THE HARD WAY (Multi-Column Layouts)
# ------------------------------------------------------------------------------
# Standard pdf_text() reads directly across the page. If a PDF has two columns, 
# it will read the first line of the left column directly into the first line of 
# the right column, creating a franken-sentence. 
# To fix this, we use spatial data to map where the words physically are!

pdf_path_hard <- "Data/Labour-Party-manifesto-2024.pdf"
raw_special_data <- pdf_text(pdf_path_hard)

extract_colums <- function(page_df, page_num){
  if (nrow(page_df) == 0) return(tibble())
  
  midpoint <- max(page_df$x) / 2
  
  page_df |>
    mutate(
      column = if_else(x < midpoint)
    )
}
