library(rvest)
library(tidyverse)
library(tidytext)

url <- "https://www.chinadaily.com.cn/a/202604/25/WS69ebfa11a310d6866eb4578f.html"
page <- read_html(url)

text_data <- page |>
  html_elements("p") |>
  html_text2()

# Extract paragraph text
df_text <- tibble(
  paragraph_id = 1:length(text_data),
  text = text_data)

# Text Cleaning
df_text_clean <- df_text |>
  filter(text != "") |>
  mutate(
    text = str_squish(text),
    text_lower = str_to_lower(text))

head(df_text_clean)

# Summary
num_paragraphs <- nrow(df_text_clean)
total_words <- sum(str_count(df_text_clean$text_lower, "\\S+"))
print(paste("Number of paragraphs:", num_paragraphs))
print(paste("Total number of words:", total_words))

# Take text into word
df_words <- df_text_clean |>
  select(paragraph_id, text_lower) |>
  unnest_tokens(word, text_lower)

data("stop_words")

df_words_clean <- df_words |>
  anti_join(stop_words, by = "word")

# Frequent words
word_frequency <- df_words_clean |>
  count(word, sort = TRUE)

# Result
head(word_frequency, 15)

