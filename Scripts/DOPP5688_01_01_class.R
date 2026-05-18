## -----------------------------------------------------------------------------
## Title:       Day 1 Applied Lab: Foundations of R & Pre-processing
## Course:      DOPP 5688: Text as Data (Spring 2026)
## Author:      Daniel Weitzel
## Email:       weitzeld@ceu.edu
## Institution: Central European University
## Description: An absolute beginner's guide to R, Tidyverse, and Regex
## -----------------------------------------------------------------------------

## =============================================================================
## PART 1: ORIENTATION & HOW TO EXECUTE CODE
## =============================================================================

## Welcome to RStudio! 
## Lines that start with a hashtag (#) are "comments." The computer ignores them.
## We use comments to explain the "why" behind our code to other humans.

## RStudio is divided into panes:
## 1. Source (Top-Left): Where you write and save your scripts (this pane!).
## 2. Console (Bottom-Left): Where the code actually runs and prints output.
## 3. Environment (Top-Right): Your computer's short-term memory. It shows saved data.
## 4. Files/Plots (Bottom-Right): Where you can see your folder structure and graphs.

## HOW TO RUN CODE:
## Put your cursor on line 29 below and press:
## - Mac: Command + Return
## - Windows: Ctrl + Enter

print("Welcome to Text as Data at CEU!")


## =============================================================================
## PART 2: R AS A CALCULATOR & THE ASSIGNMENT OPERATOR
## =============================================================================

## At its most basic, R is a powerful calculator:

## But we rarely just do math; we want to save our results into the computer's memory.
## To do this, we use the assignment operator: <-
## Think of this as an arrow saying: "Take what is on the right, and save it 
## into an object named on the left."


## Look at your Environment pane (top-right). You will see these objects saved!
## Now we can use the names instead of the numbers:



## =============================================================================
## PART 3: PACKAGES AND THE TIDYVERSE
## =============================================================================

## Base R can do a lot, but the global R community has written thousands of 
## "packages" (add-ons) that make data analysis much easier.
## The most important package for us is the "tidyverse", a collection of tools
## for data manipulation and visualization.

## CRITICAL RULE FOR PACKAGES:
## 1. You INSTALL a package exactly ONCE per computer (like buying a book).
## 2. You LOAD a package EVERY TIME you open RStudio (like opening the book to read).

## Step 1: Install packages 
## (If this is your first time, remove the # on the line below and run it. 
## It might take a minute or two to download everything - DON'T DO THIS IN CLASS!
# install.packages(c("tidyverse", "quanteda", "quanteda.textplots", "pdftools", "tesseract", "lexicon", "textreadr"))
install.packages(c("tidyverse", "quanteda", "quanteda.textplots", "pdftools", "tesseract", "lexicon", "textreadr"))


## Step 2: Load the packages (Run these lines every time you start working)
library(tidyverse)


## =============================================================================
## PART 4: GENERATING AND INSPECTING DATA
## =============================================================================

## Let's generate some fake, messy administrative data that we can practice cleaning.
## We are creating a "tibble" (a modern data frame/spreadsheet) with 3 columns.
df_city <- tibble(
  regions = c(rep("Vienna", 10), rep(" Vienna", 4), rep(" Vienna ", 6),
              rep("vienna", 10), rep(" vienna", 4), rep(" vienna ", 7),
              rep("wien", 1), rep("ViennXX", 1), rep("Bienna",1),
              rep("vienna city", 1), rep("vienn", 1)),
  number  = sample(1:100, 46),
  country = "Austria",
  code    = c(rep("BM34", 14), rep("BM32", 10), rep("BM22", 10), rep("XM34", 10),
              rep("XM22", 2))
)

## Let's inspect our data. 
## Notice any issues? The names are messy, capitalization is inconsistent, 
## and there are weird spaces, typos (Bienna), and German names mixed in (wien).



## =============================================================================
## PART 5: CLEANING TEXT DATA (INTRODUCTION TO REGEX)
## =============================================================================

## Before we write the code, let's review the tools in our Tidyverse toolbox.
## In R, we use functions as "verbs" to tell the computer what to do. 
## Here are the functions you will need for this exercise:

## 1. mutate() 
## This is how we change our data. It tells R: "I want to modify an existing 
## column or create a brand new one."

## 2. str_squish()
## A lifesaver for messy administrative data. It removes all spaces at the very 
## beginning and very end of a string, and reduces any double spaces in the 
## middle down to a single space.

## 3. str_remove_all(string, pattern)
## Looks at your text and entirely deletes every instance of the pattern you specify.

## 4. str_replace(string, pattern, replacement)
## Looks for a specific pattern and replaces it with a new string. 
## (e.g., replacing a typo with the correct letter).

## 5. ifelse(TEST, DO THIS IF TRUE, DO THIS IF FALSE)
## This applies logical rules. Example: ifelse(weather == "rain", "umbrella", "sunglasses")

## 6. %in% c("item1", "item2")
## A shortcut for checking multiple conditions. It asks: "Is this value inside 
## this specific list of items?"

## 7. str_to_lower(), str_to_upper(), str_to_title()
## Functions that standardize the capitalization of your text. 
## Title case capitalizes the first letter of every word.
## The %>% is called the "pipe". It means "AND THEN".
## Read the code below as: "Take df_city, AND THEN mutate (change) the regions column by..."

table(df_city$regions)
table(df_city_new$regions2)

df_city_new <- df_city %>%
  mutate(
    # str_squish removes annoying white spaces at the start and end of text
    regions = str_squish(df_city$regions),
    # str_remove deletes specific characters (here, upper or lower case X)
    regions = str_remove_all(regions, ("x | X")),
    # str_replace fixes specific typos (e.g., replacing a starting B with a V)
    regions = str_replace(regions,"^B" , "V"),
    # ifelse logic: If the entry is 'wien', 'vienna city', or 'vienn', change it to 'vienna'
    regions = ifelse(regions %in% c('wien', 'vienna city', 'vienn', 'Vienn'), 'Vienna', regions),
    # Standardization: Create new columns with consistent casing
    regions_low = str_to_lower(regions),
    regions_upper = str_to_upper(regions),
    regions_kebab = str_to_kebab(regions)
)

## Let's check the data again. It should be perfectly clean now!


## =============================================================================
## PART 6: DEEP DIVE INTO REGULAR EXPRESSIONS (REGEX)
## =============================================================================

## What exactly are regular expressions? They are formal patterns used to search text.
## Instead of searching for an exact word, we can search for rules.

# ^ : starts with (e.g., ^r means starts with r)
# $ : ends with preceding
# . : any character (wildcard)
# * : match the preceding zero or more times
# + : match the preceding one or more times
# [0-9] : any digit 0-9 
# [a-z] : lower-case letters a-z
# [A-Z] : capital letters
# | : logical OR (e.g., [a-z]|[0-9] means letter OR number)
# \ : escape a character (in R, we need double slashes \\)
# [:blank:] : spaces and tabs

## Let's try it on a new vector of strings:
string2 <- c('abba', 'madonna', 'metallica', 'foo fighters', 'blink 182')

## EXERCISE: Detect patterns in the strings above
## str_detect() returns TRUE if it finds the pattern, and FALSE if it doesn't.

# 1) Any string with an 'a'

# 2) Any string that starts with an 'a'

# 3) Any string that starts with an 'a' OR ends with an 'a'

# 4) Any string that starts with an 'a' AND also ends with an 'a'

# 5) Any string that ends with a number

# 6) Any string that has a space


## Why is this useful for Public Policy?
## Imagine we want to generate dummy variables (TRUE/FALSE flags) based on complex
## bureaucratic codes in our dataset. 


## View the final dataset to see our new Regex-powered columns!


## =============================================================================
## PART 7: (OPTIONAL BONUS) DATA VISUALIZATION WITH GGPLOT2
## =============================================================================

## If we have extra time, let's look at how R handles visual data.
## The tidyverse includes a package called 'ggplot2' which is the gold standard 
## for data visualization in science and journalism.

## We will use a built-in dataset called 'mpg' (fuel economy data for 38 models of cars).
## Run this line to look at the raw data:
head(mpg)

## ggplot builds graphics layer by layer. 
## 1. I pipe the data into ggplot
## 2. geom_point (Add a scatterplot layer)
## 3. aes() (Map the 'aesthetics': x-axis to engine size (displ), y-axis to highway miles (hwy))

mpg %>% 
  ggplot(aes(x = displ, y = hwy, color = class, shape = class)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() + # This gives it a clean, modern look
  labs(
    title = "Engine Size vs. Highway Fuel Economy",
    subtitle = "Larger engines generally result in lower gas mileage.",
    x = "Engine Displacement (Liters)",
    y = "Highway Miles Per Gallon",
    color = "Car Type",
    shape = "Car Type"
  )

## Try running the block above! You should see a professional-grade plot 
## pop up in the 'Plots' pane in the bottom right corner of RStudio.