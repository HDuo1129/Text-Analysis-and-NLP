## -----------------------------------------------------------------------------
## Title:       Homework 1: Tidyverse & Regex Practice
## Author:      Huang Duo
## Email:       Huang_Duo@student.ceu.edu
## Course:      DOPP 5688: Text as Data (Spring 2026)
## Description: Practice cleaning text data, using regex, and basic plotting.
## -----------------------------------------------------------------------------

## INSTRUCTIONS:
## Add in your author name and email. Also rename the file such that the title 
## includes your last name.
## Write the necessary R code below each comment prompt to complete the tasks.
## Make sure to run your code step-by-step to check your work!
## You can compare it to the solution script on Moodle

## =============================================================================
## STEP 1: LOAD YOUR PACKAGES
## =============================================================================

## Load the 'tidyverse' package below:
library(tidyverse)


## =============================================================================
## STEP 2: GENERATE THE MESSY DATA
## =============================================================================

## Run the code block below to generate your homework dataset. 
## This simulates a messy database of public policy grant applications.

df_grants <- tibble(
  department = c(
    rep("  Health", 12), rep("healTh ", 8), rep(" HLT", 5), 
    rep("eduCATION", 15), rep(" Edu", 10), rep("education city", 5),
    rep("tranSport", 10), rep("tranzport", 3), rep("Transport Dept", 2)
  ),
  grant_id = c(
    paste0("HLT-2023-A", 1:12), paste0("HLT-2024-B", 1:8), paste0("HLT-2024-BX", 1:5),
    paste0("EDU-2023-C", 1:15), paste0("EDU-2024-D", 1:10), paste0("EDU-2024-DX", 1:5),
    paste0("TRN-2022-E", 1:10), paste0("TRN-2023-F", 1:3), paste0("TRN-2024-FX", 1:2)
  ),
  budget_thousands = sample(50:500, 70, replace = TRUE)
)

## Inspect the data:
head(df_grants)
table(df_grants$department)


## =============================================================================
## STEP 3: CLEAN THE TEXT DATA
## =============================================================================

## Use your Tidyverse and stringr skills to clean the 'department' column.
## In a comment, describe the issues that you need to solve to make this data clean
## Generate a new data frame called df_grants_clean. 

## Verify your cleaning process! 
## Below, add code that will show that you completed the cleaning and that it worked

df_grants_clean <- df_grants %>% 
  mutate(
    department = str_squish(department),  # remove  white spaces
    department = str_replace(department, "tranzport", "transport"),

    department = str_to_lower(department),  # standardize the capitalization
    department = ifelse(department %in% c("health", "hlt"), "Health", department),
    department = ifelse(department %in% c("education", "edu", "education city"),
                        "Education", department),  # Collapse all "Education" variants
    department = ifelse(department %in% c("transport", "transport dept"), 
                        "Transport", department)   # Collapse all "Transport" variants
  )

table(df_grants_clean$department)

## =============================================================================
## STEP 4: REGULAR EXPRESSIONS (REGEX) FLAG CREATION
## =============================================================================

## The 'grant_id' column contains hidden metadata. 
## Create new logical (TRUE/FALSE) columns based on patterns in the grant_id.
## Describe the patterns (i want two) that you identified and want to build on


## Verify your new columns:

df_grants_clean <- df_grants_clean %>% 
  mutate(
    is_2024      = str_detect(grant_id, "-2024-"), # return TRUE if the pattern is found
    has_x_code = str_detect(grant_id, "X[0-9]+$")
  )

table(df_grants_clean$is_2024)
table(df_grants_clean$has_x_code)
table(df_grants_clean$is_2024, df_grants_clean$has_x_code)
head(df_grants_clean)

## =============================================================================
## STEP 5: DATA VISUALIZATION (GGPLOT2)
## =============================================================================

## Create a histogram showing the distribution of the grant budgets.
## Map the x-axis to the budget variable and fill the bars by the cleaned department name.
## Ensure your plot has a clean theme and appropriate titles/labels.

# Write your ggplot code below:

df_grants_clean %>% 
  ggplot(aes(x = budget_thousands, fill = department)) +
  geom_histogram(binwidth = 50, color = "white") +
  theme_minimal() +
  labs(
    title    = "Distribution of Public Policy Grant Budgets",
    x        = "Budget (in thousands)",
    y        = "Number of grants",
    fill     = "Department"
  )


## Save your script and submit it properly named on Moodles!