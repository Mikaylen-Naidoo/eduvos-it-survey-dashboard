# Load necessary libraries
library(tidyverse) # For data manipulation
library(janitor)   # For cleaning column names
library(dplyr)     # For data wrangling

# Define file path
file_path <- "C:/Users/mikay/Desktop/University/2025/Block 1/ITRDA3-11/Actual Assignment for ITRDA/graduate_survey (1).csv"

# Read the dataset
df <- read.csv(file_path, stringsAsFactors = FALSE) %>%
  clean_names()  # Clean column names to ensure consistency

# Select relevant columns
df_selected <- df %>%
  select(campus, study_field, branch, role, edu_level, prog_lang, databases, platform, 
         web_framework, industry, ai_search, ai_tool, employment)

# Handle missing values
df_cleaned <- df_selected %>%
  mutate(across(everything(), ~ ifelse(. == "" | is.na(.), NA, .))) %>%  # Convert blanks to NA
  drop_na()  # Remove rows with missing values (if appropriate)

# Standardise categorical columns (Example: Fixing campus names)
df_cleaned <- df_cleaned %>%
  mutate(campus = case_when(
    campus %in% c("Durban", "Umhlanga") ~ "Durban",
    campus %in% c("Johannesburg", "Sandton") ~ "Johannesburg",
    TRUE ~ campus
  ))

# Identify top 5 campuses with the most responses
top_campuses <- df_cleaned %>%
  count(campus, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(campus)

# Filter dataset for these campuses only
df_filtered <- df_cleaned %>%
  filter(campus %in% top_campuses)

# View summary of cleaned data
summary(df_filtered)

# Save cleaned dataset for further analysis
write.csv(df_filtered, "C:/Users/mikay/Desktop/cleaned_graduate_survey.csv", row.names = FALSE)

