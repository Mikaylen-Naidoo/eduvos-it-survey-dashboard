# Load necessary libraries
library(tidyverse)

# Read the dataset
file_path <- "C:/Users/mikay/Desktop/University/2025/Block 1/ITRDA3-11/Actual Assignment for ITRDA/graduate_survey.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# View the first few rows
head(df)

df <- df %>%
  select(Campus, StudyField, Branch, Role, EduLevel, ProgLang, Databases, Platform, 
         WebFramework, Industry, AISearch, AITool, Employment)

# Count missing values
colSums(is.na(df))

# Option 1: Remove rows with too many missing values
df <- df %>%
  filter(rowSums(is.na(.)) < ncol(df) / 2)

# Option 2: Impute missing categorical values with "Unknown"
df[is.na(df)] <- "Unknown"

df$Campus <- case_when(
  df$Campus %in% c("Durban", "Umhlanga") ~ "Durban",
  df$Campus %in% c("Pretoria", "Midrand") ~ "Pretoria",
  TRUE ~ df$Campus
)

df$EduLevel <- tolower(df$EduLevel)  # Convert education levels to lowercase
df$StudyField <- str_to_title(df$StudyField)  # Capitalize each word in StudyField

top_campuses <- df %>%
  count(Campus, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(Campus)

df <- df %>%
  filter(Campus %in% top_campuses)