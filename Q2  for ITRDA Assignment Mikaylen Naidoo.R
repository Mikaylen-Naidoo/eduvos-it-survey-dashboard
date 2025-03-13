# Load necessary libraries
library(tidyverse) # Data manipulation and visualization
library(ggplot2)   # For plots
library(tidyr)     # To separate multiple responses

# Define file path
file_path <- "C:/Users/mikay/Desktop/University/2025/Block 1/ITRDA3-11/Actual Assignment for ITRDA/cleaned_graduate_survey.csv"

# Load cleaned dataset
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Function to count occurrences of tools in multi-response columns
count_top_tools <- function(df, column, top_n = 10) {
  df %>%
    select({{column}}) %>%
    drop_na() %>%
    separate_rows({{column}}, sep = ";") %>%  # Split values into separate rows
    count({{column}}, sort = TRUE) %>%  # Count occurrences
    top_n(top_n, n)  # Get top N tools
}

# Top programming languages
top_prog_lang <- count_top_tools(df, prog_lang)

ggplot(top_prog_lang, aes(x = reorder(prog_lang, n), y = n, fill = prog_lang)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  labs(title = "Top Programming Languages Used by Graduates", x = "Programming Language", y = "Count")

# Top databases
top_databases <- count_top_tools(df, databases)

ggplot(top_databases, aes(x = reorder(databases, n), y = n, fill = databases)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  labs(title = "Top Databases Used by Graduates", x = "Database", y = "Count")

# Top web frameworks
top_web_frameworks <- count_top_tools(df, web_framework)

ggplot(top_web_frameworks, aes(x = reorder(web_framework, n), y = n, fill = web_framework)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  labs(title = "Top Web Frameworks Used by Graduates", x = "Web Framework", y = "Count")

# Top cloud platforms
top_platforms <- count_top_tools(df, platform)

ggplot(top_platforms, aes(x = reorder(platform, n), y = n, fill = platform)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  labs(title = "Top Cloud Platforms Used by Graduates", x = "Platform", y = "Count")

# Top AI search tools
top_ai_search <- count_top_tools(df, ai_search)

ggplot(top_ai_search, aes(x = reorder(ai_search, n), y = n, fill = ai_search)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  labs(title = "Top AI Search Tools Used by Graduates", x = "AI Search Tool", y = "Count")

# Top AI developer tools
top_ai_tools <- count_top_tools(df, ai_tool)

ggplot(top_ai_tools, aes(x = reorder(ai_tool, n), y = n, fill = ai_tool)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  labs(title = "Top AI Developer Tools Used by Graduates", x = "AI Developer Tool", y = "Count")

# Split and count industries per study field
industry_study_field <- df %>%
  select(study_field, industry) %>%
  drop_na() %>%
  separate_rows(industry, sep = ";") %>%
  count(study_field, industry, sort = TRUE)

# Visualise popular industries per study field
ggplot(industry_study_field, aes(x = reorder(industry, n), y = n, fill = study_field)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~study_field, scales = "free") +
  labs(title = "Top Industries by Study Field", x = "Industry", y = "Count") +
  theme(legend.position = "bottom")

# Count job roles per study field
role_study_field <- df %>%
  select(study_field, role) %>%
  drop_na() %>%
  count(study_field, role, sort = TRUE)

# Visualise job roles
ggplot(role_study_field, aes(x = reorder(role, n), y = n, fill = study_field)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~study_field, scales = "free") +
  labs(title = "Top Job Roles by Study Field", x = "Job Role", y = "Count") +
  theme(legend.position = "bottom")

# Count employment status per study field
employment_rate <- df %>%
  count(study_field, employment) %>%
  group_by(study_field) %>%
  mutate(percentage = n / sum(n) * 100)

# Visualise employment rate
ggplot(employment_rate, aes(x = study_field, y = percentage, fill = employment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Percentage") +
  theme_minimal()

