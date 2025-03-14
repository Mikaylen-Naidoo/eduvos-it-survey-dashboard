df_langs <- df %>%
  separate_rows(ProgLang, sep = ";") %>%
  mutate(ProgLang = str_trim(ProgLang))

top_langs <- df_langs %>%
  count(ProgLang, sort = TRUE) %>%
  top_n(10, n)

ggplot(top_langs, aes(x = reorder(ProgLang, n), y = n, fill = ProgLang)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Programming Languages Used by Graduates", x = "Programming Language", y = "Count")

df_db <- df %>%
  separate_rows(Databases, sep = ";") %>%
  mutate(Databases = str_trim(Databases))

top_db <- df_db %>%
  count(Databases, sort = TRUE) %>%
  top_n(10, n)

ggplot(top_db, aes(x = reorder(Databases, n), y = n, fill = Databases)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Databases Used by Graduates", x = "Database", y = "Count")

df_ai_search <- df %>%
  separate_rows(AISearch, sep = ";") %>%
  mutate(AISearch = str_trim(AISearch))

top_ai_search <- df_ai_search %>%
  count(AISearch, sort = TRUE) %>%
  top_n(10, n)

ggplot(top_ai_search, aes(x = reorder(AISearch, n), y = n, fill = AISearch)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Most Popular AI Search Tools", x = "AI Search Tool", y = "Count")

df_ai_tool <- df %>%
  separate_rows(AITool, sep = ";") %>%
  mutate(AITool = str_trim(AITool))

top_ai_tool <- df_ai_tool %>%
  count(AITool, sort = TRUE) %>%
  top_n(10, n)

ggplot(top_ai_tool, aes(x = reorder(AITool, n), y = n, fill = AITool)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Most Popular AI Developer Tools", x = "AI Tool", y = "Count")

df_web <- df %>%
  separate_rows(WebFramework, sep = ";") %>%
  mutate(WebFramework = str_trim(WebFramework))

top_web <- df_web %>%
  count(WebFramework, sort = TRUE) %>%
  top_n(10, n)

ggplot(top_web, aes(x = reorder(WebFramework, n), y = n, fill = WebFramework)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Web Frameworks Used by Graduates", x = "Web Framework", y = "Count")

df_platform <- df %>%
  separate_rows(Platform, sep = ";") %>%
  mutate(Platform = str_trim(Platform))

top_platform <- df_platform %>%
  count(Platform, sort = TRUE) %>%
  top_n(10, n)

ggplot(top_platform, aes(x = reorder(Platform, n), y = n, fill = Platform)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Cloud Platforms Used by Graduates", x = "Cloud Platform", y = "Count")

df_industry <- df %>%
  separate_rows(Industry, sep = ";") %>%
  mutate(Industry = str_trim(Industry))

top_industry <- df_industry %>%
  count(StudyField, Industry, sort = TRUE) %>%
  group_by(StudyField) %>%
  top_n(3, n)  # Top 3 industries per study field

ggplot(top_industry, aes(x = reorder(Industry, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Most Popular Industries by Study Field", x = "Industry", y = "Count")

df_role <- df %>%
  count(StudyField, Role, sort = TRUE) %>%
  group_by(StudyField) %>%
  top_n(3, n)  # Top 3 roles per study field

ggplot(df_role, aes(x = reorder(Role, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Job Roles by Study Field", x = "Job Role", y = "Count")

df_employment <- df %>%
  count(StudyField, Employment, sort = TRUE)

ggplot(df_employment, aes(x = StudyField, y = n, fill = Employment)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Employment Rate")

