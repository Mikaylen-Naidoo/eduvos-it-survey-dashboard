library(shiny)
library(shinydashboard)
library(tidyverse)

# Load dataset
df <- read.csv("C:/Users/mikay/Desktop/University/2025/Block 1/ITRDA3-11/Actual Assignment for ITRDA/graduate_survey.csv", stringsAsFactors = FALSE)

# Data Cleaning (Modify according to previous steps)
df <- df %>%
  select(Campus, StudyField, ProgLang, Databases, WebFramework, Platform, AISearch, AITool, Industry, Role, Employment) %>%
  mutate(across(everything(), ~ replace_na(.x, "Unknown")))

ui <- dashboardPage(
  dashboardHeader(title = "Eduvos Graduate Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Programming Languages", tabName = "prog_langs", icon = icon("code")),
      menuItem("Databases", tabName = "databases", icon = icon("database")),
      menuItem("Web Frameworks", tabName = "web_frameworks", icon = icon("globe")),
      menuItem("Employment & Industry", tabName = "employment", icon = icon("briefcase"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Select Study Field", width = 4, 
                    selectInput("studyField", "Study Field", choices = unique(df$StudyField), selected = "IT")),
                box(title = "Employment Rate", width = 8, plotOutput("employmentPlot"))
              )
      ),
      tabItem(tabName = "prog_langs",
              fluidRow(
                box(title = "Most Popular Programming Languages", width = 12, plotOutput("progLangPlot"))
              )
      ),
      tabItem(tabName = "databases",
              fluidRow(
                box(title = "Top Databases Used", width = 12, plotOutput("dbPlot"))
              )
      ),
      tabItem(tabName = "web_frameworks",
              fluidRow(
                box(title = "Most Used Web Frameworks", width = 12, plotOutput("webPlot"))
              )
      ),
      tabItem(tabName = "employment",
              fluidRow(
                box(title = "Industry Distribution", width = 12, plotOutput("industryPlot"))
              )
      )
    )
  )
)

server <- function(input, output) {
  
  # Employment Rate Plot
  output$employmentPlot <- renderPlot({
    df_filtered <- df %>% filter(StudyField == input$studyField)
    ggplot(df_filtered, aes(x = Employment, fill = Employment)) +
      geom_bar() +
      theme_minimal() +
      labs(title = paste("Employment Status for", input$studyField), x = "Employment Type", y = "Count")
  })
  
  # Programming Languages Plot
  output$progLangPlot <- renderPlot({
    df_langs <- df %>% separate_rows(ProgLang, sep = ";") %>% mutate(ProgLang = str_trim(ProgLang))
    top_langs <- df_langs %>% count(ProgLang, sort = TRUE) %>% top_n(10, n)
    ggplot(top_langs, aes(x = reorder(ProgLang, n), y = n, fill = ProgLang)) +
      geom_bar(stat = "identity") + coord_flip() +
      theme_minimal() + labs(title = "Most Popular Programming Languages", x = "Language", y = "Count")
  })
  
  # Database Plot
  output$dbPlot <- renderPlot({
    df_db <- df %>% separate_rows(Databases, sep = ";") %>% mutate(Databases = str_trim(Databases))
    top_db <- df_db %>% count(Databases, sort = TRUE) %>% top_n(10, n)
    ggplot(top_db, aes(x = reorder(Databases, n), y = n, fill = Databases)) +
      geom_bar(stat = "identity") + coord_flip() +
      theme_minimal() + labs(title = "Top Databases", x = "Database", y = "Count")
  })
  
  # Web Frameworks Plot
  output$webPlot <- renderPlot({
    df_web <- df %>% separate_rows(WebFramework, sep = ";") %>% mutate(WebFramework = str_trim(WebFramework))
    top_web <- df_web %>% count(WebFramework, sort = TRUE) %>% top_n(10, n)
    ggplot(top_web, aes(x = reorder(WebFramework, n), y = n, fill = WebFramework)) +
      geom_bar(stat = "identity") + coord_flip() +
      theme_minimal() + labs(title = "Top Web Frameworks", x = "Framework", y = "Count")
  })
  
  # Industry Distribution Plot
  output$industryPlot <- renderPlot({
    df_industry <- df %>% separate_rows(Industry, sep = ";") %>% mutate(Industry = str_trim(Industry))
    top_industry <- df_industry %>% count(Industry, sort = TRUE) %>% top_n(10, n)
    ggplot(top_industry, aes(x = reorder(Industry, n), y = n, fill = Industry)) +
      geom_bar(stat = "identity") + coord_flip() +
      theme_minimal() + labs(title = "Industry Distribution", x = "Industry", y = "Count")
  })
}

# Run the application
shinyApp(ui = ui, server = server)