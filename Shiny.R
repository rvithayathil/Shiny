# Literary Prizes Interactive Explorer
library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)

# Load data
prizes <- read_csv("prizes_data.csv", show_col_types = FALSE)

# Data cleaning
prizes <- prizes %>%
  mutate(
    # Standardize gender
    gender = case_when(
      gender == "man" ~ "Male",
      gender == "woman" ~ "Female",
      TRUE ~ "Other"
    ),
    # Clean UK residence
    uk_residence = case_when(
      uk_residence == "TRUE" ~ "UK",
      uk_residence == "FALSE" ~ "Non-UK",
      TRUE ~ "Unknown"
    ),
    # Create decade
    decade = floor(prize_year / 10) * 10
  ) %>%
  filter(!is.na(prize_year))


# UI
ui <- page_navbar(
  title = "Literary Prizes Explorer",
  theme = bs_theme(bootswatch = "flatly", primary = "#2C3E50"),
  
  # Tab 1: Interactive Dashboard
  nav_panel(
    title = "Dashboard",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("🎛️ Controls"),
        sliderInput("year_range", "Year Range:", 
                    min = 1991, max = 2024, 
                    value = c(2000, 2024), sep = "", 
                    animate = animationOptions(interval = 300)),
        checkboxGroupInput("categories", "Categories:",
                           choices = c("Fiction" = "fiction", "Poetry" = "poetry", 
                                       "Biography" = "biography", "Children's" = "children's",
                                       "Crime" = "crime", "Non-fiction" = "non-fiction"),
                           selected = c("fiction", "poetry", "biography")),
        radioButtons("view_mode", "View Mode:",
                     choices = c("Count" = "count", "Percentage" = "pct", "Cumulative" = "cum"),
                     selected = "count"),
        hr(),
        actionButton("random_sample", "🎲 Random Sample", class = "btn-primary btn-block"),
        actionButton("reset_all", "🔄 Reset All", class = "btn-secondary btn-block")
      ),
      card(
        card_header("Trends Over Time"),
        plotlyOutput("main_trend", height = 350)
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Gender Split"),
          plotlyOutput("gender_pie", height = 250)
        ),
        card(
          card_header("UK Residence"),
          plotlyOutput("uk_bar", height = 250)
        )
      )
    )
  ),
  
  # Tab 2: Compare & Contrast
  nav_panel(
    title = "Compare",
    icon = icon("code-compare"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Prize A", class = "bg-primary"),
        selectInput("prize_a", NULL, 
                    choices = unique(prizes$prize_name), 
                    selected = "Booker Prize"),
        plotlyOutput("compare_a", height = 300),
        verbatimTextOutput("stats_a")
      ),
      card(
        card_header("Prize B", class = "bg-info"),
        selectInput("prize_b", NULL, 
                    choices = unique(prizes$prize_name), 
                    selected = "Costa Book of the Year"),
        plotlyOutput("compare_b", height = 300),
        verbatimTextOutput("stats_b")
      )
    ),
    card(
      card_header("Side-by-Side Analysis"),
      plotlyOutput("compare_combined", height = 300)
    )
  ),
  
  # Tab 3: Deep Dive Explorer
  nav_panel(
    title = "Explore",
    icon = icon("magnifying-glass-chart"),
    layout_sidebar(
      sidebar = sidebar(
        position = "right",
        width = 350,
        h4("🔍 Filters"),
        selectInput("explore_prize", "Prize:", 
                    choices = c("All", unique(prizes$prize_name)),
                    selected = "All"),
        selectInput("explore_gender", "Gender:",
                    choices = c("All", "Male", "Female", "Other"),
                    selected = "All"),
        selectInput("explore_uk", "UK Residence:",
                    choices = c("All", "UK", "Non-UK", "Unknown"),
                    selected = "All"),
        selectInput("explore_role", "Role:",
                    choices = c("All", unique(prizes$person_role)),
                    selected = "All"),
        hr(),
        downloadButton("download_filtered", "📥 Download Data", class = "btn-success btn-block"),
        hr(),
        h5("Quick Stats"),
        uiOutput("filter_stats")
      ),
      card(
        full_screen = TRUE,
        card_header("Interactive Scatter Plot"),
        plotlyOutput("scatter_explore", height = 500)
      ),
      card(
        card_header("Filtered Data Table"),
        DTOutput("explore_table")
      )
    )
  ),
  
  # Tab 4: Prize Sharing Network
  nav_panel(
    title = "Winners",
    icon = icon("trophy"),
    card(
      card_header("Winner Analysis"),
      layout_columns(
        col_widths = c(8, 4),
        plotlyOutput("winner_timeline", height = 400),
        card(
          h5("🎯 Insights"),
          uiOutput("winner_insights"),
          hr(),
          sliderInput("focus_decade", "Focus Decade:",
                      min = 1990, max = 2020, value = 2000, 
                      step = 10, sep = "")
        )
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Winners by Prize"),
        plotlyOutput("winner_by_prize", height = 300)
      ),
      card(
        card_header("Multiple Winners"),
        plotlyOutput("multiple_winners", height = 300)
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered <- reactive({
    prizes %>%
      filter(
        prize_year >= input$year_range[1],
        prize_year <= input$year_range[2],
        prize_genre %in% input$categories
      )
  })
  
  # Random sample action
  observeEvent(input$random_sample, {
    sample_years <- sample(1991:2024, 2) %>% sort()
    updateSliderInput(session, "year_range", value = sample_years)
    updateCheckboxGroupInput(session, "categories", 
                             selected = sample(c("fiction", "poetry", "biography"), 2))
  })
  
  # Reset action
  observeEvent(input$reset_all, {
    updateSliderInput(session, "year_range", value = c(2000, 2024))
    updateCheckboxGroupInput(session, "categories", 
                             selected = c("fiction", "poetry", "biography"))
    updateRadioButtons(session, "view_mode", selected = "count")
  })
  
  # Dashboard plots
  output$main_trend <- renderPlotly({
    data <- filtered() %>% count(prize_year, prize_genre)
    
    if(input$view_mode == "pct") {
      data <- data %>% group_by(prize_year) %>% mutate(n = n/sum(n)*100)
    } else if(input$view_mode == "cum") {
      data <- data %>% group_by(prize_genre) %>% mutate(n = cumsum(n))
    }
    
    plot_ly(data, x = ~prize_year, y = ~n, color = ~prize_genre,
            type = 'scatter', mode = 'lines+markers',
            colors = viridis(length(unique(data$prize_genre)))) %>%
      layout(hovermode = 'x unified',
             yaxis = list(title = ifelse(input$view_mode == "pct", "Percentage", "Count")))
  })
  
  output$gender_pie <- renderPlotly({
    data <- filtered() %>% filter(gender %in% c("Male", "Female")) %>% count(gender)
    plot_ly(data, labels = ~gender, values = ~n, type = 'pie',
            marker = list(colors = c('#E74C3C', '#3498DB')),
            textposition = 'inside', textinfo = 'label+percent') %>%
      layout(showlegend = FALSE)
  })
  
  output$uk_bar <- renderPlotly({
    data <- filtered() %>% count(uk_residence)
    plot_ly(data, x = ~uk_residence, y = ~n, type = 'bar',
            marker = list(color = '#2ECC71')) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Count"))
  })
  
  # Compare tab
  output$compare_a <- renderPlotly({
    data <- prizes %>% filter(prize_name == input$prize_a) %>% count(prize_year)
    plot_ly(data, x = ~prize_year, y = ~n, type = 'scatter', 
            mode = 'lines', fill = 'tozeroy',
            line = list(color = '#3498DB', width = 2)) %>%
      layout(yaxis = list(title = "Prizes"))
  })
  
  output$compare_b <- renderPlotly({
    data <- prizes %>% filter(prize_name == input$prize_b) %>% count(prize_year)
    plot_ly(data, x = ~prize_year, y = ~n, type = 'scatter', 
            mode = 'lines', fill = 'tozeroy',
            line = list(color = '#1ABC9C', width = 2)) %>%
      layout(yaxis = list(title = "Prizes"))
  })
  
  output$stats_a <- renderText({
    data <- prizes %>% filter(prize_name == input$prize_a)
    paste0("Total: ", nrow(data), "\n",
           "Winners: ", sum(data$person_role == "winner", na.rm = TRUE), "\n",
           "Female %: ", round(mean(data$gender == "Female", na.rm = TRUE)*100, 1))
  })
  
  output$stats_b <- renderText({
    data <- prizes %>% filter(prize_name == input$prize_b)
    paste0("Total: ", nrow(data), "\n",
           "Winners: ", sum(data$person_role == "winner", na.rm = TRUE), "\n",
           "Female %: ", round(mean(data$gender == "Female", na.rm = TRUE)*100, 1))
  })
  
  output$compare_combined <- renderPlotly({
    data <- prizes %>% 
      filter(prize_name %in% c(input$prize_a, input$prize_b)) %>%
      group_by(prize_name) %>%
      count(decade)
    
    plot_ly(data, x = ~decade, y = ~n, color = ~prize_name,
            type = 'bar', colors = c('#3498DB', '#1ABC9C')) %>%
      layout(barmode = 'group')
  })
  
  # Explore tab
  explore_filtered <- reactive({
    data <- prizes
    
    if(input$explore_prize != "All") {
      data <- data %>% filter(prize_name == input$explore_prize)
    }
    
    if(input$explore_gender != "All") {
      data <- data %>% filter(gender == input$explore_gender)
    }
    
    if(input$explore_uk != "All") {
      data <- data %>% filter(uk_residence == input$explore_uk)
    }
    
    if(input$explore_role != "All") {
      data <- data %>% filter(person_role == input$explore_role)
    }
    
    data
  })
  
  output$scatter_explore <- renderPlotly({
    plot_ly(explore_filtered(), x = ~prize_year, y = ~prize_name,
            color = ~gender, text = ~name,
            type = 'scatter', mode = 'markers',
            marker = list(size = 10),
            colors = viridis(3)) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = ""))
  })
  
  output$explore_table <- renderDT({
    datatable(explore_filtered() %>% 
                select(prize_year, prize_name, name, gender, uk_residence, 
                       person_role, ethnicity_macro),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$filter_stats <- renderUI({
    n <- nrow(explore_filtered())
    HTML(paste0(
      "<div class='alert alert-info'>",
      "<strong>", n, "</strong> prizes selected<br>",
      "<small>", round(n/nrow(prizes)*100, 1), "% of total</small>",
      "</div>"
    ))
  })
  
  output$download_filtered <- downloadHandler(
    filename = function() paste0("prizes_", Sys.Date(), ".csv"),
    content = function(file) write_csv(explore_filtered(), file)
  )
  
  # Winners tab
  output$winner_timeline <- renderPlotly({
    data <- prizes %>%
      filter(person_role == "winner") %>%
      count(prize_year, gender) %>%
      filter(gender %in% c("Male", "Female"))
    
    plot_ly(data, x = ~prize_year, y = ~n, color = ~gender,
            type = 'scatter', mode = 'lines+markers',
            colors = c('#E74C3C', '#3498DB')) %>%
      layout(yaxis = list(title = "Number of Winners"))
  })
  
  output$winner_insights <- renderUI({
    decade_data <- prizes %>%
      filter(person_role == "winner",
             prize_year >= input$focus_decade, 
             prize_year < input$focus_decade + 10)
    
    female_pct <- mean(decade_data$gender == "Female", na.rm = TRUE) * 100
    
    HTML(paste0(
      "<p><strong>", input$focus_decade, "s:</strong></p>",
      "<p>", nrow(decade_data), " total winners</p>",
      "<p>", round(female_pct, 1), "% female</p>",
      "<p class='text-muted'><small>",
      ifelse(female_pct > 30, "Good representation", "More diversity needed"),
      "</small></p>"
    ))
  })
  
  output$winner_by_prize <- renderPlotly({
    data <- prizes %>%
      filter(person_role == "winner") %>%
      count(prize_name) %>%
      arrange(desc(n)) %>%
      head(10)
    
    plot_ly(data, x = ~n, y = ~reorder(prize_name, n), 
            type = 'bar', orientation = 'h',
            marker = list(color = '#9B59B6')) %>%
      layout(xaxis = list(title = "Winners"), 
             yaxis = list(title = ""),
             margin = list(l = 200))
  })
  
  output$multiple_winners <- renderPlotly({
    data <- prizes %>%
      filter(person_role == "winner") %>%
      count(name) %>%
      filter(n > 1) %>%
      arrange(desc(n)) %>%
      head(10)
    
    plot_ly(data, x = ~n, y = ~reorder(name, n), 
            type = 'bar', orientation = 'h',
            marker = list(color = '#E67E22')) %>%
      layout(xaxis = list(title = "Wins"), 
             yaxis = list(title = ""),
             margin = list(l = 150))
  })
}

shinyApp(ui, server)