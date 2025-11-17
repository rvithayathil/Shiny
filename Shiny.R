

# Literary Prizes Interactive Explorer
library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)

# Load data
prizes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/main/data/2025/2025-10-28/prizes.csv")
write_csv(prizes, "prizes_data.csv")
prizes <- read_csv("prizes_data.csv")

# UI
ui <- page_navbar(
  title = "Literary Prizes Explorer",
  theme = bs_theme(bootswatch = "flatly", primary = "#8E44AD"),
  
  # Tab 1: Interactive Dashboard
  nav_panel(
    title = "Dashboard",
    icon = icon("trophy"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        h4("Interactive Controls"),
        sliderInput("year_range", "Prize Years:", 
                    min = min(prizes$prize_year, na.rm = TRUE), 
                    max = max(prizes$prize_year, na.rm = TRUE), 
                    value = c(2000, max(prizes$prize_year, na.rm = TRUE)), 
                    sep = "",
                    animate = animationOptions(interval = 500, loop = TRUE)),
        
        selectInput("prize_select", "Prize:",
                    choices = c("All", unique(prizes$prize_name)),
                    selected = "All"),
        
        checkboxGroupInput("gender_check", "Gender:",
                           choices = unique(prizes$gender),
                           selected = unique(prizes$gender)),
        
        radioButtons("view_type", "Visualization Style:",
                     choices = c("Timeline" = "line", 
                                 "Distribution" = "bar", 
                                 "Heatmap" = "heat"),
                     selected = "line"),
        
        hr(),
        actionButton("randomize", "🎲 Random Period", 
                     class = "btn-primary btn-block mb-2"),
        actionButton("reset", "🔄 Reset All", 
                     class = "btn-secondary btn-block")
      ),
      
      layout_columns(
        col_widths = c(12, 6, 6),
        card(
          full_screen = TRUE,
          card_header("Awards Over Time"),
          plotlyOutput("main_viz", height = 400)
        ),
        value_box(
          title = "Total Winners",
          value = textOutput("total_winners"),
          showcase = icon("award"),
          theme = "success"
        ),
        value_box(
          title = "Unique Prizes",
          value = textOutput("prize_count"),
          showcase = icon("star"),
          theme = "info"
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Gender Distribution"),
          plotlyOutput("gender_plot", height = 280)
        ),
        card(
          card_header("Top Winners"),
          plotlyOutput("top_winners", height = 280)
        )
      )
    )
  ),
  
  # Tab 2: Prize Comparison
  nav_panel(
    title = "Compare",
    icon = icon("scale-balanced"),
    card(
      card_header("Prize-to-Prize Comparison"),
      layout_columns(
        col_widths = c(3, 3, 6),
        selectInput("prize_a", "Prize A:", 
                    choices = unique(prizes$prize_name),
                    selected = unique(prizes$prize_name)[1]),
        selectInput("prize_b", "Prize B:", 
                    choices = unique(prizes$prize_name),
                    selected = unique(prizes$prize_name)[2]),
        checkboxGroupInput("compare_metrics", "Compare:",
                           choices = c("Gender" = "gender", 
                                       "UK Residence" = "uk_residence",
                                       "Education" = "degree",
                                       "Ethnicity" = "ethnicity_macro"),
                           selected = c("gender", "uk_residence"),
                           inline = TRUE)
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header("Prize A Analysis"),
        plotlyOutput("compare_plot_a", height = 350)
      ),
      card(
        full_screen = TRUE,
        card_header("Prize B Analysis"),
        plotlyOutput("compare_plot_b", height = 350)
      )
    ),
    
    card(
      card_header("Side-by-Side Comparison"),
      plotlyOutput("side_by_side", height = 300)
    )
  ),
  
  # Tab 3: Diversity Explorer
  nav_panel(
    title = "Diversity",
    icon = icon("users"),
    layout_sidebar(
      sidebar = sidebar(
        position = "right",
        width = 320,
        h4("🔍 Diversity Filters"),
        
        selectInput("diversity_prize", "Focus Prize:",
                    choices = c("All", unique(prizes$prize_name)),
                    selected = "All"),
        
        sliderInput("diversity_years", "Year Range:",
                    min = min(prizes$prize_year, na.rm = TRUE),
                    max = max(prizes$prize_year, na.rm = TRUE),
                    value = c(1990, max(prizes$prize_year, na.rm = TRUE)),
                    sep = ""),
        
        radioButtons("diversity_metric", "Analyze:",
                     choices = c("Ethnicity" = "ethnicity_macro",
                                 "Gender" = "gender",
                                 "UK Residence" = "uk_residence",
                                 "Sexuality" = "sexuality",
                                 "Education Level" = "highest_degree"),
                     selected = "ethnicity_macro"),
        
        hr(),
        h5("📊 Quick Stats"),
        uiOutput("diversity_stats")
      ),
      
      card(
        full_screen = TRUE,
        card_header("Diversity Trends Over Time"),
        plotlyOutput("diversity_timeline", height = 400)
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Current Distribution"),
          plotlyOutput("diversity_pie", height = 300)
        ),
        card(
          card_header("Representation Index"),
          plotlyOutput("diversity_index", height = 300)
        )
      )
    )
  ),
  
  # Tab 4: Data Explorer
  nav_panel(
    title = "Explore",
    icon = icon("table"),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        h4("🎯 Data Filters"),
        
        selectInput("explore_prize", "Prize:",
                    choices = c("All", unique(prizes$prize_name)),
                    selected = "All",
                    multiple = TRUE),
        
        selectInput("explore_role", "Role:",
                    choices = c("All", unique(prizes$person_role)),
                    selected = "All"),
        
        selectInput("explore_uk", "UK Residence:",
                    choices = c("All", unique(prizes$uk_residence)),
                    selected = "All"),
        
        textInput("search_name", "Search Name:", 
                  placeholder = "Enter name..."),
        
        hr(),
        downloadButton("download_csv", "📥 Download CSV", 
                       class = "btn-success btn-block mb-2"),
        downloadButton("download_excel", "📥 Download Excel", 
                       class = "btn-info btn-block"),
        
        hr(),
        h5("Selection Info"),
        verbatimTextOutput("selection_info")
      ),
      
      card(
        full_screen = TRUE,
        card_header("Interactive Data Table"),
        DTOutput("data_table", height = 600)
      ),
      
      card(
        card_header("Data Visualization"),
        plotlyOutput("explore_scatter", height = 350)
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- prizes %>%
      filter(
        prize_year >= input$year_range[1],
        prize_year <= input$year_range[2],
        gender %in% input$gender_check
      )
    
    if(input$prize_select != "All") {
      data <- data %>% filter(prize_name == input$prize_select)
    }
    
    data
  })
  
  # Random period button
  observeEvent(input$randomize, {
    start_year <- sample(min(prizes$prize_year):max(prizes$prize_year)-10, 1)
    updateSliderInput(session, "year_range", 
                      value = c(start_year, start_year + 10))
  })
  
  # Reset button
  observeEvent(input$reset, {
    updateSliderInput(session, "year_range", 
                      value = c(2000, max(prizes$prize_year, na.rm = TRUE)))
    updateSelectInput(session, "prize_select", selected = "All")
    updateCheckboxGroupInput(session, "gender_check", 
                             selected = unique(prizes$gender))
  })
  
  # Value boxes
  output$total_winners <- renderText({
    nrow(filtered_data() %>% filter(person_role == "winner"))
  })
  
  output$prize_count <- renderText({
    n_distinct(filtered_data()$prize_name)
  })
  
  # Main visualization
  output$main_viz <- renderPlotly({
    data <- filtered_data()
    
    if(input$view_type == "line") {
      plot_data <- data %>% 
        count(prize_year, prize_name) %>%
        group_by(prize_year) %>%
        summarise(total = sum(n))
      
      plot_ly(plot_data, x = ~prize_year, y = ~total,
              type = 'scatter', mode = 'lines+markers',
              line = list(color = '#8E44AD', width = 3),
              marker = list(size = 8)) %>%
        layout(xaxis = list(title = "Year"),
               yaxis = list(title = "Number of Awards"))
      
    } else if(input$view_type == "bar") {
      plot_data <- data %>% count(prize_name) %>% arrange(desc(n)) %>% head(10)
      
      plot_ly(plot_data, x = ~n, y = ~reorder(prize_name, n),
              type = 'bar', orientation = 'h',
              marker = list(color = viridis(nrow(plot_data)))) %>%
        layout(xaxis = list(title = "Count"),
               yaxis = list(title = ""))
      
    } else {
      plot_data <- data %>%
        count(prize_year, gender) %>%
        complete(prize_year, gender, fill = list(n = 0))
      
      plot_ly(plot_data, x = ~prize_year, y = ~gender, z = ~n,
              type = "heatmap", colors = viridis(50)) %>%
        layout(xaxis = list(title = "Year"),
               yaxis = list(title = "Gender"))
    }
  })
  
  # Gender plot
  output$gender_plot <- renderPlotly({
    data <- filtered_data() %>% 
      filter(!is.na(gender)) %>%
      count(gender)
    
    plot_ly(data, labels = ~gender, values = ~n,
            type = 'pie', hole = 0.4,
            marker = list(colors = viridis(nrow(data)))) %>%
      layout(showlegend = TRUE)
  })
  
  # Top winners
  output$top_winners <- renderPlotly({
    data <- filtered_data() %>%
      filter(person_role == "winner") %>%
      count(name) %>%
      arrange(desc(n)) %>%
      head(8)
    
    plot_ly(data, x = ~n, y = ~reorder(name, n),
            type = 'bar', orientation = 'h',
            marker = list(color = '#E67E22')) %>%
      layout(xaxis = list(title = "Wins"),
             yaxis = list(title = ""),
             margin = list(l = 150))
  })
  
  # Compare Tab
  compare_data_a <- reactive({
    prizes %>% filter(prize_name == input$prize_a)
  })
  
  compare_data_b <- reactive({
    prizes %>% filter(prize_name == input$prize_b)
  })
  
  output$compare_plot_a <- renderPlotly({
    data <- compare_data_a()
    
    if("gender" %in% input$compare_metrics) {
      plot_data <- data %>% count(gender) %>% filter(!is.na(gender))
      
      plot_ly(plot_data, labels = ~gender, values = ~n,
              type = 'pie', marker = list(colors = viridis(nrow(plot_data)))) %>%
        layout(title = list(text = "Gender Distribution", x = 0.5))
    }
  })
  
  output$compare_plot_b <- renderPlotly({
    data <- compare_data_b()
    
    if("gender" %in% input$compare_metrics) {
      plot_data <- data %>% count(gender) %>% filter(!is.na(gender))
      
      plot_ly(plot_data, labels = ~gender, values = ~n,
              type = 'pie', marker = list(colors = viridis(nrow(plot_data)))) %>%
        layout(title = list(text = "Gender Distribution", x = 0.5))
    }
  })
  
  output$side_by_side <- renderPlotly({
    data_a <- compare_data_a() %>% mutate(prize = input$prize_a)
    data_b <- compare_data_b() %>% mutate(prize = input$prize_b)
    
    combined <- bind_rows(data_a, data_b) %>%
      count(prize, prize_year) %>%
      filter(!is.na(prize_year))
    
    plot_ly(combined, x = ~prize_year, y = ~n, color = ~prize,
            type = 'scatter', mode = 'lines+markers',
            colors = c('#3498DB', '#E74C3C')) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Awards"))
  })
  
  # Diversity Tab
  diversity_data <- reactive({
    data <- prizes
    
    if(input$diversity_prize != "All") {
      data <- data %>% filter(prize_name == input$diversity_prize)
    }
    
    data %>%
      filter(prize_year >= input$diversity_years[1],
             prize_year <= input$diversity_years[2])
  })
  
  output$diversity_timeline <- renderPlotly({
    metric <- input$diversity_metric
    
    data <- diversity_data() %>%
      filter(!is.na(!!sym(metric))) %>%
      count(prize_year, !!sym(metric))
    
    plot_ly(data, x = ~prize_year, y = ~n, 
            color = as.formula(paste0("~", metric)),
            type = 'scatter', mode = 'lines+markers',
            colors = viridis(n_distinct(data[[metric]]))) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Count"))
  })
  
  output$diversity_pie <- renderPlotly({
    metric <- input$diversity_metric
    
    data <- diversity_data() %>%
      filter(!is.na(!!sym(metric))) %>%
      count(!!sym(metric))
    
    plot_ly(data, labels = as.formula(paste0("~", metric)), 
            values = ~n, type = 'pie',
            marker = list(colors = viridis(nrow(data))))
  })
  
  output$diversity_index <- renderPlotly({
    metric <- input$diversity_metric
    
    data <- diversity_data() %>%
      filter(!is.na(!!sym(metric))) %>%
      mutate(decade = floor(prize_year / 10) * 10) %>%
      count(decade, !!sym(metric)) %>%
      group_by(decade) %>%
      mutate(prop = n / sum(n),
             shannon = -prop * log(prop)) %>%
      summarise(diversity = sum(shannon))
    
    plot_ly(data, x = ~decade, y = ~diversity,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = '#9B59B6', width = 3),
            marker = list(size = 10)) %>%
      layout(xaxis = list(title = "Decade"),
             yaxis = list(title = "Shannon Index"))
  })
  
  output$diversity_stats <- renderUI({
    n <- nrow(diversity_data())
    metric <- input$diversity_metric
    categories <- n_distinct(diversity_data()[[metric]], na.rm = TRUE)
    
    HTML(paste0(
      "<div class='alert alert-info'>",
      "<strong>", format(n, big.mark = ","), "</strong> records<br>",
      "<strong>", categories, "</strong> categories<br>",
      "<small>", round(n/nrow(prizes)*100, 1), "% of total</small>",
      "</div>"
    ))
  })
  
  # Explore Tab
  explore_data <- reactive({
    data <- prizes
    
    if(!"All" %in% input$explore_prize && length(input$explore_prize) > 0) {
      data <- data %>% filter(prize_name %in% input$explore_prize)
    }
    
    if(input$explore_role != "All") {
      data <- data %>% filter(person_role == input$explore_role)
    }
    
    if(input$explore_uk != "All") {
      data <- data %>% filter(uk_residence == input$explore_uk)
    }
    
    if(nchar(input$search_name) > 0) {
      data <- data %>% 
        filter(str_detect(tolower(name), tolower(input$search_name)))
    }
    
    data
  })
  
  output$data_table <- renderDT({
    datatable(
      explore_data() %>%
        select(prize_year, prize_name, name, gender, uk_residence, 
               person_role, ethnicity_macro, highest_degree),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        searchHighlight = TRUE,
        dom = 'Bfrtip'
      ),
      filter = 'top',
      rownames = FALSE,
      selection = 'single'
    )
  })
  
  output$explore_scatter <- renderPlotly({
    data <- explore_data() %>%
      filter(!is.na(prize_year), !is.na(gender))
    
    plot_ly(data, x = ~prize_year, y = ~prize_name,
            color = ~gender, text = ~name,
            type = 'scatter', mode = 'markers',
            marker = list(size = 10),
            colors = viridis(3)) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = ""))
  })
  
  output$selection_info <- renderText({
    paste0(
      "Showing: ", nrow(explore_data()), " rows\n",
      "Total: ", nrow(prizes), " rows"
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("prizes_", Sys.Date(), ".csv"),
    content = function(file) write_csv(explore_data(), file)
  )
  
  output$download_excel <- downloadHandler(
    filename = function() paste0("prizes_", Sys.Date(), ".xlsx"),
    content = function(file) {
      writexl::write_xlsx(explore_data(), file)
    }
  )
}

shinyApp(ui, server)

#Sources: Claude was used to help develop this code
