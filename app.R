library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)

# Load data
disaster_data <- read_xlsx("F:/Code/DataSci2/dataset.xlsx")

# Clean column names for easier access
# Note: We'll keep the original names to match the dataset exactly

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Global Disaster Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main Map", tabName = "map", icon = icon("globe")),
      menuItem("Statistics", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Data Table", tabName = "table", icon = icon("table"))
    ),
    
    # Statistics in sidebar
    h4("Quick Stats", style = "color: white; margin-left: 15px;"),
    
    valueBoxOutput("total_disasters", width = 12),
    valueBoxOutput("total_deaths", width = 12),
    valueBoxOutput("total_affected", width = 12),
    
    # Filters
    h4("Filters", style = "color: white; margin-left: 15px;"),
    
    selectInput("disaster_type_filter", 
                "Disaster Type:",
                choices = c("All" = "all", sort(unique(disaster_data$`Disaster Type`))),
                selected = "all"),
    
    selectInput("country_filter", 
                "Country:",
                choices = c("All" = "all", sort(unique(disaster_data$Country))),
                selected = "all"),
    
    sliderInput("year_range", 
                "Year Range:",
                min = min(disaster_data$`Start Year`, na.rm = TRUE),
                max = max(disaster_data$`Start Year`, na.rm = TRUE),
                value = c(min(disaster_data$`Start Year`, na.rm = TRUE), 
                          max(disaster_data$`Start Year`, na.rm = TRUE)),
                step = 1,
                sep = "")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: #f4f4f4;
      }
    "))),
    
    tabItems(
      # Map tab
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "Global Disaster Map", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  height = "600px",
                  leafletOutput("disaster_map", height = "550px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Disaster Timeline", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("timeline_plot")
                )
              )
      ),
      
      # Statistics tab
      tabItem(tabName = "stats",
              fluidRow(
                box(
                  title = "Disasters by Type", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("disaster_type_plot")
                ),
                
                box(
                  title = "Impact by Country", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("country_impact_plot")
                )
              ),
              
              fluidRow(
                box(
                  title = "Yearly Trend", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("yearly_trend_plot")
                ),
                
                box(
                  title = "Regional Distribution", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("regional_plot")
                )
              )
      ),
      
      # Data table tab
      tabItem(tabName = "table",
              fluidRow(
                box(
                  title = "Disaster Data", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("disaster_table")
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    data <- disaster_data
    
    # Filter by disaster type
    if (input$disaster_type_filter != "all") {
      data <- data[data$`Disaster Type` == input$disaster_type_filter, ]
    }
    
    # Filter by country
    if (input$country_filter != "all") {
      data <- data[data$Country == input$country_filter, ]
    }
    
    # Filter by year range
    data <- data[!is.na(data$`Start Year`) & 
                   data$`Start Year` >= input$year_range[1] & 
                   data$`Start Year` <= input$year_range[2], ]
    
    return(data)
  })
  
  # Value boxes in sidebar
  output$total_disasters <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Total Disasters",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$total_deaths <- renderValueBox({
    deaths <- sum(filtered_data()$`Total Deaths`, na.rm = TRUE)
    valueBox(
      value = format(deaths, big.mark = ","),
      subtitle = "Total Deaths",
      icon = icon("users"),
      color = "yellow"
    )
  })
  
  output$total_affected <- renderValueBox({
    affected <- sum(filtered_data()$`Total Affected`, na.rm = TRUE)
    valueBox(
      value = format(affected, big.mark = ","),
      subtitle = "People Affected",
      icon = icon("home"),
      color = "blue"
    )
  })
  
  # Main map
  output$disaster_map <- renderLeaflet({
    data <- filtered_data()
    
    # Remove rows with missing coordinates
    data <- data[!is.na(data$Latitude) & !is.na(data$Longitude), ]
    
    if (nrow(data) == 0) {
      leaflet() %>% addTiles() %>%
        addControl("No data available for current filters", position = "topright")
    } else {
      # Color palette for disaster types
      unique_types <- unique(data$`Disaster Type`)
      colors <- rainbow(length(unique_types))
      pal <- colorFactor(palette = colors, domain = unique_types)
      
      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          radius = ~pmax(3, pmin(15, sqrt(`Total Deaths` + 1) * 0.5)),
          color = ~pal(`Disaster Type`),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          popup = ~paste(
            "<strong>", ifelse(is.na(`Event Name`), "Unnamed Event", `Event Name`), "</strong><br>",
            "Country: ", Country, "<br>",
            "Type: ", `Disaster Type`, "<br>",
            "Year: ", `Start Year`, "<br>",
            "Deaths: ", ifelse(is.na(`Total Deaths`), "Unknown", format(`Total Deaths`, big.mark = ",")), "<br>",
            "Affected: ", ifelse(is.na(`Total Affected`), "Unknown", format(`Total Affected`, big.mark = ",")), "<br>",
            "Damage: $", ifelse(is.na(`Total Damage`), "Unknown", 
                                paste0(format(`Total Damage`, big.mark = ","), "K"))
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~`Disaster Type`,
          title = "Disaster Type",
          position = "bottomright"
        )
    }
  })
  
  # Timeline plot
  output$timeline_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6) +
        theme_void()
    } else {
      # Aggregate by year
      yearly_data <- data %>%
        group_by(`Start Year`, `Disaster Type`) %>%
        summarise(
          Count = n(),
          Total_Deaths = sum(`Total Deaths`, na.rm = TRUE),
          .groups = 'drop'
        )
      
      p <- ggplot(yearly_data, aes(x = `Start Year`, y = Count, color = `Disaster Type`)) +
        geom_point(aes(size = Total_Deaths), alpha = 0.7) +
        geom_line(alpha = 0.5) +
        scale_size_continuous(range = c(2, 8), name = "Deaths") +
        labs(title = "Disaster Timeline",
             x = "Year",
             y = "Number of Disasters",
             color = "Disaster Type") +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  # Disasters by type plot
  output$disaster_type_plot <- renderPlotly({
    data <- filtered_data() %>%
      count(`Disaster Type`, sort = TRUE)
    
    if (nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6) +
        theme_void()
    } else {
      p <- ggplot(data, aes(x = reorder(`Disaster Type`, n), y = n, fill = `Disaster Type`)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Number of Disasters by Type",
             x = "Disaster Type",
             y = "Count") +
        theme_minimal() +
        theme(legend.position = "none")
    }
    
    ggplotly(p)
  })
  
  # Country impact plot
  output$country_impact_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Country) %>%
      summarise(Total_Affected = sum(`Total Affected`, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total_Affected)) %>%
      head(10)
    
    if (nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6) +
        theme_void()
    } else {
      p <- ggplot(data, aes(x = reorder(Country, Total_Affected), y = Total_Affected, fill = Country)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Top 10 Countries by People Affected",
             x = "Country",
             y = "People Affected") +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_y_continuous(labels = scales::comma_format())
    }
    
    ggplotly(p)
  })
  
  # Yearly trend plot
  output$yearly_trend_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(`Start Year`) %>%
      summarise(
        Count = n(),
        Total_Deaths = sum(`Total Deaths`, na.rm = TRUE),
        Total_Affected = sum(`Total Affected`, na.rm = TRUE),
        .groups = 'drop'
      )
    
    if (nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6) +
        theme_void()
    } else {
      p <- ggplot(data, aes(x = `Start Year`)) +
        geom_line(aes(y = Count, color = "Number of Disasters"), size = 1) +
        geom_point(aes(y = Count, color = "Number of Disasters"), size = 2) +
        labs(title = "Yearly Disaster Trends",
             x = "Year",
             y = "Number of Disasters",
             color = "Metric") +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  # Regional distribution plot
  output$regional_plot <- renderPlotly({
    data <- filtered_data() %>%
      count(Region, sort = TRUE)
    
    if (nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6) +
        theme_void()
    } else {
      p <- ggplot(data, aes(x = "", y = n, fill = Region)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Disasters by Region") +
        theme_void() +
        theme(legend.position = "right")
    }
    
    ggplotly(p)
  })
  
  # Data table
  output$disaster_table <- DT::renderDataTable({
    data <- filtered_data()
    
    # Select key columns for display
    display_data <- data %>%
      select(
        `...1`, 
        `Disaster Type`, 
        Country, 
        `Start Year`, 
        `Total Deaths`, 
        `Total Affected`, 
        `Total Damage`
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        columnDefs = list(
          list(className = 'dt-center', targets = 3:6)
        )
      ),
      filter = 'top',
      colnames = c(
        'Disaster ID' = '...1',
        'Type' = 'Disaster Type',
        'Country' = 'Country',
        'Year' = 'Start Year',
        'Deaths' = 'Total Deaths',
        'Affected' = 'Total Affected',
        'Damage (000 USD)' = 'Total Damage'
      )
    ) %>%
      DT::formatCurrency(c('Damage (000 USD)'), currency = "$", digits = 0) %>%
      DT::formatCurrency(c('Affected'), currency = "", digits = 0, mark = ",")
  })
}

# Run the application
shinyApp(ui = ui, server = server)