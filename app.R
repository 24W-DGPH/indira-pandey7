### Load necessary libraries
pacman::p_load(
  shiny,
  leaflet,
  ggplot2,
  janitor,
  dplyr,
  rnaturalearth,
  sf,
  countrycode,
  rsconnect,
  renv
)


source("clean.R")
source("visualize.R")

# UI Layout
ui <- fluidPage(
  titlePanel("New HIV Cases Map and Trends (2010-2016)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = sort(unique(global_data$year)), selected = max(global_data$year, na.rm = TRUE)),
      selectInput("country", "Select Country:", choices = c("Global", sort(unique(global_data$country[!is.na(global_data$country)]))), selected = "Global"),
      plotOutput("time_series_plot", height = "400px")
    ),
    mainPanel(
      leafletOutput("hiv_map", height = "600px")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$year)
    global_data %>% filter(year == input$year)
  })
  
  time_series_data <- reactive({
    if (input$country == "Global") {
      global_data %>%
        group_by(year) %>%
        summarise(new_infection_children = sum(new_infection_children, na.rm = TRUE))
    } else {
      global_data %>%
        filter(country == input$country) %>%
        group_by(year) %>%
        summarise(new_infection_children = sum(new_infection_children, na.rm = TRUE))
    }
  })
  
  output$hiv_map <- renderLeaflet({
    req(filtered_data())
    map_data <- world %>% left_join(filtered_data(), by = c("iso_a3" = "code"))
    
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", new_infection_children, na.color = "transparent")(new_infection_children),
        weight = 1, color = "black", fillOpacity = 0.7,
        label = ~paste0("<strong>Country:</strong> ", name, "<br><strong>New Cases:</strong> ", ifelse(is.na(new_infection_children), "No Data", new_infection_children)) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(weight = 4, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend(pal = colorNumeric("YlOrRd", domain = filtered_data()$new_infection_children), values = filtered_data()$new_infection_children, title = "New HIV Cases", position = "bottomleft")
  })
  
  output$time_series_plot <- renderPlot({
    req(time_series_data())
    ggplot(time_series_data(), aes(x = year, y = new_infection_children)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(
        title = if (input$country == "Global") "Global Trend of New HIV Cases" else paste("Trend of New HIV Cases in", input$country),
        x = "Year", y = "New HIV Cases"
      )
  })
}

# Run App
shinyApp(ui, server)
