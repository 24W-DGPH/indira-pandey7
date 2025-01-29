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
  rsconnect
)


### Data Cleaning & Preparation

# Check if dataset exists before loading
if (!file.exists("linelist_raw.csv")) {
  stop("Error: Missing 'linelist_raw.csv'. Upload it before running the app.")
}

# Import dataset
linelist_raw <- read.csv("linelist_raw.csv", stringsAsFactors = FALSE)

# Ensure file has data before proceeding
if (ncol(linelist_raw) == 0) {
  stop("Error: 'linelist_raw.csv' is empty or missing column names.")
} else {
  linelist_raw <- clean_names(linelist_raw)
}

# Rename columns safely
if ("new_hiv_infections_children_aged_0_14_children_0_14_estimate" %in% colnames(linelist_raw)) {
  linelist_raw <- linelist_raw %>%
    rename(new_infection_children = new_hiv_infections_children_aged_0_14_children_0_14_estimate)
} else {
  stop("Error: Expected column 'new_hiv_infections_children_aged_0_14_children_0_14_estimate' not found in dataset.")
}

# Convert columns to appropriate data types
linelist_cleaned <- linelist_raw %>%
  mutate(
    year = as.numeric(year),
    code = as.character(code),
    new_infection_children = as.numeric(new_infection_children)
  )

# Map country codes to country names
linelist_cleaned <- linelist_cleaned %>%
  mutate(country = countrycode(code, "iso3c", "country.name"))

# Remove rows with missing values and filter valid years
linelist_cleaned <- linelist_cleaned %>%
  filter(!is.na(year), !is.na(code), !is.na(new_infection_children), year >= 2010, year <= 2016)

# Remove duplicates
linelist_cleaned <- linelist_cleaned %>% distinct(year, code, .keep_all = TRUE)

# Save cleaned dataset
write.csv(linelist_cleaned, "linelist_cleaned.csv", row.names = FALSE)

# Check if world map file exists
if (!file.exists("world_map.rds")) {
  stop("Error: Missing 'world_map.rds'. Upload it before running the app.")
}
world <- readRDS("world_map.rds")

### Load Processed Data

# Check if cleaned dataset exists before loading
if (!file.exists("linelist_cleaned.csv")) {
  stop("Error: Missing 'linelist_cleaned.csv'. Ensure it's generated before running the app.")
}

global_data <- read.csv("linelist_cleaned.csv", stringsAsFactors = FALSE)

# Ensure file has valid column names before applying clean_names()
if (ncol(global_data) == 0) {
  stop("Error: 'linelist_cleaned.csv' is empty or missing column names.")
} else {
  global_data <- clean_names(global_data)
}

global_data <- global_data %>%
  mutate(country = countrycode(code, "iso3c", "country.name"))

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
