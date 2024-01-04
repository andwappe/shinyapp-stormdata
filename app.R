# Title: Visualizing Storm Data 
# Description: This is a tool for visualizing storm data from the year 1975-2021.
# Details: Used packages Shiny, tidyverse, sf, and rnaturalearth.
# Author: Andrew Wapperom
# Date: 1/4/2024


# ======================================================
# Packages
# ======================================================
library(shiny)
library(tidyverse)      # for syntactic manipulation of tables
library(sf)             # provides classes and functions for vector data
library(rnaturalearth)  # map data sets from Natural Earth


# ======================================================
# Auxiliary objects (that don't depend on input widgets)
# ======================================================
# world map data for ggplot()
world_countries = ne_countries(returnclass = "sf")

# map to be used as canvas
atlantic_map = ggplot(data = world_countries) +
  geom_sf() +
  coord_sf(xlim = c(-110, 0), ylim = c(5, 65))

# ===========================================================
# Define UI for graphing a map, and display data table
# ===========================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Visualizing Storm Data"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets 
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year",
                   label = "Select a Year",
                   sep = "",
                   min = 1975,
                   max = 2021,
                   value = 1975),
      checkboxInput(inputId = "facetByMonth",
                    label = "Facet By Month",
                    value = FALSE),
      checkboxInput(inputId = "wind",
                    label = "Wind Speed",
                   value = FALSE),
      checkboxInput(inputId = "majorHurricanes",
                    label = "Major Hurricanes",
                    value = FALSE),
      checkboxInput(inputId = "pres",
                   label = "pressure",
                   value = FALSE)
    ), # closes sidebarPanel
    
    # -----------------------------------------------------------
    # Main Panel with outputs: plot map of storms, and show table
    # -----------------------------------------------------------
    mainPanel(
      plotOutput(outputId = "plot_map"),
      hr(),
      dataTableOutput(outputId = "summary_table")
    )
  ) # closes sidebarLayout
) # closes fluidPage


# ======================================================
# Server logic to graph the map, and obtain table
# ======================================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Reactive table of filtered storms
  # ------------------------------------------------------------
  tbl = reactive({
    selected_year = input$year
    facet_by_month = input$facetByMonth
    consider_wind_speed = input$wind
    major_hurricanes_only = input$majorHurricanes
    consider_pressure = input$pres
    
    filtered = storms |> filter(year == selected_year) |>
      mutate(id = paste0(name, "-", year)) |> 
      mutate(major_hurricane = factor(case_when(
        is.na(category) ~ FALSE,
        category == 1 ~ FALSE,
        category == 2 ~ FALSE,
        category == 3 ~ TRUE,
        category == 4 ~ TRUE,
        category == 5 ~ TRUE,
      )))
    
    if (major_hurricanes_only) {
      filtered = filtered |> 
        filter(major_hurricane == TRUE)
    }
    filtered
      
  })
  
  
  # ------------------------------------------------------------
  # Map of storms
  # ------------------------------------------------------------
  output$plot_map <- renderPlot({
    selected_year = input$year
    facet_by_month = input$facetByMonth
    consider_wind_speed = input$wind
    major_hurricanes_only = input$majorHurricanes
    consider_pressure = input$pres
    
    if (facet_by_month) {
      p = atlantic_map +
        geom_point(data = tbl(), 
                   aes(x=long, y=lat, color=name)) +
        geom_path(data = tbl(), aes(x=long, y=lat, color=name)) +
        facet_wrap(~month)
        guides(alpha = "none")
    } else {
      # No facet
      p = atlantic_map +
        geom_point(data = tbl(), 
                   aes(x=long, y=lat, color=name)) +
        geom_path(data = tbl(), aes(x=long, y=lat, color=name)) +
        guides(alpha = "none")
    }
    
    if (consider_wind_speed) {
      p = p + 
        geom_path(data = tbl(), aes(x=long, y=lat, color=name, linewidth = wind))
    }
    if (consider_pressure) { 
      p = p + 
        geom_path(data = tbl(), aes(x=long, y=lat, color=name, linewidth = 1/pressure))
    }
    # map output
    p
  })
  
  
  # ----------------------------------------------------------
  # Summary Data Table
  # Contains 5 columns:
  # - name
  # - start date
  # - end date
  # - maximum wind
  # - minimum pressure
  # ----------------------------------------------------------
  output$summary_table <- renderDataTable({
    tbl() |>
      group_by(name) |> 
      summarize(start_date = paste0(first(month), "-", first(day)), 
                end_date = paste0(last(month), "-", last(day)), 
                max_wind = max(wind), 
                min_pressure = min(pressure))
  })
} # closes server


# Run the application 
shinyApp(ui = ui, server = server)
