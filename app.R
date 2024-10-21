# ==============================================================================
#    NAME: scripts/app.R
#   INPUT: Airport and climate data files
# ACTIONS: Run a Shiny dashboard to visualize the climate data by airport
#  OUTPUT: Interactive Shiny dashboard
# RUNTIME: N/A
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2024
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Clear the environment
rm(list = ls())

# Load the required libraries
library(bsicons)
library(bslib)
library(data.table)
library(dplyr)
library(htmlwidgets)
library(leaflet)
library(shiny)
library(shinyBS)
library(shinyjs)

# Define a Bootstrap theme using bslib
# theme = bs_theme(version = 5, bootswatch = "cerulean")  # Example of a custom theme

# Clear the console
cat("\014")

# Load the airport data
dt_apt <- fread(
  file          = "data/apt/airports.csv",
  header        = TRUE,
  colClasses    = c(rep("character", 3L), rep("numeric", 2L), "factor")
) |>
  setkey(cols   = iata)

# Load the climate data
dt_cli <- fread(
  file          = "data/cli/cli.csv",
  header        = TRUE,
  colClasses    = c(rep("factor", 3L), rep("numeric", 12L))
) |>
  setkey(cols   = icao, ssp, year)

# Define list items for the selectors
choices <- list(
  "apt"  = dt_apt$icao,          # Airports
  "ssp"  = unique(dt_cli$ssp),   # SSPs
  "stat" = colnames(dt_cli)[4:9] # Statistics
)

# Define display names for the airport dropdown list
names(choices$apt) <- paste(dt_apt$iata, "/", dt_apt$icao, " ", dt_apt$name, sep = "")

# Define display names for the SSP dropdown list
names(choices$ssp) <- c(
  paste(toupper(substring(choices$ssp[1], 0, 4)), "Sustainability (best-case scenario)",             sep = " - "),
  paste(toupper(substring(choices$ssp[2], 0, 4)), "Middle of the road",                              sep = " - "),
  paste(toupper(substring(choices$ssp[3], 0, 4)), "Regional rivalry",                                sep = " - "),
  paste(toupper(substring(choices$ssp[4], 0, 4)), "Fossil-fueled development (worst-case scenario)", sep = " - ")
)

# Define display names for the statistics dropdown list
names(choices$stat) <- c(
  "Minimum (lowest temperature of the year)",
  "1st quartile (25th percentile temperature of the year)",
  "Mean (average temperature of the year)",
  "Median (50th percentile temperature of the year)",
  "3rd quartile (75th percentile temperature of the year)",
  "Maximum (highest temperature of the year)"
)

# ==============================================================================
# 1 UI layout
# ==============================================================================

ui <- fillPage(

  # ==============================================================================
  # 1.1 CSS
  # ==============================================================================

  tags$head(
    tags$style(
      HTML("
        #controls {
          position:      absolute;
          top:           50px;
          left:          50px;
          background:    rgba(255, 255, 255, 0.75);
          padding:       10px;
          z-index:       1000;
          border-radius: 8px;
          width:         800px;
        }
        .shiny-input-select {
          font-family: 'Courier New', Courier, monospace;
        }
        .info-icon {
          font-size: 16px;
          margin-left: 5px;
          cursor: pointer;
          color: #007BFF;
        }
      ")
    )
  ),

  # ==============================================================================
  # 1.2 Display map
  # ==============================================================================

  leafletOutput("map", width = "100%", height = "100%"),

  # ==============================================================================
  # 1.3 Display controls
  # ==============================================================================

  div(
    id = "controls",

    # Title
    h3("Air temperature at airports worldwide, 2015–2100"),

    # Airport selector
    span("Select an airport (optional):"),
    tooltip(
      span(bs_icon("info-circle-fill")),
      "Optionally, pick one of the ~900 airports worldwide with at least 1M passengers in annual traffic, sorted alphabetically by their IATA code. 'All' will display all airports at once.",
      placement = "bottom"
    ),
    selectInput(
      inputId  = "airport",
      label    = NULL,
      choices  = NULL,
      width    = "100%"
    ),

    # SSP selector
    span("Select a climate scenario:"),
    tooltip(
      span(bs_icon("info-circle-fill")),
      "Shared Socioeconomic Pathways (SSPs) are climate change scenarios defined by the Intergovernmental Panel on Climate Change (IPCC) to standardize climate research. They are based on projected socioeconomic development trajectories up to the year 2100. The IPCC Sixth Report (2021) described SSP2 as likely, hence it is selected as default here.",
      placement = "bottom"
    ),
    selectInput(
      inputId  = "ssp",
      label    = NULL,
      choices  = NULL,
      width    = "100%"
    ),
    
    # Climate statistic selector
    span("Select a temperature statistic:"),
    tooltip(
      span(bs_icon("info-circle-fill")),
      "Global warming affects air temperature in asymmetrical ways. It is possible, for example, for the summer maxima to increase faster than the annual mean at some locations. This option lets you explore different statistics individually.",
      placement = "bottom"
    ),
    selectInput(
      inputId  = "stat",
      label    = NULL,
      choices  = choices$stat,
      width    = "100%"
    ),

    # Year selector
    span("Select an observation year:"),
    tooltip(
      span(bs_icon("info-circle-fill")),
      "2015 is taken as the baseline year for all subsequent observations, which are expressed in degrees Celsius above or below that baseline.",
      placement = "bottom"
    ),
    sliderInput(
      inputId  = "year",
      label    = NULL,
      min      = 2015,
      max      = 2100,
      value    = 2015,
      step     = 1,
      sep      = "",
      width    = "100%"
    ),

    # For debugging only
    htmlOutput("airport"),
    htmlOutput("ssp"),
    htmlOutput("stat"),
    htmlOutput("year"),
    htmlOutput("nrow")
  )
)

# ==============================================================================
# 2 Server logic
# ==============================================================================

server <- function(input, output, session) {

  # ==============================================================================
  # 2.1 Update selectors dynamically
  # ==============================================================================

  # Airport selector
  observe(
    {
      updateSelectInput(
        session,
        inputId = "airport",
        label   = NULL,
        choices = c("All", choices$apt)
      )
    }
  )

  # SSP selector
  observe(
    {
      updateSelectInput(
        session,
        inputId  = "ssp",
        label    = NULL,
        choices  = choices$ssp,
        selected = choices$ssp[2]
      )
    }
  )

  # Climate statistic selector
  observe(
    {
      updateSelectInput(
        session,
        inputId  = "stat",
        label    = NULL,
        choices  = choices$stat,
        selected = choices$stat[3]
      )
    }
  )

  # ==============================================================================
  # 2.2 Filter climate data dynamically based on selector inputs
  # ==============================================================================

  filtered_data <- reactive(
    {

      return(
        dt_cli[
          # Filter by airport
          if (input$airport == "All") icao != input$airport else icao == input$airport
        ][
          # Filter by SSP
          ssp == input$ssp
        ][
          # Filter by year
          year == as.character(input$year)
        ]
      )

    }
  )

  # ==============================================================================
  # 2.3 Render and update the map
  # ==============================================================================

  # Render the map
  output$map <- renderLeaflet(
    {
      leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        addProviderTiles("OpenStreetMap") |>
        setView(
          lng = 0,
          lat = 5,
          # lng = if (input$airport == "All")  0 else ~dt_apt[icao == input$airport, lon],
          # lat = if (input$airport == "All") 50 else ~dt_apt[icao == input$airport, lat],
          zoom = 3
        ) |>
        # Move zoon controls to top right
        onRender("function(el, x) { L.control.zoom({position:'topright'}).addTo(this); }")
    }
  )

  # Update the map
  observe(
    {
      # data <- filtered_data()
      # Create color palette for temperature
      pal <- colorNumeric(palette = "RdYlBu", domain = c(0, 2), reverse = TRUE)
      # Add points to the map
      leafletProxy("map", data = filtered_data()) |>
        clearMarkers() |> 
        addCircleMarkers(
          lng         = if (input$airport == "All") ~dt_apt[icao == icao, lon] else ~dt_apt[icao == input$airport, lon],
          lat         = if (input$airport == "All") ~dt_apt[icao == icao, lat] else ~dt_apt[icao == input$airport, lat],
          radius      = 5,
          # color       = ~pal(mean_dif),
          stroke      = FALSE,
          fillOpacity = 0.75,
          popup       = ~paste(
            dt_apt[icao == icao, name],
            " (", dt_apt[icao == icao, iata], "/", dt_apt[icao == icao, icao], ")<br />",
            names(choices$stat[choices$stat == input$stat]),
            " in ", input$year, ": ",
            filtered_data()[icao == icao & year == input$year & ssp == input$ssp, get(input$stat)], "°C",
            if(input$year != 2015) paste("<br />", "Change since 2015: ", if (filtered_data()[, get(paste(input$stat, "dif", sep = "_"))] > 0) "+", filtered_data()[, get(paste(input$stat, "dif", sep = "_"))], "°C", sep = ""),
            sep = ""
          )
        )
    }
  )

  # For debugging only
  output$airport <- renderText(input$airport)
  output$ssp     <- renderText(input$ssp)
  output$stat    <- renderText(input$stat)
  output$year    <- renderText(input$year)
  # output$nrow    <- renderText(nrow(filtered_data()))
  output$nrow    <- renderText(length(unlist(filtered_data()[get(input$stat)])))
}

# Run the app
# shinyApp(ui, server)
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))