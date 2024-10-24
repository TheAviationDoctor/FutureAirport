# ==============================================================================
#    NAME: scripts/app.R
#   INPUT: Airport and climate data files
# ACTIONS: Run a Shiny dashboard to visualize future climate data by airport
#  OUTPUT: Interactive Shiny dashboard
# RUNTIME: N/A (interactive)
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

# Clear the console
cat("\014")

# Load the airport data
dt_apt <- fread(
  file          = "data/apt/airports.csv",
  header        = TRUE,
  colClasses    = c(rep("character", 3L), rep("numeric", 2L), "factor")
) |>
  setkey(cols   = icao)

# Load the climate data
dt_cli <- fread(
  file          = "data/cli/cli.csv",
  header        = TRUE,
  colClasses    = c(rep("factor", 3L), rep("numeric", 12L))
) |>
  setkey(cols   = icao, ssp, year)

# Define list items for the selectors
choices <- list(
  "apt"  = dt_apt$icao,              # Airports
  "ssp"  = unique(dt_cli$ssp),       # SSPs
  "stat" = colnames(dt_cli)[4:9],    # Statistics
  "key"  = c("abs", "dif")           # Color key
)

# Define display names for the airport dropdown list
names(choices$apt) <- paste(dt_apt$iata, "/", dt_apt$icao, " ", dt_apt$name, sep = "")

# Define display names for the SSP dropdown list
names(choices$ssp) <- c(
  "SSP1 — Sustainability (best-case scenario)",
  "SSP2 — Middle of the road",
  "SSP3 — Regional rivalry",
  "SSP5 — Fossil-fueled development (worst-case scenario)"
)

# Define display names for the statistic dropdown list
names(choices$stat) <- c(
  "Minimum (lowest annual temperature)",
  "1st quartile (25th percentile annual temperature)",
  "Mean (average annual temperature)",
  "Median (50th percentile annual temperature)",
  "3rd quartile (75th percentile annual temperature)",
  "Maximum (highest annual temperature)"
)

# Define display names for the color key radio buttons
names(choices$key) <- c(
  "Predicted temperature for the year",
  "Temperature difference since 2015"
)

# ==============================================================================
# 1 UI layout
# ==============================================================================

ui <- fillPage(

  # ==============================================================================
  # 1.1 CSS and theme
  # ==============================================================================

  # Define a Bootstrap theme using bslib
  theme = bs_theme(version = 5, bootswatch = "cosmo"),

  # Do CSS positioning
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
          width:         600px;
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
    tooltip(
      h6("Select an airport (optional):", bs_icon("info-circle-fill")),
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
    tooltip(
      h6("Select a climate scenario:", bs_icon("info-circle-fill")),
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
    tooltip(
      h6("Select a temperature statistic:", bs_icon("info-circle-fill")),
      "Global warming affects air temperature asymmetrically. It is possible, for example, for the summer maxima to increase faster than the annual mean at some locations. This option lets you explore different statistics individually.",
      placement = "bottom"
    ),
    selectInput(
      inputId  = "stat",
      label    = NULL,
      choices  = choices$stat,
      width    = "100%"
    ),

    # Color key
    tooltip(
      h6("Select color legend:", bs_icon("info-circle-fill")),
      "The color of the dots can either display the absolute temperature at each airport for the observation year, or the amount of change since the year 2015.",
      placement = "bottom"
    ),
    radioButtons(
      inputId  = "key",
      label    = NULL,
      choices  = choices$key,
      inline   = FALSE
    ),

    # Year selector
    tooltip(
      h6("Select an observation year:", bs_icon("info-circle-fill")),
      "The climate model forecasts temperatures up to the year 2100. 2015 is taken as the baseline year for all subsequent observations, which are expressed in degrees Celsius above or below that baseline.",
      placement = "bottom"
    ),
    sliderInput(
      inputId  = "year",
      label    = NULL,
      min      = 2015,
      max      = 2100,
      value    = 2100,
      step     = 1,
      sep      = "",
      width    = "100%"
    ),

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
        choices = c("All", sort(choices$apt))
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

  # Color key
  updateRadioButtons(
    session,
    inputId      = "key",
    label        = NULL,
    choices      = choices$key,
    selected     = choices$key[2],
    inline       = FALSE
  )

  # ==============================================================================
  # 2.2 Filter climate data dynamically based on selector inputs
  # ==============================================================================

  filtered_data <- reactive(
    {
      return(
        dt_cli[
          # Filter by airport, or return all airports
          if(input$airport == "All") icao != input$airport else icao == input$airport
        ][
          # Filter by SSP
          ssp == input$ssp
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
        # setView(
        flyTo(
          lng = 0,
          lat = 50,
          # lng  = if(input$airport == "All")  0 else dt_apt[icao == input$airport, lon],
          # lat  = if(input$airport == "All") 50 else dt_apt[icao == input$airport, lat],
          # zoom = 3
          zoom = if(input$airport == "All") 3 else 4
        ) |>
        # Move zoom controls to top right
        onRender("function(el, x) { L.control.zoom({position:'topright'}).addTo(this); }")
    }
  )

  # Update the map
  observe(
    {
      # Create color palette for temperature
      pal <- colorNumeric(
        palette = "RdYlBu",
        # Narrow down the range of temperature colors based on whether the user wants to display absolute or relative temperatures
        domain = if(input$key == "abs") {
          # If absolute temperatures
          c(min(dt_cli[, get(input$stat)]), max(dt_cli[, get(input$stat)]))
        } else {
          # If relative temperatures
          c(min(dt_cli[, get(paste(input$stat, "dif", sep = "_"))]), max(dt_cli[, get(paste(input$stat, "dif", sep = "_"))]))
        },
        reverse = TRUE
      )
      # Add points with tooltips to the map
      leafletProxy("map", data = filtered_data()) |>
        clearMarkers() |>
        addCircleMarkers(
          # Return all coordinates or just the ones of the selected airport
          lng         = if(input$airport == "All") dt_apt[icao == icao, lon] else dt_apt[icao == input$airport, lon],
          lat         = if(input$airport == "All") dt_apt[icao == icao, lat] else dt_apt[icao == input$airport, lat],
          radius      = if(input$airport == "All") 5L else 10L,
          # Set the dot color to be either the absolute temperature or the temperature difference since 2015
          color       = "black",
          stroke      = TRUE,
          weight      = .75,
          fill        = TRUE,
          fillColor   = pal(if(input$key == "abs") dt_cli[icao == icao & ssp == input$ssp & year == input$year, get(input$stat)] else dt_cli[icao == icao & ssp == input$ssp & year == input$year, get(paste(input$stat, "dif", sep = "_"))]),
          fillOpacity = if(input$airport == "All") 0.8 else 1L,
          # Tooltip
          popup       = paste(
            # Statistic
            names(choices$stat[choices$stat == input$stat]), " at ",
            # Airport
            if(input$airport == "All") {
              paste(dt_apt[icao == icao, name], " (", dt_apt[icao == icao, iata], "/", dt_apt[icao == icao, icao], ")", sep = "")
            } else {
              paste(dt_apt[icao == input$airport, name], " (", dt_apt[icao == input$airport, iata], "/", dt_apt[icao == input$airport, icao], ")", sep = "")
            },
            # Year
            " in ", input$year, " under ",
            # SSP
            substr(x = toupper(input$ssp), start = 1, stop = 4),": ",
            # Absolute temperature
            filtered_data()[icao == icao & year == input$year, get(input$stat)], "°C",
            # Temperature difference
            if(input$year != 2015) paste(" (", sprintf(fmt = "%+.1f", filtered_data()[icao == icao & year == input$year, get(paste(input$stat, "dif", sep = "_"))]), "°C since 2015).", sep = "") else ".",
            sep = ""
          )
        )
    }
  )

}

# Run the app
shinyApp(ui = ui, server = server)