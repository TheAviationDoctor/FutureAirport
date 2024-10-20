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
  "apt" = dt_apt$icao,          # Airports
  "ssp" = unique(dt_cli$ssp),   # SSPs
  "sta" = colnames(dt_cli)[4:9] # Statistic
)

# Define display names for the list items
# Airports
names(choices$apt) <- paste(dt_apt$iata, "/", dt_apt$icao, " ", dt_apt$name, sep = "")
# SSPs
names(choices$ssp) <- c(
  paste(toupper(substring(choices$ssp[1], 0, 4)), "Sustainability (best-case scenario)",             sep = " - "),
  paste(toupper(substring(choices$ssp[2], 0, 4)), "Middle of the road",                              sep = " - "),
  paste(toupper(substring(choices$ssp[3], 0, 4)), "Regional rivalry",                                sep = " - "),
  paste(toupper(substring(choices$ssp[4], 0, 4)), "Fossil-fueled development (worst-case scenario)", sep = " - ")
)
# Statistics
names(choices$sta) <- c(
  "Minimum", "1st quartile", "Mean", "Median", "3rd quartile", "Maximum"
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
    h3("Surface air temperature at airports worldwide, 2015–2100"),

    # Airport selector
    span("Select an airport:"),
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
    span("Select which statistic to display:"),
    tooltip(
      span(bs_icon("info-circle-fill")),
      "By default, the map displays changes in the mean (average) annual surface air temperature at the selected airport(s) relative to the 2015 baseline. However, you can also choose to display the annual minimum, first quartile (25th percentile), median (50th percentile), third quartile (75th percentile), or maximum instead. It is possible, for example, that the temperature maxima increase after than the mean over time at some locations.",
      placement = "bottom"
    ),
    radioButtons(
      inputId  = "sta",
      label    = NULL,
      choices  = choices$sta,
      inline   = TRUE,
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
    htmlOutput("sta"),
    htmlOutput("year"),
    htmlOutput("dat")
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
        inputId = "ssp",
        label   = NULL,
        choices = choices$ssp,
        selected = choices$ssp[2]
      )
    }
  )

  # Climate statistic selector
  observe(
    {
      updateRadioButtons(
        session,
        inputId  = "sta",
        label    = NULL,
        choices  = choices$sta,
        selected = choices$sta[3],
        inline   = TRUE
      )
    }
  )

  # ==============================================================================
  # 2.2 Filter climate data dynamically based on selector inputs
  # ==============================================================================

  filtered_data <- reactive(
    {
      
      # # WORKING START
      # # Filter by year
      # data <- dt_cli |> filter(year == as.character(input$year))
      # 
      # # Filter by airport
      # if (input$airport != "All") { data <- data |> filter(icao == input$airport) }
      # 
      # return(data)
      # # WORKING END

      return(
        dt_cli |>
          # Filter by airport
          filter(if (input$airport == "All") icao != input$airport else icao == input$airport) |>
          # Filter by SSP
          filter(ssp == input$ssp) |> 
          # Filter by year
          filter(year == as.character(input$year)) |>
          # Select only data columns for the chosen climate statistic
          select(icao, any_of(c(input$sta, paste(input$sta, "dif", sep = "_"))))
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
        setView(lng = 0, lat = 50, zoom = 3) |>
        # Move zoon controls to top right
        onRender("function(el, x) { L.control.zoom({position:'topright'}).addTo(this); }")
    }
  )

  # Update the map
  observe(
    {
      data <- filtered_data()
      # Create color palette for temperature
      pal <- colorNumeric(palette = "RdYlBu", domain = c(0, 2), reverse = TRUE)
      # Add points to the map
      leafletProxy("map", data = data) |>
        clearMarkers() |> 
        addCircleMarkers(
          lng         = ~dt_apt[icao == icao, lon],
          lat         = ~dt_apt[icao == icao, lat],
          radius      = 5,
          # color       = ~pal(mean_dif),
          # color       = ~pal(mean_dif),
          stroke      = FALSE,
          fillOpacity = 0.8,
          popup       = ~paste("ICAO:", icao, "<br>", "Name:", icao)
        )
    }
  )
  
  # For debugging only
  output$airport <- renderText(input$airport)
  output$ssp     <- renderText(input$ssp)
  output$sta     <- renderText(input$sta)
  output$dat     <- renderText(unlist(filtered_data()))
}

# Run the app
# shinyApp(ui, server)
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))