# ==== Colophon ====
#    NAME: scripts/app.R
#   INPUT: Airport and climate data files
# ACTIONS: Run a Shiny dashboard to visualize future climate data by airport
#  OUTPUT: Interactive Shiny dashboard
# RUNTIME: N/A (interactive)
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2024

# ==== 0 Housekeeping ====

# Clear the environment
rm(list = ls())

# Load the required libraries
library(bsicons)
library(bslib)
library(data.table)
library(dplyr)
library(htmlwidgets)
library(leaflet)
library(plotly)
library(RColorBrewer)
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
  colClasses    = c(rep("factor", 4L), rep("numeric", 12L))
) |>
  setkey(cols   = icao, var, ssp, year)

# Define list items for the selectors
choices <- list(
  "apt"  = dt_apt$icao,                                               # Airports
  "ssp"  = unique(dt_cli$ssp),                                        # SSPs
  "var"  = unique(dt_cli$var),                                        # Variables
  "key"  = c("abs", "dif"),                                           # Color key
  "stat" = sub("abs_", "", names(select(dt_cli, starts_with("abs")))) # Statistics
)

# Define display names for the airports
names(choices$apt) <- paste(dt_apt$iata, "/", dt_apt$icao, " ", dt_apt$name, sep = "")

# Define display names for the SSPs
names(choices$ssp) <- c(
  "SSP1 — Sustainability (best-case scenario)",
  "SSP2 — Middle of the road",
  "SSP3 — Regional rivalry",
  "SSP5 — Fossil-fueled development (worst-case scenario)"
)

# Define display names for the variables
names(choices$var) <- c(
  "Relative humidity",
  "Air pressure",
  "Air temperature"
)

# Define display names for the statistics
names(choices$stat) <- c(
  "Minimum (lowest annual value)",
  "1st quartile (25th percentile annual value)",
  "Mean (average annual value)",
  "Median (50th percentile annual value)",
  "3rd quartile (75th percentile annual value)",
  "Maximum (highest annual value)"
)

# Define display names for the color keys
names(choices$key) <- c(
  "Predicted value for the year",
  "Change in value since 2015"
)

# FOR DEBUGGING ONLY
input <- list()
input$apt  <- "All"
input$ssp  <- "ssp245"
input$var  <- "tas"
input$stat <- "mean"
input$key  <- "abs"
input$year <- 2100

# ==== 1 UI layout ====

ui <- fillPage(

  # Do CSS positioning
  tags$head(
    tags$style(
      HTML("
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

  sidebarLayout(

    # ==== 1.1 Sidebar panel ====

    sidebarPanel(
      width = 3,
      # style = "position: fixed; height: 100%;",

      # Bootstrap theme
      theme = bs_theme(version = 5, bootswatch = "cosmo"),

      # Title
      h3("Climate change at airports worldwide, 2015–2100"),

      hr(),
  
      # Airport selector
      tooltip(
        h6("Select an airport (optional):", bs_icon("info-circle-fill")),
        "Optionally, pick one of the ~900 airports worldwide with at least 1M passengers in annual traffic, sorted alphabetically by their IATA code. 'All' will display all airports at once.",
        placement = "bottom"
      ),
      selectInput(
        inputId  = "apt",
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
      
      # Climate variable selector
      tooltip(
        h6("Select a climate variable:", bs_icon("info-circle-fill")),
        "Choose whether to display the predicted near-surface air temperature, air pressure, or relative humidity at the airports.",
        placement = "bottom"
      ),
      selectInput(
        inputId  = "var",
        label    = NULL,
        choices  = choices$var,
        width    = "100%"
      ),
      
      # Climate statistic selector
      tooltip(
        h6("Select a statistic:", bs_icon("info-circle-fill")),
        "Climate change affects different statistics asymmetrically. It is possible, for example, for the annual maximum to change faster than the annual mean at some locations. This option lets you explore different statistics individually. Choose the mean if you are unsure.",
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

      hr(),

      # For debugging only
      htmlOutput("apt"),
      htmlOutput("ssp"),
      htmlOutput("var"),
      htmlOutput("stat"),
      htmlOutput("key"),
      htmlOutput("year"),
      htmlOutput("pal")
    ),

    # ==== 1.2 Main panel ====

    mainPanel(
      width = 9,
      leafletOutput("map", height = "100%"),
      # DT::dataTableOutput("mytable")
    )
  )

) # End ui

# ==== 2 Server logic ====

server <- function(input, output, session) {

  # ==== 2.1 Update selectors dynamically ====

  # Update airport choices
  observe(
    {
      updateSelectInput(
        session,
        inputId = "apt",
        label   = NULL,
        choices = c("All", sort(choices$apt))
      )
    }
  )

  # Update SSP choices
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

  # Update climate variable choices
  observe(
    {
      updateSelectInput(
        session,
        inputId  = "var",
        label    = NULL,
        choices  = choices$var,
        selected = choices$var[3]
      )
    }
  )

  # Update climate statistic choices
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

  # Update color key choices
  observe(
    {
      updateRadioButtons(
        session,
        inputId      = "key",
        label        = NULL,
        choices      = choices$key,
        selected     = choices$key[2],
        inline       = FALSE
      )
    }
  )

  # ==== 2.2 Filter data ====

  # Filter map data
  filtered_data <- reactive(
    {
      # req(input$ssp, input$var, input$year) # Check if user inputs are available
      dt_cli[
        ssp  == input$ssp  &
        var  == input$var  &
        year == input$year &
        if(input$apt %in% icao) icao == input$apt else icao != input$apt
      ]
    }
  )

  output$mytable = DT::renderDataTable({
    filtered_data()
  })

  # ==== 2.3 Render the map ====

  # Create color palette based on values
  color_pal <- reactive(
    {
      colorBin(
        palette = "RdYlBu",
        domain = filtered_data()[, get(paste(input$key, input$stat, sep = "_"))],
        reverse = TRUE
      )
    }
  )

  # Render the map
  output$map <- renderLeaflet(
    {
      map <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        addProviderTiles("OpenStreetMap") |>
        addCircleMarkers(
                data        = filtered_data(),
                lng         = dt_apt[icao == icao, lon],
                lat         = dt_apt[icao == icao, lat],
                radius      = 8,
                fillColor   = ~color_pal()(get(paste(input$key, input$stat, sep = "_"))),
                color       = "black",
                weight      = 1,
                opacity     = 1,
                fillOpacity = 0.7,
        ) |>
        # Move zoom controls to top right
        onRender("function(el, x) { L.control.zoom({position:'topright'}).addTo(this); }")
    }
  )

  # # Render the map
  # output$map <- renderLeaflet(
  #   {
  #     leaflet(options = leafletOptions(zoomControl = FALSE)) |>
  #     addProviderTiles(providers$CartoDB.Positron) |>
  #     addCircleMarkers(
  #       data        = filtered_data(),
  #       lng         = dt_apt[icao == icao, lon],
  #       lat         = dt_apt[icao == icao, lat],
  #       radius      = 8,
  #       fillColor   = ~color_pal()(get(paste(input$key, input$stat, sep = "_"))),
  #       color       = "black",
  #       weight      = 1,
  #       opacity     = 1,
  #       fillOpacity = 0.7,
  #     )
  #   }
  # )

  # Update the map
  observe(
    {
      # Add points with tooltips to the map
      leafletProxy("map", data = map_data()) |>
        clearMarkers() |>
        addCircleMarkers(
          # Return all coordinates or just the ones of the selected airport
          lng         = if(input$apt %in% dt_apt[, icao]) dt_apt[icao == input$apt, lon] else dt_apt[icao == icao, lon],
          lat         = if(input$apt %in% dt_apt[, icao]) dt_apt[icao == input$apt, lat] else dt_apt[icao == icao, lat],
          radius      = if(input$apt %in% dt_apt[, icao]) 10L else 5L,
          # Set the dot color to be either the absolute temperature or the temperature difference since 2015
          color       = "black",
          stroke      = TRUE,
          weight      = .75,
          fill        = TRUE,
          # fillColor   = pal(dt_cli[icao == icao & ssp == input$ssp & var == input$var & year == input$year, get(paste(input$key, input$stat, sep = "_"))]),
          fillOpacity = if(input$apt %in% dt_apt[, icao]) 1L else .8,
          # Tooltip
          popupOptions(keepInView = TRUE, closeOnClick = NULL),
        )
    }
  )
  

  # 
  # # Update the map when an airport is selected
  # observe({
  #   req(input$airport, filtered_data())
  #   
  #   if (input$airport != "all") {
  #     df <- filtered_data()
  #     leafletProxy("map") %>%
  #       setView(
  #         lng = df$lon[1],
  #         lat = df$lat[1],
  #         zoom = 8
  #       )
  #   } else {
  #     leafletProxy("map") %>%
  #       setView(
  #         lng = 0,
  #         lat = 20,
  #         zoom = 2
  #       )
  #   }
  # })

      # For debugging only
      output$apt  <- renderText(input$apt)
      output$ssp  <- renderText(input$ssp)
      output$var  <- renderText(input$var)
      output$stat <- renderText(input$stat)
      output$key  <- renderText(input$key)
      output$year <- renderText(input$year)
      output$pal <- renderText(filtered_data()[, get(paste(input$key, input$stat, sep = "_"))])


}

# Run the app
shinyApp(ui = ui, server = server)