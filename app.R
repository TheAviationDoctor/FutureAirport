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

# Clear the console
cat("\014")

# Load the required libraries
library(bsicons)
library(bslib)
library(data.table)
library(htmltools)
library(leaflet)
library(shiny)
library(shinyjs)

# Load the airport data
dt_apt <- fread(
  file           = "data/apt/airports.csv",
  header         = TRUE,
  colClasses     = c(rep("character", 3L), rep("numeric", 2L), "factor")
) |> setkey(cols = icao)

# Load the climate data
dt_cli <- fread(
  file           = "data/cli/cli.csv",
  header         = TRUE,
  colClasses     = c(rep("factor", 4L), rep("numeric", 12L))
) |> setkey(cols = icao, var, ssp, year)

# Initialize a list for the user choices
choices <- list("apt" = dt_apt$icao)

# Define display names for the airports
names(choices$apt) <- paste(dt_apt$iata, "/", dt_apt$icao, " ", dt_apt$name, sep = "")

# Define display names for the SSPs
choices$ssp <- c(
  "SSP1 — Sustainability (best-case scenario)"             = "ssp126",
  "SSP2 — Middle of the road"                              = "ssp245",
  "SSP3 — Regional rivalry"                                = "ssp370",
  "SSP5 — Fossil-fueled development (worst-case scenario)" = "ssp585"
)

# Define display names for the variables
choices$var <- c(
  "Relative humidity" = "hurs",
  "Air pressure"      = "ps",
  "Air temperature"   = "tas"
)

# Define display names for the variables
choices$units <- c(
  "hurs" = "%",
  "ps"   = " Pa",
  "tas"  = "°C"
)

# Define display names for the statistics
choices$stat <- c(
  "Minimum (lowest annual value)"               = "min",
  "1st quartile (25th percentile annual value)" = "lq",
  "Mean (average annual value)"                 = "mean",
  "Median (50th percentile annual value)"       = "median",
  "3rd quartile (75th percentile annual value)" = "uq",
  "Maximum (highest annual value)"              = "max"
)

# Define display names for the color keys
choices$key <- c(
  "Predicted value for the year" = "abs",
  "Change in value since 2015"   = "dif"
)

# # FOR DEBUGGING ONLY
# input      <- list()
# input$apt  <- "All"
# input$ssp  <- "ssp245"
# input$var  <- "tas"
# input$stat <- "mean"
# input$key  <- "abs"
# input$year <- 2100

# ==== 1 UI layout ====

ui <- fillPage(
  
  # ==== 1.1 Styling ====
  
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  tags$head(tags$style(HTML("
    .bi-info-circle-fill { font-size: 14px; margin-left: 5px; cursor: pointer; color: #2780E3; }
    .row, .well          { height: 100%;} # Side and main panels height
    .shiny-input-select  { font-family: 'Courier New', Courier, monospace; }
    .col-sm-3            { padding-right: 0px; }
    .col-sm-9            { padding: 0px; }
  "))),
  
  sidebarLayout(
    
    # ==== 1.2 Display sidebar panel with selectors ====
    
    sidebarPanel(
      width = 3,
      h3("Climate change at airports worldwide, 2015–2100"),
      hr(),
      # Airport selector
      tooltip(
        h6("Select or click on an airport (optional):", bs_icon("info-circle-fill")), "Optionally, pick one of the ~900 airports worldwide with at least 1M passengers in annual traffic, sorted alphabetically by their IATA code. 'All' will display all airports at once.",
        placement = "bottom"
      ),
      selectInput(
        inputId  = "apt",
        label    = NULL,
        choices  = NULL,
        width    = "100%"
      ),
      # SSP selector
      tooltip(h6("Select a climate scenario:", bs_icon("info-circle-fill")), "Shared Socioeconomic Pathways (SSPs) are climate change scenarios defined by the Intergovernmental Panel on Climate Change (IPCC) to standardize climate research. They are based on projected socioeconomic development trajectories up to the year 2100. The IPCC Sixth Report (2021) described SSP2 as likely, hence it is selected as default here."),
      selectInput(
        inputId  = "ssp",
        label    = NULL,
        choices  = NULL,
        width    = "100%"
      ),
      # Climate variable selector
      tooltip(h6("Select a climate variable:", bs_icon("info-circle-fill")), "Choose whether to display the predicted near-surface air temperature, air pressure, or relative humidity at the airports."),
      selectInput(
        inputId  = "var",
        label    = NULL,
        choices  = choices$var,
        width    = "100%"
      ),
      # Climate statistic selector
      tooltip(h6("Select a statistic:", bs_icon("info-circle-fill")), "Climate change affects different statistics asymmetrically. It is possible, for example, for the annual maximum to change faster than the annual mean at some locations. This option lets you explore different statistics individually. Choose the mean if you are unsure."),
      selectInput(
        inputId  = "stat",
        label    = NULL,
        choices  = choices$stat,
        width    = "100%"
      ),
      # Color key
      tooltip(h6("Select color legend:", bs_icon("info-circle-fill")), "The color of the dots can either display the absolute temperature at each airport for the observation year, or the amount of change since the year 2015."),
      radioButtons(
        inputId  = "key",
        label    = NULL,
        choices  = choices$key,
        inline   = FALSE
      ),
      # Year selector
      tooltip(h6("Select an observation year:", bs_icon("info-circle-fill")), "The climate model forecasts temperatures up to the year 2100. 2015 is taken as the baseline year for all subsequent observations, which are expressed in degrees Celsius above or below that baseline."),
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
      
      # # For debugging only
      # htmlOutput("apt"),
      # htmlOutput("ssp"),
      # htmlOutput("var"),
      # htmlOutput("stat"),
      # htmlOutput("key"),
      # htmlOutput("year"),
      # htmlOutput("selected"),
      # DT::DTOutput("table")
    ),
    
    # ==== 1.3 Display main panel with map ====
    
    mainPanel(width = 9, leafletOutput("map", height = "100%"))
  )
)

# ==== 2 Server logic ====

server <- function(input, output, session) {
  
  # ==== 2.1 Update input values from selectors ====
  
  # Airport selector
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
  # SSP selector
  observe(
    {
      updateSelectInput(
        session,
        inputId  = "ssp",
        label    = NULL,
        choices  = choices$ssp,
        selected = choices$ssp[3]
      )
    }
  )
  # Climate variable selector
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
  # Color key selector
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
  
  # ==== 2.2 Filter data for SSP, variable, and year, based on selector inputs (always returns all airports) ====
  
  dt_map <- reactive(
    {
      dt_cli[
        ssp  == input$ssp &
          var  == input$var &
          year == input$year,
        .(
          icao = icao,
          ssp  = ssp,
          var  = var,
          year = year,
          abs  = get(paste("abs", input$stat, sep = "_")),
          dif  = get(paste("dif", input$stat, sep = "_"))
        )
      ][
        dt_apt, on = "icao" # Merge with airport data table
      ][,
        label := paste(                                                                 # Assemble hover label
          "<b>", name, " (", iata, "/", icao, ") ",                                     # Airport
          " in ", year,                                                                 # Year
          " under ", substr(x = toupper(input$ssp), start = 1, stop = 4), ":</b></br>", # SSP
          names(choices$stat[choices$stat == input$stat]), " ",                         # Statistic
          tolower(names(choices$var[choices$var == input$var])), ": ",                  # Variable
          "<b>", sprintf(fmt = "%.2f", abs), choices$units[[input$var]], "</b>.</br>",  # Predicted value for the year
          # Change in value since 2015
          if(input$year > 2015) paste("Change since 2015: <b>", sprintf(fmt = "%+.2f", dif), if(choices$units[[input$var]] == "%") " p.p" else choices$units[[input$var]], "</b>.", sep = ""), sep = "")
      ]
    }
  )
  
  # ==== 2.3 Render the base map ====
  
  output$map <- renderLeaflet(
    {
      leaflet(data = dt_map()) |>
        addProviderTiles(providers$CartoDB.Positron)
    }
  )
  
  # ==== 2.4 Observe filtered data and update the circle markers' color ====
  
  observe(
  # observeEvent(
    # input$ssp,
    {
      # Update the color palette
      pal <- colorBin(
        palette = "RdYlBu",
        domain  = dt_map()[, abs],
        reverse = TRUE
      )
      # Update the map
      leafletProxy("map", data = dt_map()) |>
        addCircleMarkers(
          lng          = ~lon,
          lat          = ~lat,
          layerId      = ~icao,
          radius       = 5L,
          color        = "black",
          stroke       = TRUE,
          weight       = .75,
          fillColor    = ~pal(abs),
          fillOpacity  = 1L,
          label        = ~paste(iata, "/", icao, " ", name, sep = ""),
          labelOptions = labelOptions(textsize = "12px"),
        )
    }
  )
  
  # ==== 2.5 Listen for airport dropdown selection ====
  
  observeEvent(
    input$apt,
    {
      if(input$apt %in% dt_apt[, icao]) {
        leafletProxy("map") |>
          setView(lng = dt_map()[icao == input$apt, lon], lat = dt_map()[icao == input$apt, lat], zoom = 14) |>
          addPopups(lng = dt_map()[icao == input$apt, lon], lat = dt_map()[icao == input$apt, lat], popup = dt_map()[icao == input$apt, label])
      } else {
        leafletProxy("map") |>
          setView(lng = 0, lat = 20, zoom = 2) |>
          clearPopups()
      }
    }
  )
  
  # ==== 2.6 Listen for clicks on map markers ====
  
  observeEvent(
    input$map_marker_click,
    {
      # Update the selectInput dropdown to match the clicked airport
      updateSelectInput(session, inputId = "apt", selected = input$map_marker_click$id)
      # Zoom in and center the map on the clicked marker
      leafletProxy("map", data = dt_map()) |>
        setView(lng = dt_map()[icao == input$map_marker_click$id, lon], lat = dt_map()[icao == input$map_marker_click$id, lon], zoom = 14) |>
        addPopups(lng = dt_map()[icao == input$map_marker_click$id, lon], lat = dt_map()[icao == input$map_marker_click$id, lat], popup = dt_map()[icao == input$map_marker_click$id, label])
    }
  )
  
  # ADD LEGEND TOO
  
  # ==== 2.6 For debugging only ====
  
  observe(
    {
      output$apt  <- renderText(input$apt)
      output$ssp  <- renderText(input$ssp)
      output$var  <- renderText(input$var)
      output$stat <- renderText(input$stat)
      output$key  <- renderText(input$key)
      output$year <- renderText(input$year)
      output$table <- DT::renderDT(dt_map())
    }
  )
  
}

# Run the app
shinyApp(ui = ui, server = server)