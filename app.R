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
library(leaflet)
library(shiny)
library(shinyjs)

# Load the airport data
dt_apt <- fread(
  file          = "data/apt/airports.csv",
  header        = TRUE,
  colClasses    = c(rep("character", 3L), rep("numeric", 2L), "factor")
) |> setkey(cols   = icao)

# Load the climate data
dt_cli <- fread(
  file          = "data/cli/cli.csv",
  header        = TRUE,
  colClasses    = c(rep("factor", 4L), rep("numeric", 12L))
) |> setkey(cols   = icao, var, ssp, year)

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

# FOR DEBUGGING ONLY
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

  # ==== 1.2 Display contents ====

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Climate change at airports worldwide, 2015–2100"),
      hr(),
      # Airport selector
      tooltip(
        h6("Select an airport (optional):", bs_icon("info-circle-fill")), "Optionally, pick one of the ~900 airports worldwide with at least 1M passengers in annual traffic, sorted alphabetically by their IATA code. 'All' will display all airports at once.",
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
      hr(),
      # For debugging only
      htmlOutput("apt"),
      htmlOutput("ssp"),
      htmlOutput("var"),
      htmlOutput("stat"),
      htmlOutput("key"),
      htmlOutput("year"),
      DT::DTOutput("table")
    ),
    mainPanel(
      width = 9,
      leafletOutput("map", height = "100%") # Display map
    )
  )
)

# ==== 2 Server logic ====

server <- function(input, output, session) {

  # ==== 2.1 Update selectors dynamically ====

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
        selected = choices$ssp[2]
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

  # ==== 2.2 Filter data dynamically based on selector inputs ====

  # Filter map data
  # dt_map <- reactive(
  #   {
  #     return(
  #       dt_cli[
  #         ssp  == input$ssp &
  #         var  == input$var &
  #         year == input$year,
  #         # if(input$apt %in% icao) icao == input$apt else icao != input$apt,
  #         .SD,
  #         .SDcols = patterns(paste("icao", "ssp", "var", "year", input$stat, sep = "|"))
  #       ]
  #     )
  #   }
  # )
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
      ]
    }
  )

  # ==== 2.4 Process the map ====

  # Render the map
  output$map <- renderLeaflet(
    {
      leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        flyTo(
          lng  = if(input$apt %in% dt_apt[, icao]) dt_apt[icao == input$apt, lon] else 0,
          lat  = if(input$apt %in% dt_apt[, icao]) dt_apt[icao == input$apt, lat] else 50,
          zoom = if(input$apt %in% dt_apt[, icao]) 6 else 3
        ) |>
        addProviderTiles("OpenStreetMap") |>
        # clearMarkers() |>
        addCircleMarkers(
          data        = dt_map(),
          lng         = dt_apt[icao == icao, lon],
          lat         = dt_apt[icao == icao, lat],
          radius      = if(input$apt %in% dt_apt[, icao]) 10L else 5L,
          color       = "black",
          stroke      = TRUE,
          weight      = .75,
          fill        = TRUE,
          fillColor   = "#2780E3",
          fillOpacity = .75
        )
    }
  )

  # Update the map
  observe(
    {
      leafletProxy("map", data = dt_map()) |>
      clearMarkers()
      # addCircleMarkers(
      #   # Return all coordinates or just the ones of the selected airport
      #   # lng         = if(input$apt %in% dt_apt[, icao]) dt_apt[icao == input$apt, lon] else dt_apt[icao == icao, lon],
      #   # lat         = if(input$apt %in% dt_apt[, icao]) dt_apt[icao == input$apt, lat] else dt_apt[icao == icao, lat],
      #   lng         = dt_apt[icao == icao, lon],
      #   lat         = dt_apt[icao == icao, lat],
      #   radius      = if(input$apt %in% dt_apt[, icao]) 10L else 5L,
      #   # Set the dot color to be either the absolute temperature or the temperature difference since 2015
      #   color       = "black",
      #   stroke      = TRUE,
      #   weight      = .75,
      #   fill        = TRUE,
      #   # fillColor   = pal(dt_cli[icao == icao & ssp == input$ssp & var == input$var & year == input$year, get(paste(input$key, input$stat, sep = "_"))]),
      #   # fillColor   = ~dt_pal(),
      #   fillOpacity = if(input$apt %in% dt_apt[, icao]) 1L else .8,
      #   # Tooltip
      #   popupOptions(keepInView = TRUE, closeOnClick = NULL),
      #   popup       = paste(
      #     if(input$apt %in% dt_apt[, icao]) {
      #       paste("<b>", dt_apt[icao == input$apt, name], " (", dt_apt[icao == input$apt, iata], "/", dt_apt[icao == input$apt, icao], ")", "</b></br>", sep = "")
      #     } else {
      #       paste("<b>", dt_apt[icao == icao, name], " (", dt_apt[icao == icao, iata], "/", dt_apt[icao == icao, icao], ")", "</b></br>", sep = "")
      #     },
      #     # Statistic
      #     names(choices$stat[choices$stat == input$stat]),
      #     # Variable
      #     tolower(names(choices$var[choices$var == input$var])),
      #     # Year
      #     "in", input$year, "under",
      #     # SSP
      #     substr(x = toupper(input$ssp), start = 1, stop = 4), "is",
      #     # Absolute temperature
      #     sprintf(fmt = "%.2f", dt_map()[icao == icao & year == input$year, get(paste("abs", input$stat, sep = "_"))]), "°C",
      #     # Temperature difference
      #     if(input$year > 2015) paste(" (", sprintf(fmt = "%+.2f", dt_map()[icao == icao & year == input$year, get(paste("dif", input$stat, sep = "_"))]), "°C since 2015).", sep = "") else ".",
      #     sep = " "
      #   )
      

      # For debugging only
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