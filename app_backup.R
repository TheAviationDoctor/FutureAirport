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
input$apt <- "All"
input$ssp <- "ssp245"
input$var <- "tas"
input$stat <- "mean"
input$key <- "abs"
input$year <- 2100

# ==== 1 UI layout ====

ui <- fillPage(

  # ==== 1.1 CSS and theme ====

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

  # ==== 1.2 Display controls ====

  div(
    id = "controls",

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

    # Display line chart
    plotlyOutput("chart"),

    # For debugging only
    htmlOutput("apt"),
    htmlOutput("ssp"),
    htmlOutput("var"),
    htmlOutput("stat"),
    htmlOutput("key"),
    htmlOutput("year")

  ),
  
  # ==== 1.3 Display map ====
  
  leafletOutput("map", width = "100%", height = "100%")
)

# ==== 2 Server logic ====

server <- function(input, output, session) {

  # ==== 2.1 Update selectors dynamically ====

  # Airport selector
  # observe(
  #   {
      updateSelectInput(
        session,
        inputId = "apt",
        label   = NULL,
        choices = c("All", sort(choices$apt))
      )
    # }
  # )

  # SSP selector
  # observe(
  #   {
      updateSelectInput(
        session,
        inputId  = "ssp",
        label    = NULL,
        choices  = choices$ssp,
        selected = choices$ssp[2]
      )
  #   }
  # )

  # Climate variable selector
  # observe(
  #   {
      updateSelectInput(
        session,
        inputId  = "var",
        label    = NULL,
        choices  = choices$var,
        selected = choices$var[3]
      )
  #   }
  # )

  # Climate statistic selector
  # observe(
  #   {
      updateSelectInput(
        session,
        inputId  = "stat",
        label    = NULL,
        choices  = choices$stat,
        selected = choices$stat[3]
      )
  #   }
  # )

  # Color key
  updateRadioButtons(
    session,
    inputId      = "key",
    label        = NULL,
    choices      = choices$key,
    selected     = choices$key[2],
    inline       = FALSE
  )

  # ==== 2.2 Filter climate data dynamically based on selector inputs ====

  # Filter chart data by SSP and airport (if one is selected), then summarize the statistics by year
  chart_data <- reactive(
    {
      return(
        dt_cli[
          ssp == input$ssp &
          var == input$var &
          if(input$apt %in% icao) icao == input$apt else icao != input$apt,
          .(
            min    = min( paste(input$key, "min",    sep = "_")),
            max    = max( paste(input$key, "max",    sep = "_")),
            median = mean(paste(input$key, "median", sep = "_")),
            mean   = mean(paste(input$key, "mean",   sep = "_")),
            lq     = mean(paste(input$key, "lq",     sep = "_")),
            uq     = mean(paste(input$key, "uq",     sep = "_"))
          ),
          by = year
        ]
      )
    }
  )

  # Filter map data by SSP, variable, and airport (if one is selected)
  map_data <- reactive(
    {
      return(
        dt_cli[
          ssp == input$ssp &
          var == input$var &
          if(input$apt %in% icao) icao == input$apt else icao != input$apt
        ]
      )
    }
  )

  # ==== 2.3 Render the line chart ====

  # output$chart <- renderPlotly(
  #   {
  #     plot_ly(chart_data, x = ~year) %>%
  #       add_lines(y = ~min, name = "Minimum", line = list(color = 'blue')) %>%
  #       add_lines(y = ~max, name = "Maximum", line = list(color = 'red')) %>%
  #       add_lines(y = ~median, name = "Median", line = list(color = 'green')) %>%
  #       add_lines(y = ~mean, name = "Mean", line = list(color = 'orange')) %>%
  #       add_lines(y = ~lq, name = "Lower Quartile", line = list(color = 'purple', dash = 'dash')) %>%
  #       add_lines(y = ~uq, name = "Upper Quartile", line = list(color = 'purple', dash = 'dash')) %>%
  #       layout(
  #         title = paste("Temperature Trends for", input$scenario),
  #         xaxis = list(title = "Year"),
  #         yaxis = list(title = "Temperature (°C)"),
  #         legend = list(title = list(text = "Temperature Statistics")),
  #         showlegend = FALSE,  # Hide the legend
  #         shapes = list(
  #           list(
  #             type = "line",
  #             x0 = input$year - 2015, x1 = input$year - 2015,
  #             y0 = min(chart_data$min), y1 = max(chart_data$max),
  #             line = list(color = "black", dash = "dash", width = 2)
  #           )
  #         ),
  #         # Add annotations for each line
  #         annotations = list(
  #           list(x = 85, y = chart_data[year == 2100]$min,
  #                text = "Minimum", showarrow = FALSE, xanchor = "left", font = list(color = "blue")),
  #           list(x = 85, y = chart_data[year == 2100]$max,
  #                text = "Maximum", showarrow = FALSE, xanchor = "left", font = list(color = "red")),
  #           list(x = 85, y = chart_data[year == 2100]$median,
  #                text = "Median", showarrow = FALSE, xanchor = "left", font = list(color = "green")),
  #           list(x = 85, y = chart_data[year == 2100]$mean,
  #                text = "Mean", showarrow = FALSE, xanchor = "left", font = list(color = "orange")),
  #           list(x = 85, y = chart_data[year == 2100]$lq,
  #                text = "Lower Quartile", showarrow = FALSE, xanchor = "left", font = list(color = "purple")),
  #           list(x = 85, y = chart_data[year == 2100]$uq,
  #                text = "Upper Quartile", showarrow = FALSE, xanchor = "left", font = list(color = "purple"))
  #         )
  #       )
  #   }
  # )

  # ==== 2.4 Render the map ====

  # Render the map
  output$map <- renderLeaflet(
    {
      leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        addProviderTiles("OpenStreetMap") |>
        # setView(
        flyTo(
          # lng = 0,
          # lat = 50,
          lng  = if(input$apt %in% dt_apt[, icao]) dt_apt[icao == input$apt, lon] else 0,
          lat  = if(input$apt %in% dt_apt[, icao]) dt_apt[icao == input$apt, lat] else 50,
          # zoom = 3
          zoom = if(input$apt == "All") 3 else 4
        ) |>
        # Move zoom controls to top right
        onRender("function(el, x) { L.control.zoom({position:'topright'}).addTo(this); }")
    }
  )

  # ==== 2.5 Update the map ====

  # Update the map
  observe(
    {
      # Create color palette for temperature
      pal <- colorNumeric(
        palette = "RdYlBu",
        # Narrow down the range of temperature colors based on whether the user wants to display absolute or relative temperatures
        # domain = c(
        #   min(dt_cli[ssp == input$ssp & var == input$var, get(paste(input$key, input$stat, sep = "_"))]),
        #   max(dt_cli[ssp == input$ssp & var == input$var, get(paste(input$key, input$stat, sep = "_"))])
        # ),
        domain = c(-60, 60),
        reverse = TRUE
      )
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
        fillColor   = pal(dt_cli[icao == icao & ssp == input$ssp & var == input$var & year == input$year, get(paste(input$key, input$stat, sep = "_"))]),
        fillOpacity = if(input$apt %in% dt_apt[, icao]) 1L else .8,
        # Tooltip
        popupOptions(keepInView = TRUE, closeOnClick = NULL),
        popup       = paste(
          if(input$apt %in% dt_apt[, icao]) {
            paste("<b>", dt_apt[icao == input$apt, name], " (", dt_apt[icao == input$apt, iata], "/", dt_apt[icao == input$apt, icao], ")", "</b></br>", sep = "")
          } else {
            paste("<b>", dt_apt[icao == icao, name], " (", dt_apt[icao == icao, iata], "/", dt_apt[icao == icao, icao], ")", "</b></br>", sep = "")
          },
          # Statistic
          names(choices$stat[choices$stat == input$stat]),
          # Variable
          tolower(names(choices$var[choices$var == input$var])),
          # Year
          "in", input$year, "under",
          # SSP
          substr(x = toupper(input$ssp), start = 1, stop = 4), "is",
          # Absolute temperature
          sprintf(fmt = "%.2f", map_data()[icao == icao & year == input$year, get(paste("abs", input$stat, sep = "_"))]), "°C",
          # Temperature difference
          if(input$year > 2015) paste(" (", sprintf(fmt = "%+.2f", map_data()[icao == icao & year == input$year, get(paste("dif", input$stat, sep = "_"))]), "°C since 2015).", sep = "") else ".",
          sep = " "
        )
      )

      # For debugging only
      output$apt  <- renderText(input$apt)
      output$ssp  <- renderText(input$ssp)
      output$var  <- renderText(input$var)
      output$stat <- renderText(input$stat)
      output$key  <- renderText(input$key)
      output$key  <- renderText(input$year)

    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)