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
library(ggplot2)
library(htmltools)
library(leaflet)
library(plotly)
library(shiny)
library(shinyjs)

# Load the airport data
dt_apt <- fread(
  file           = "data/apt/airports.csv",
  header         = TRUE,
  colClasses     = c(rep("character", 3), rep("numeric", 2), "factor")
) |> setkey(cols = icao)

# Load the climate data
dt_cli <- fread(
  file           = "data/cli/cli.csv",
  header         = TRUE,
  colClasses     = c(rep("factor", 3), "integer", rep("numeric", 12))
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
  "ps"   = " hPa",
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
  "Predicted value for the year"   = "abs",
  "Change in value (±) since 2015" = "dif"
)

# Set sidebar panel width
sidebar_width <- 4L

# ==== 1 UI layout ====

ui <- fillPage(
  
  # ==== 1.1 Styling ====
  
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  tags$head(
    tags$style(
      HTML(
        "
        .bi-info-circle-fill { font-size: 14px; margin-left: 5px; cursor: pointer; color: #2780E3; }
        .row, .well          { height: 100%;}
        .shiny-input-select  { font-family: 'Courier New', Courier, monospace; }
        .col-sm-4            { padding-right: 0px; }
        .col-sm-8            { padding: 0px; }
        "
      )
    )
  ),
  
  sidebarLayout(
    
    # ==== 1.2 Display sidebar panel with selectors ====
    
    sidebarPanel(
      width = sidebar_width,
      h4("Climate change at airports worldwide, 2015–2100"),
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
      tooltip(h6("Select value for the colored markers to display:", bs_icon("info-circle-fill")), "The color of the dots can either display the absolute temperature at each airport for the observation year, or the amount of change since the year 2015."),
      radioButtons(
        inputId  = "key",
        label    = NULL,
        choices  = choices$key
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
      h6(htmlOutput("title")),
      
      # Display the climate plot
      plotlyOutput("plot", height = "350px"),
      
    ),
    
    # ==== 1.3 Display main panel with map ====
    
    mainPanel(width = 12L - sidebar_width, leafletOutput("map", height = "100%"))
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
  
  # ==== 2.2 Filter data based on user selections ====
  
  # Map data
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
        popup := paste(                                                                 # Assemble hover label
          "<b>", name, " (", iata, "/", icao, ") ",                                     # Airport
          " in ", year,                                                                 # Year
          " under ", substr(x = toupper(input$ssp), start = 1, stop = 4), ":</b></br>", # SSP
          names(choices$stat[choices$stat == input$stat]), " ",                         # Statistic
          tolower(names(choices$var[choices$var == input$var])), ": ",                  # Variable
          "<b>", sprintf(fmt = "%.2f", abs), choices$units[[input$var]], "</b></br>",   # Predicted value for the year
                                                                                        # Change in value since 2015
          if(input$year > 2015) paste("Change in value (±) since 2015: <b>", sprintf(fmt = "%+.2f", dif), ifelse(choices$units[[input$var]] == "%", " p.p", choices$units[[input$var]]), "</b>", sep = ""),
          sep = ""
        )
      ]
    }
  )
  
  # Plot data
  dt_plt <- reactive(
    if(input$apt %in% dt_apt[, icao]) { # If user selected an airport
      dt_cli[
        icao == input$apt & ssp == input$ssp & var == input$var,
        .(Year = year, Value = get(paste(input$key, input$stat, sep = "_")))
      ]
    } else { # If user did not select an airport
      dt_cli[
        ssp == input$ssp & var == input$var,
        # Take the lowest annual value for the minima, or the highest annual value for the maxima, or the mean for every other statistic
        .(Year = year, Value = round(sapply(X = .SD, FUN = if (input$stat == "min") min else if (input$stat == "max") max else mean), 2)),
        by = year,
        .SDcols = paste(input$key, input$stat, sep = "_")
      ][, !"year"]
    }
  )
  
  # ==== 2.3 Render the base map ====
  
  output$map <- renderLeaflet(
    {
      leaflet(data = dt_map()) |>
        addProviderTiles(providers$CartoDB.Positron)
    }
  )
  
  # ==== 2.4 Render the plot ====
  
  # Title
  observe(
    {
      output$title <- renderText(
        paste(
          "Plot of the",
          ifelse(input$key == "abs", "annual values for the", "centennial change in the"),
          tolower(names(choices$stat[choices$stat == input$stat])),
          tolower(names(choices$var[choices$var == input$var])),
          paste("(in", ifelse(choices$units[[input$var]] == "%", " p.p", choices$units[[input$var]]), ")", sep = ""),
          ifelse(input$apt %in% dt_apt[, icao], "at", "across"),
          ifelse(input$apt %in% dt_apt[, icao], paste(dt_apt[icao == input$apt, name], " (", dt_apt[icao == input$apt, iata], "/", dt_apt[icao == input$apt, icao], ")", sep = ""), tolower(input$apt)),
          ifelse(input$apt %in% dt_apt[, icao], "", "airports worldwide"),
          paste("under ", toupper(substr(input$ssp, 1, 4)), ":", sep = ""),
          sep = " "
        )
      )
    }
  )
  
  # Plot
  observe(
    {
      output$plot <- renderPlotly(
        {
          ggplot(data = dt_plt()) +
            geom_point(mapping = aes(x = Year, y = Value), color = "#2780E3", alpha = 0.5) +
            geom_smooth(mapping = aes(x = Year, y = Value), formula = y ~ x, method = "loess", linewidth = 1, color = "#2780E3", se = FALSE) +
            geom_vline(xintercept = input$year, color = "#2780E3", linetype = "dotted") +
            theme(
              axis.title         = element_blank(),
              legend.position    = "none",
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color = "lightgray"),
              panel.background   = element_rect(fill  = "#F7F7F7"),
              plot.background    = element_rect(fill  = "#F7F7F7", color = NA)
            )
        }
      )
    }
  )
  
  # ==== 2.5 Listen for change in filtered data ====
  
  observe(
    {
      # Update the color palette
      pal <- colorBin(
        palette = "plasma",
        domain  = dt_map()[, get(input$key)],
        reverse = TRUE
      )
      # Update the map
      leafletProxy("map", data = dt_map()) |>
        addCircleMarkers(
          lng          = ~lon,
          lat          = ~lat,
          layerId      = ~icao,
          radius       = 5,
          color        = "black",
          stroke       = TRUE,
          weight       = .75,
          fillColor    = ~pal(get(input$key)),
          fillOpacity  = .8,
          label        = ~paste(name, " (", iata, "/", icao, "): ", sprintf(fmt = ifelse(input$key == "abs", "%.2f", "%+.2f"), get(input$key)), ifelse(choices$units[[input$var]] == "%", " p.p", choices$units[[input$var]]), sep = ""),
          labelOptions = labelOptions(textsize = "12px")
        ) |>
        clearControls() |>
        addLegend(
          position  = "bottomright",
          pal       = pal,
          values    = ~get(input$key),
          title     = paste("Values in", choices$units[[input$var]], sep = " "),
          labFormat = labelFormat(suffix = ""),
          opacity   = 1
        )
    }
  )
  
  # ==== 2.6 Listen for change in airport selection ====
  
  observeEvent(
    input$apt,
    {
      if(input$apt %in% dt_apt[, icao]) {
        leafletProxy("map") |>
          clearPopups() |>
          flyTo(lng = dt_map()[icao == input$apt, lon], lat = dt_map()[icao == input$apt, lat], zoom = 14) |>
          addPopups(lng = dt_map()[icao == input$apt, lon], lat = dt_map()[icao == input$apt, lat] + 0.001, popup = dt_map()[icao == input$apt, popup])
      } else {
        leafletProxy("map") |>
          flyTo(lng = 0, lat = 0, zoom = 2) |>
          clearPopups()
      }
    }
  )
  
  # ==== 2.7 Listen for click on a map marker ====
  
  observeEvent(
    input$map_marker_click,
    {
      # Update the selectInput dropdown to match the clicked airport and trigger that action
      updateSelectInput(session, inputId = "apt", selected = input$map_marker_click$id)
    }
  )
  
  # ==== 2.8 Listen for change in climate scenario selection ====
  
  observeEvent(
    input$ssp,
    {
      # If one airport is already selected, don't reset the view, only the popup
      if(input$apt %in% dt_apt[, icao]) {
        leafletProxy("map") |>
          setView(lng = dt_map()[icao == input$apt, lon], lat = dt_map()[icao == input$apt, lat], zoom = 14) |>
          clearPopups() |>
          addPopups(lng = dt_map()[icao == input$apt, lon], lat = dt_map()[icao == input$apt, lat] + 0.001, popup = dt_map()[icao == input$apt, popup])
      }
    }
  )
  
  # ==== 2.9 Listen for change in climate variable selection ====
  
  observeEvent(
    input$var,
    {
      # If one airport is already selected, don't reset the view, only the popup
      if(input$apt %in% dt_apt[, icao]) {
        leafletProxy("map") |>
          clearPopups() |>
          setView(lng = dt_map()[icao == input$apt, lon], lat = dt_map()[icao == input$apt, lat], zoom = 14) |>
          addPopups(lng = dt_map()[icao == input$apt, lon], lat = dt_map()[icao == input$apt, lat] + 0.001, popup = dt_map()[icao == input$apt, popup])
      }
    }
  )
  
  # ==== 2.10 Listen for change in climate statistic selection ====
  
  observeEvent(
    input$stat,
    {
      # If one airport is already selected, don't reset the view, only the popup
      if(input$apt %in% dt_apt[, icao]) {
        leafletProxy("map") |>
          clearPopups() |>
          setView(lng = dt_map()[icao == input$apt, lon], lat = dt_map()[icao == input$apt, lat], zoom = 14) |>
          addPopups(lng = dt_map()[icao == input$apt, lon], lat = dt_map()[icao == input$apt, lat] + 0.001, popup = dt_map()[icao == input$apt, popup])
      }
    }
  )
  
  # ==== 2.11 Listen for change in year selection ====
  
  observeEvent(
    input$year,
    {
      # If one airport is already selected, don't reset the view, only the popup
      if(input$apt %in% dt_apt[, icao]) {
        leafletProxy("map") |>
          clearPopups() |>
          setView(lng = dt_map()[icao == input$apt, lon], lat = dt_map()[icao == input$apt, lat], zoom = 14) |>
          addPopups(lng = dt_map()[icao == input$apt, lon], lat = dt_map()[icao == input$apt, lat] + 0.001, popup = dt_map()[icao == input$apt, popup])
      }
    }
  )
  
}

# Run the app
shinyApp(ui = ui, server = server)