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
  colClasses     = c(rep("factor", 3), "integer", rep("numeric", 12)),
  drop           = "var" # The file contains temperature data only, so we don't need the variable name
) |> setkey(cols = icao, ssp, year)

# Initialize a list for the user choices
choices <- list("apt" = dt_apt$icao)

# Define display names for the airports
names(choices$apt) <- paste(dt_apt$iata, "/", dt_apt$icao, " ", dt_apt$name, sep = "")

# Define display names for the SSPs
choices$ssp <- c(
  "SSP1 — Sustainability (most optimistic   )"             = "ssp126",
  "SSP2 — Middle of the road"                              = "ssp245",
  "SSP3 — Regional rivalry"                                = "ssp370",
  "SSP5 — Fossil-fueled development (least optimistic)"    = "ssp585"
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

# Set sidebar panel width
sidebar_width <- 4L

# ==== 1 UI layout ====

ui <- fillPage(
  
  # ==== 1.1 Styling ====
  
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  tags$head(
    tags$style(
      HTML("
        a                    { margin-right: 50px; }
        h4, h6               { display: inline }
        .bi-info-circle-fill { font-size: 14px; margin-left: 5px; cursor: pointer; color: #2780E3; }
        .col-sm-4            { padding-right: 0px; }
        .col-sm-8            { padding: 0px; }
        .footer              { text-align: center }
        .row, .well          { height: 100%;}
        .shiny-input-select  { font-family: 'Courier New', Courier, monospace; }
      ")
    )
  ),
  
  sidebarLayout(
    
    # ==== 1.2 Display sidebar panel ====
    
    sidebarPanel(
      width = sidebar_width,
      # Title
      h4("Climate change at airports worldwide, 2015–2100"),
      tooltip(
        trigger   = bs_icon("info-circle-fill"), "This dashboard shows the change in surface air temperature expected at the world's largest airports up to the year 2100 based on a leading IPCC climate model under four climate scenarios.",
        placement = "bottom"
      ),
      hr(),
      # Airport selector
      h6("Select or click on an airport (optional):"),
      tooltip(
        trigger   = bs_icon("info-circle-fill"), "These are the 907 airports with one million passengers or more in 2019. 'All' will display all airports at once.",
        placement = "right"
      ),
      selectInput(
        inputId   = "apt",
        label     = NULL,
        choices   = NULL,
        width     = "100%"
      ),
      # SSP selector
      h6("Select a climate scenario:"),
      tooltip(
        trigger   = bs_icon("info-circle-fill"), "These are four socioeconomic development scenarios of CO2 emissions commonly used in climate research. SSP2 and SSP3 are considered the most likely.",
        placement = "right"
      ),
      selectInput(
        inputId   = "ssp",
        label     = NULL,
        choices   = NULL,
        width     = "100%"
      ),
      # Climate statistic selector
      h6("Select an air temperature statistic:"),
      tooltip(
        trigger   = bs_icon("info-circle-fill"), "Climate change affects different temperature statistics, such as minima or mean, asymmetrically. This option lets you explore different statistics individually. Choose the mean if you are unsure.",
        placement = "right"
      ),
      selectInput(
        inputId   = "stat",
        label     = NULL,
        choices   = choices$stat,
        width     = "100%"
      ),
      # Year selector
      h6("Select an observation year:"),
      tooltip(
        trigger   = bs_icon("info-circle-fill"), "2015 is the first year of model predictions. The change in temperatures for later years is expressed in degrees Celsius above or below that baseline.",
        placement = "right"
      ),
      sliderInput(
        inputId   = "year",
        label     = NULL,
        min       = 2015,
        max       = 2100,
        value     = 2100,
        step      = 1,
        sep       = "",
        width     = "100%"
      ),
      hr(),
      # Plot title
      h6("Plot of the surface air temperature (in ℃) over time:"),
      tooltip(
        trigger   = bs_icon("info-circle-fill"), "This plot displays the annualized values for (top to bottom) the maximum, 75th percentile (dotted), median (dashed), mean, 25th percentile (dotted), and minimum temperatures for the airport(s) selected above. Each trend line uses a linear polynomial regression fitting.",
        placement = "right"
      ),
      # Plot
      plotlyOutput("plot", height = "450px"),
      hr(),
      div(
        class = "footer",
        tooltip(
          trigger   = tags$a(bs_icon("github", size = "1.25em"), href = "https://github.com/TheAviationDoctor/FutureAirport", target = "_blank"),
          "Visit my Github repo for the codebase and methodology",
          placement = "top"
        ),
        tooltip(
          trigger   = tags$a(bs_icon("mortarboard-fill", size = "1.25em"), href = "https://commons.erau.edu/edt/720/", target = "_blank"),
          "Link to the underlying research",
          placement = "top"
        ),
        tooltip(
          trigger   = tags$a(bs_icon("linkedin", size = "1.25em"), href = "https://www.linkedin.com/in/thomasdpellegrin/", target = "_blank"),
          "Connect with me on LinkedIn",
          placement = "top"
        ),
        tooltip(
          trigger   = tags$a(bs_icon("file-person-fill", size = "1.25em"), href = "https://thomas.pellegr.in/", target = "_blank"),
          "Visit my personal website",
          placement = "top"
        )
      )
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
  
  # ==== 2.2 Filter data based on user selections ====
  
  # Map data
  dt_map <- reactive(
    {
      dt_cli[
        ssp  == input$ssp & year == input$year,
        .(
          icao = icao,
          ssp  = ssp,
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
          names(choices$stat[choices$stat == input$stat]),                              # Statistic
          " air temperature: ",                                                         # Variable
          "<b>", sprintf(fmt = "%.2f", abs), "℃</b></br>",   # Predicted value for the year
                                                                                        # Change in value since 2015
          if(input$year > 2015) paste("Change in value (±) since 2015: <b>", sprintf(fmt = "%+.2f", dif), "℃</b>", sep = ""),
          sep = ""
        )
      ]
    }
  )
  
  # Plot data
  dt_plt <- reactive(
    if(input$apt %in% dt_apt[, icao]) { # If user selected an airport
      dt_cli[
        icao == input$apt & ssp == input$ssp,
        .(
          Year           = year,
          Minimum        = abs_min,
          LowerQuartile  = abs_lq,
          Mean           = abs_mean,
          Median         = abs_median,
          UpperQuartile  = abs_uq,
          Maximum        = abs_max
        )
      ]
    } else { # If user did not select an airport
      dt_cli[
        ssp == input$ssp,
        .(
          Year           = year,
          Minimum        = round(suppressWarnings(min(abs_min)), 2),
          LowerQuartile  = round(mean(abs_lq),                   2),
          Mean           = round(mean(abs_mean),                 2),
          Median         = round(mean(abs_median),               2),
          UpperQuartile  = round(mean(abs_uq),                   2),
          Maximum        = round(suppressWarnings(max(abs_uq)),  2)
        ),
        by = year
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
  
  # Plot
  observe(
    {
      output$plot <- renderPlotly(
        {
          p <- ggplot(data = dt_plt()) +
          # Scatterplots
          geom_point(mapping = aes(x = Year, y = Minimum),       color = "#2780E3", alpha = 0.25) +
          geom_point(mapping = aes(x = Year, y = LowerQuartile), color = "#2780E3", alpha = 0.25) +
          geom_point(mapping = aes(x = Year, y = Mean),          color = "#2780E3", alpha = 0.25) +
          geom_point(mapping = aes(x = Year, y = Median),        color = "#2780E3", alpha = 0.25) +
          geom_point(mapping = aes(x = Year, y = UpperQuartile), color = "#2780E3", alpha = 0.25) +
          geom_point(mapping = aes(x = Year, y = Maximum),       color = "#2780E3", alpha = 0.25) +
          # Local polynomial regression fitting lines
          geom_smooth(mapping = aes(x = Year, y = Minimum),       formula = y ~ x, method = "loess", linewidth = 1, color = "#2780E3", se = FALSE) +
          geom_smooth(mapping = aes(x = Year, y = LowerQuartile), formula = y ~ x, method = "loess", linewidth = 1, color = "#2780E3", se = FALSE, linetype = "dotted") +
          geom_smooth(mapping = aes(x = Year, y = Mean),          formula = y ~ x, method = "loess", linewidth = 1, color = "#2780E3", se = FALSE) +
          geom_smooth(mapping = aes(x = Year, y = Median),        formula = y ~ x, method = "loess", linewidth = 1, color = "#2780E3", se = FALSE, linetype = "dashed") +
          geom_smooth(mapping = aes(x = Year, y = UpperQuartile), formula = y ~ x, method = "loess", linewidth = 1, color = "#2780E3", se = FALSE, linetype = "dotted") +
          geom_smooth(mapping = aes(x = Year, y = Maximum),       formula = y ~ x, method = "loess", linewidth = 1, color = "#2780E3", se = FALSE) +
          # Year indicator (vertical line)
          geom_vline(xintercept = input$year, color = "#2780E3", linetype = "dotted") +
          # Styling
          theme(
            axis.ticks.length  = unit(0, "pt"),
            axis.title         = element_blank(),
            legend.position    = "none",
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "lightgray"),
            panel.background   = element_rect(fill  = "#F7F7F7"),
            plot.background    = element_rect(fill  = "#F7F7F7", color = NA),
            plot.margin        = unit(c(0, 0, 0, 0), "pt")
          )
          # Hide the Plotly toolbar
          ggplotly(p) |> config(displayModeBar = FALSE)
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
        domain  = dt_map()[, dif],
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
          fillColor    = ~pal(dif),
          fillOpacity  = .8,
          label        = ~paste(name, " (", iata, "/", icao, "): ", sprintf(fmt = "%+.2f", dif), "℃", sep = ""),
          labelOptions = labelOptions(textsize = "12px")
        ) |>
        clearControls() |>
        addLegend(
          position  = "bottomright",
          pal       = pal,
          values    = ~dif,
          title     = "Change in ℃ since 2015",
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