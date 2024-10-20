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
library(leaflet)
library(shiny)
library(shinyBS)
library(shinyjs)

# Clear the console
cat("\014")

# ==============================================================================
# 1 UI layout
# ==============================================================================

# ui <- fluidPage(
ui <- fillPage(
  useShinyjs(),  # Enable shinyjs if needed for any further interactivity
  # Define a Bootstrap theme using bslib
  theme = bs_theme(version = 5, bootswatch = "cerulean"),  # Example of a custom theme
  # CSS
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
  # Map
  leafletOutput("map", width = "100%", height = "100%"),
  # Controls
  div(
    id = "controls",
    h4("Surface air temperature at airports worldwide, 2015–2100"),
    span("Select an airport:"),
    tooltip(
      span(bsicons::bs_icon("info-circle-fill")),
      "Optionally, pick one of the ~900 airports worldwide with at least 1M passengers in annual traffic, sorted alphabetically by their IATA code. 'All' will display all airports at once.",
      placement = "bottom"
    ),
    selectInput(
      inputId  = "airport",
      label    = NULL,
      choices  = NULL,
      selected = NULL,
      width    = "100%"
    ),
    span("Select a climate scenario:"),
    tooltip(
      span(bsicons::bs_icon("info-circle-fill")),
      "Shared Socioeconomic Pathways (SSPs) are climate change scenarios defined by the Intergovernmental Panel on Climate Change (IPCC) to standardize climate research. They are based on projected socioeconomic development trajectories up to the year 2100. The IPCC Sixth Report (2021) described SSP2 as likely, hence it is selected as default here.",
      placement = "bottom"
    ),
    selectInput(
      inputId  = "ssp",
      label    = NULL,
      choices  = names(ssp_choices),
      width    = "100%"
    ),
    # htmlOutput("ssp_desc"),
    sliderInput(
      inputId  = "year",
      label    = "Select an observation year:",
      min      = 2015,
      max      = 2100,
      value    = 2015,
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

  # Load the airport data
  dt_apt <- fread(
    file          = "data/apt/airports.csv",
    header        = TRUE,
    colClasses    = c(rep("character", 3L), rep("numeric", 2L), "factor")
  ) |>
    setkey(cols   = iata)

  # Cosmetic names for the dropdown
  apt_choices <- dt_apt$icao
  names(apt_choices) <- paste(dt_apt$iata, "/", dt_apt$icao, " ", dt_apt$name, sep = "")

  # Update airport dropdown choices dynamically
  observe(
    {
      updateSelectInput(
        session,
        inputId = "airport",
        label   = NULL,
        choices = c("All", apt_choices)
      )
    }
  )

  # Load the climate data
  dt_cli <- fread(
    file          = "data/cli/cli.csv",
    header        = TRUE,
    colClasses    = c(rep("factor", 3L), rep("numeric", 12L))
  ) |>
    setkey(cols   = icao, var, ssp)

  # Cosmetic names for the ssp dropdown
  ssp_choices <- toupper(unique(dt_cli$ssp))
  names(ssp_choices) <- c(
    "SSP1 — Sustainability (best-case scenario)",
    "SSP2 — Middle of the road",
    "SSP3 — Regional rivalry",
    "SSP5 — Fossil-fueled development (worst-case scenario)"
  )

  # Update ssp dropdown choices dynamically
  observe(
    {
      updateSelectInput(
        session,
        inputId = "ssp",
        label   = NULL,
        choices = ssp_choices
      )
    }
  )

  # Update the SSP description
  ssp_desc <- reactive(
    {
      case_when(
        input$ssp == "SSP126" ~ "The world shifts gradually, but pervasively, toward a more sustainable path, emphasizing more inclusive development that respects perceived environmental boundaries. Management of the global commons slowly improves, educational and health investments accelerate the demographic transition, and the emphasis on economic growth shifts toward a broader emphasis on human well-being. Driven by an increasing commitment to achieving development goals, inequality is reduced both across and within countries. Consumption is oriented toward low material growth and lower resource and energy intensity.",
        input$ssp == "SSP245" ~ "The world follows a path in which social, economic, and technological trends do not shift markedly from historical patterns. Development and income growth proceeds unevenly, with some countries making relatively good progress while others fall short of expectations. Global and national institutions work toward but make slow progress in achieving sustainable development goals. Environmental systems experience degradation, although there are some improvements and overall the intensity of resource and energy use declines. Global population growth is moderate and levels off in the second half of the century. Income inequality persists or improves only slowly and challenges to reducing vulnerability to societal and environmental changes remain.",
        input$ssp == "SSP370" ~ "A resurgent nationalism, concerns about competitiveness and security, and regional conflicts push countries to increasingly focus on domestic or, at most, regional issues. Policies shift over time to become increasingly oriented toward national and regional security issues. Countries focus on achieving energy and food security goals within their own regions at the expense of broader-based development. Investments in education and technological development decline. Economic development is slow, consumption is material-intensive, and inequalities persist or worsen over time. Population growth is low in industrialized and high in developing countries. A low international priority for addressing environmental concerns leads to strong environmental degradation in some regions.",
        input$ssp == "SSP585" ~ "This world places increasing faith in competitive markets, innovation and participatory societies to produce rapid technological progress and development of human capital as the path to sustainable development. Global markets are increasingly integrated. There are also strong investments in health, education, and institutions to enhance human and social capital. At the same time, the push for economic and social development is coupled with the exploitation of abundant fossil fuel resources and the adoption of resource and energy intensive lifestyles around the world. All these factors lead to rapid growth of the global economy, while global population peaks and declines in the twenty-first century. Local environmental problems like air pollution are successfully managed. There is faith in the ability to effectively manage social and ecological systems, including by geo-engineering if necessary."
      )
    }
  )

  # Render the SSP description
  output$ssp_desc <- renderText(ssp_desc())

  # Filter data based on the slider and dropdown inputs
  filtered_data <- reactive(
    {
      data <- dt_cli |>
        filter(year == as.character(input$year))
      if (input$airport != "All") {
        data <- data |>
          filter(icao == input$airport)
      }
      return(data)
    }
  )

  # Render the map
  output$map <- renderLeaflet(
    {
      leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        addProviderTiles("OpenStreetMap") |>
        setView(lng = 0, lat = 10, zoom = 3) |>
        htmlwidgets::onRender(
          "function(el, x) {
          L.control.zoom({position:'topright'}).addTo(this);
        }")
    }
  )
  
  # Observe filtered data and update the map
  observe(
    {
      data <- filtered_data()
      # Create color palette for temperature
      pal <- colorNumeric(palette = "RdYlBu", domain = data$tas, reverse = TRUE)
      # Add points to the map
      leafletProxy("map", data = data) |>
        clearMarkers() |> 
        addCircleMarkers(
          lng         = ~dt_apt[icao == icao, lon],
          lat         = ~dt_apt[icao == icao, lat],
          radius      = 5,
          color       = ~pal(mean),
          stroke      = FALSE,
          fillOpacity = 0.8,
          popup       = ~paste("ICAO:", icao, "<br>", "Name:", icao)
        )
    }
  )
}

# Run the app
# shinyApp(ui, server)
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))