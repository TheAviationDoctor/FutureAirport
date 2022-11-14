# ==============================================================================
#    NAME: scripts/2_sample.R
#   INPUT: 8,982 rows read from the dat$pop table
# ACTIONS: Subset the airport sample and plot its characteristics
#  OUTPUT: Five plots saved to disk
# RUNTIME: ~18 seconds (3.8 GHz CPU / 128 GB DDR4 RAM / SSD)
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2022
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load the required libraries
library(data.table)
library(DBI)
library(geosphere)
library(kgc)
library(maps)
library(rgeos)
library(rnaturalearth)
library(scales)
library(tidyverse)
library(tmaptools)
library(viridis)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Load and describe the population
# ==============================================================================

# Fetch the population data
dt_pop <- fn_sql_qry(
  statement = paste(
    "SELECT
      icao,
      iata,
      traffic,
      name,
      lat,
      lon,
      zone,
      rwy,
      toda
    FROM",
      dat$pop,
    ";",
    sep = " "
  )
)

# Recast column types
set(x = dt_pop, j = "zone", value = as.factor(dt_pop[, zone]))

# Describe the population airports
summary(dt_pop)

# ==============================================================================
# 2 Subset the sample from the population based on a minimum traffic threshold
# ==============================================================================

# Select only airports above the minimum traffic threshold in passengers
dt_smp <- subset(dt_pop, traffic >= sim$pop_thr)

# Describe the sample airports
summary(dt_smp)

# ==============================================================================
# 3 Test that the sample is representative of the population's traffic
# ==============================================================================

# Describe the population vs. sample traffic
list(
  "Airport count (population, sample, percentage)" =
    c(
      length(unique(dt_pop$icao)),
      length(unique(dt_smp$icao)),
      length(unique(dt_smp$icao)) / length(unique(dt_pop$icao))
    ),
  "Runway count (population, sample, percentage)" =
    c(
      nrow(dt_pop),
      nrow(dt_smp),
      nrow(dt_smp) / nrow(dt_pop)
    ),
  "Passengers count (population, sample, percentage)" =
    c(
      sum(dt_pop$traffic[!rev(duplicated(rev(dt_pop$icao)))]),
      sum(dt_smp$traffic[!rev(duplicated(rev(dt_smp$icao)))]),
      sum(dt_smp$traffic[!rev(duplicated(rev(dt_smp$icao)))]) /
        sum(dt_pop$traffic[!rev(duplicated(rev(dt_pop$icao)))])
    )
)

# ==============================================================================
# 4 Test that the sample is representative of the population's latitudes
# ==============================================================================

# ==============================================================================
# 4.1 Summarize the results
# ==============================================================================

# Define the traffic bins (logarithmic sequence)
breaks <- c(1L %o% 10^(0:9))

# Define the bin labels
labels <- c(
  "[1–10)",
  "[10–100)",
  "[100–1K)",
  "[1K–10K)",
  "[10K–100K)",
  "[100K–1M)",
  "[1M–10M)",
  "[10M–100M)",
  "[100M–1B)"
)

# Describe the population's latitude variable (in °)
summary(dt_pop$lat[!rev(duplicated(rev(dt_pop$icao)))])

# Describe the sample's latitude variable (in °)
summary(dt_smp$lat[!rev(duplicated(rev(dt_smp$icao)))])

# Describe the population vs. sample latitudes (in km)
list(
  "Distance from the median latitude to the equator (population, sample)" =
    c(
      distm(
        c(0L, median(dt_pop$lat[!rev(duplicated(rev(dt_pop$icao)))])),
        c(0L, 0L),
        fun = distHaversine
      ) / 1000L,
      distm(
        c(0L, median(dt_smp$lat[!rev(duplicated(rev(dt_smp$icao)))])),
        c(0L, 0L),
        fun = distHaversine
      ) / 1000L
    ),
  "Distance from the mean latitude to the equator (population, sample)" =
    c(
      distm(
        c(0L, mean(dt_pop$lat[!rev(duplicated(rev(dt_pop$icao)))])),
        c(0L, 0L),
        fun = distHaversine
      ) / 1000L,
      distm(
        c(0L, mean(dt_smp$lat[!rev(duplicated(rev(dt_smp$icao)))])),
        c(0L, 0L),
        fun = distHaversine
      ) / 1000L
    ),
  "Distance from the median latitude to the mean (population, sample)" =
    c(
      distm(
        c(0L, median(dt_pop$lat[!rev(duplicated(rev(dt_pop$icao)))])),
        c(0L, mean(dt_pop$lat[!rev(duplicated(rev(dt_pop$icao)))])),
        fun = distHaversine
      ) / 1000L,
      distm(
        c(0L, median(dt_smp$lat[!rev(duplicated(rev(dt_smp$icao)))])),
        c(0L, mean(dt_smp$lat[!rev(duplicated(rev(dt_smp$icao)))])),
        fun = distHaversine
      ) / 1000L
    )
)

# Find the population's northernmost airport
dt_pop[which.max(dt_pop$lat), c(1L, 4L, 5L)]

# Find the sample's northernmost airport
dt_smp[which.max(dt_smp$lat), c(1L, 4L, 5L)]

# Find the population's southernmost airport
dt_pop[which.min(dt_pop$lat), c(1L, 4L, 5L)]

# Find the sample's southernmost airport
dt_smp[which.min(dt_smp$lat), c(1L, 4L, 5L)]

# Bin the population airports (not runways) by passenger traffic and geo. zones
dt_pop_binned <- dt_pop[!duplicated(dt_pop$icao), ] %>%
  mutate(
    bin = cut(
      x              = traffic,
      breaks         = breaks,
      labels         = labels,
      include.lowest = TRUE,
      right          = FALSE
    )
  ) %>%
  mutate(
    geo = cut(
      x              = lat,
      breaks         = unique(unlist(x = geo, use.names = FALSE)),
      labels         = names(geo),
      include.lowest = TRUE,
      right          = FALSE
    )
  )

# Bin the sample airports (not runways) by passenger traffic and geo. zones
dt_smp_binned <- dt_smp[!duplicated(dt_smp$icao), ] %>%
  mutate(
    bin = cut(
      x              = traffic,
      breaks         = breaks,
      labels         = labels,
      include.lowest = TRUE,
      right          = FALSE
    )
  ) %>%
  mutate(
    geo = cut(
      x              = lat,
      breaks         = unique(unlist(x = geo, use.names = FALSE)),
      labels         = names(geo),
      include.lowest = TRUE,
      right          = FALSE
    )
  )

# Count the population airports by geographical zone
dt_pop_binned %>%
  group_by(geo) %>%
  dplyr::summarize(n = n()) %>%
  mutate(per = n / nrow(dt_pop_binned) * 100L) %>%
  bind_rows(summarize_all(., ~ifelse(is.numeric(.), sum(.), "Total")))

# Count the sample airports by geographical zone
dt_smp_binned %>%
  group_by(geo) %>%
  dplyr::summarize(n = n()) %>%
  mutate(per = n / nrow(dt_smp_binned) * 100L) %>%
  bind_rows(summarize_all(., ~ifelse(is.numeric(.), sum(.), "Total")))

# Sum the population traffic by geographical zone
dt_pop_binned %>%
  group_by(geo) %>%
  dplyr::summarize(n = sum(traffic) / 10^6) %>%
  mutate(per = n / sum(dt_pop[!duplicated(dt_pop$icao), ]$traffic) * 10^8) %>%
  bind_rows(summarize_all(., ~ifelse(is.numeric(.), sum(.), "Total")))

# Sum the sample traffic by geographical zone
dt_smp_binned %>%
  group_by(geo) %>%
  dplyr::summarize(n = sum(traffic) / 10^6) %>%
  mutate(per = n / sum(dt_smp[!duplicated(dt_smp$icao), ]$traffic) * 10^8) %>%
  bind_rows(summarize_all(., ~ifelse(is.numeric(.), sum(.), "Total")))

# ==============================================================================
# 4.2 Plot the results
# ==============================================================================

# Define the world object from the Natural Earth package
world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

# Plot the population airports onto a world map
ggplot() +
  geom_sf(data = world, fill = "gray") +
  coord_sf(expand = FALSE) +
  # Define the scales
  scale_x_continuous(breaks = c(-180L, 180L)) +
  scale_y_continuous(
    breaks = unique(unlist(x = geo, use.names = FALSE)),
    limits = c(-90L, 90L)
  ) +
  scale_color_viridis(
    breaks    = breaks,
    direction = -1L,
    labels    = trans_format(trans = "log10", format = math_format(10^.x)),
    limits    = c(
      dt_pop$traffic[which.min(dt_pop$traffic)],
      dt_pop$traffic[which.max(dt_pop$traffic)]
    ),
    name      = "PPA",
    trans     = "log"
  ) +
  scale_size_continuous(name = "traffic", guide = "none") +
  # Add the airports
  geom_point(
    data    = dt_pop[!duplicated(dt_pop$icao), ],
    mapping = aes(x = lon, y = lat, color = traffic, size = traffic),
    shape   = 20L,
  ) +
  # Add parallels
  geom_hline(
    color      = "black",
    linewidth  = .25,
    yintercept = c(
      dt_pop$lat[which.max(dt_pop$lat)],                 # Max latitude
      dt_pop$lat[which.min(dt_pop$lat)],                 # Min latitude
      mean(dt_pop[!duplicated(dt_pop$icao), ]$lat),      # Mean latitude
      as.numeric(
        crossprod(
          dt_pop[!duplicated(dt_pop$icao), ]$traffic,
          dt_pop[!duplicated(dt_pop$icao), ]$lat
        ) /
          sum(dt_pop[!duplicated(dt_pop$icao), ]$traffic)
      ),                                                 # PPA-weighted latitude
      median(dt_pop[!duplicated(dt_pop$icao), ]$lat)     # Median latitude
    )
  ) +
  # Add parallel labels
  geom_text(
    data  = world,
    color = "black",
    hjust = 1L,
    label = paste(
      "Max. latitude ",
      sprintf(
        fmt = "%.2f",
        round(x = dt_pop$lat[which.max(dt_pop$lat)], digits = 2L)
      ),
      "°",
      sep = ""
    ),
    size  = 1.5,
    x     = 179L,
    y     = dt_pop$lat[which.max(dt_pop$lat)] + 2L
  ) +
  geom_text(
    data  = world,
    color = "black",
    hjust = 1L,
    label = paste(
      "Min. latitude ",
      sprintf(
        fmt = "%.2f",
        round(x = dt_pop$lat[which.min(dt_pop$lat)], digits = 2L)
      ),
      "°",
      sep = ""
    ),
    size  = 1.5,
    x     = 179L,
    y     = dt_pop$lat[which.min(dt_pop$lat)] + 2L
  ) +
  geom_text(
    data  = world,
    color = "black",
    hjust = 1L,
    label = paste(
      "Mean latitude ",
      sprintf(
        fmt = "%.2f",
        round(x = mean(dt_pop[!duplicated(dt_pop$icao), ]$lat), digits = 2L)
      ),
      "°",
      sep = ""
    ),
    size  = 1.5,
    x     = 179L,
    y     = mean(dt_pop[!duplicated(dt_pop$icao), ]$lat) - 1.5
  ) +
  geom_text(
    data  = world,
    color = "black",
    hjust = 1L,
    label = paste(
      "PPA-weighted mean latitude ",
      sprintf(
        fmt = "%.2f",
        round(
          x      = as.numeric(crossprod(
            dt_pop[!duplicated(dt_pop$icao), ]$traffic,
            dt_pop[!duplicated(dt_pop$icao), ]$lat
            ) /
            sum(dt_pop[!duplicated(dt_pop$icao), ]$traffic)
          ),
          digits = 2L)
      ),
      "°",
      sep = ""
    ),
    size  = 1.5,
    x     = 179L,
    y     = as.numeric(
      crossprod(
        dt_pop[!duplicated(dt_pop$icao), ]$traffic,
        dt_pop[!duplicated(dt_pop$icao), ]$lat
      ) /
      sum(dt_pop[!duplicated(dt_pop$icao), ]$traffic)
    ) - 1.5
  ) +
  geom_text(
    data  = world,
    color = "black",
    hjust = 1L,
    label = paste(
      "Median latitude ",
      sprintf(
        fmt = "%.2f",
        round(x = median(dt_pop[!duplicated(dt_pop$icao), ]$lat), digits = 2L)
      ),
      "°",
      sep = ""
    ),
    size  = 1.5,
    x     = 179L,
    y     = median(dt_pop[!duplicated(dt_pop$icao), ]$lat) + 2L
  ) +
  geom_text(
    data  = world,
    color = "gray",
    hjust = 0L,
    label = "Arctic circle",
    size  = 1.5,
    x     = -179L,
    y     = unique(unlist(x = geo, use.names = FALSE))[5] - 2L
  ) +
  geom_text(
    data  = world,
    color = "gray",
    hjust = 0L,
    label = "Tropic of Cancer",
    size  = 1.5,
    x     = -179L,
    y     = unique(unlist(x = geo, use.names = FALSE))[4] - 2L
  ) +
  geom_text(
    data  = world,
    color = "gray",
    hjust = 0L,
    label = "Tropic of Capricorn",
    size  = 1.5,
    x     = -179L,
    y     = unique(unlist(x = geo, use.names = FALSE))[3] - 2L
  ) +
  geom_text(
    data  = world,
    color = "gray",
    hjust = 0L,
    label = "Antarctic circle",
    size  = 1.5,
    x     = -179L,
    y     = unique(unlist(x = geo, use.names = FALSE))[2] - 2L
  ) +
  # Add zonal labels
  geom_text(
    angle = 90L,
    data  = world,
    color = "gray",
    hjust = .5,
    label = unique(names(geo))[1],
    size  = 2.5,
    x     = -175L,
    y     = mean(
      c(
        unique(unlist(x = geo, use.names = FALSE))[1],
        unique(unlist(x = geo, use.names = FALSE))[2]
      )
    )
  ) +
  geom_text(
    angle = 90L,
    data  = world,
    color = "gray",
    hjust = .5,
    label = unique(names(geo))[2],
    size  = 2.5,
    x     = -175L,
    y     = mean(
      c(
        unique(unlist(x = geo, use.names = FALSE))[2],
        unique(unlist(x = geo, use.names = FALSE))[3]
      )
    )
  ) +
  geom_text(
    angle = 90L,
    data  = world,
    color = "gray",
    hjust = .5,
    label = unique(names(geo))[3],
    size  = 2.5,
    x     = -175L,
    y     = mean(
      c(
        unique(unlist(x = geo, use.names = FALSE))[3],
        unique(unlist(x = geo, use.names = FALSE))[4]
      )
    )
  ) +
  geom_text(
    angle = 90L,
    data  = world,
    color = "gray",
    hjust = .5,
    label = unique(names(geo))[2],
    size  = 2.5,
    x     = -175L,
    y     = mean(
      c(
        unique(unlist(x = geo, use.names = FALSE))[4],
        unique(unlist(x = geo, use.names = FALSE))[5]
      )
    )
  ) +
  geom_text(
    angle = 90L,
    data  = world,
    color = "gray",
    hjust = .5,
    label = unique(names(geo))[1],
    size  = 2.5,
    x     = -175L,
    y     = mean(
      c(
        unique(unlist(x = geo, use.names = FALSE))[5],
        unique(unlist(x = geo, use.names = FALSE))[6]
      )
    )
  ) +
  theme_light() +
  theme(
    axis.title      = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    legend.key.size = unit(.2, "in"),
    legend.title    = element_text(size = 5L),
    legend.text     = element_text(size = 5L),
    plot.margin     = margin(-.8, 0L, -.8, 0L, "in")
  )

# Find the aspect ratio of the map
ar <- tmaptools::get_asp_ratio(world)

# Save the plot
ggsave(
  filename = "2_map_of_population_airports.png",
  plot     = last_plot(),
  device   = "png",
  path     = "plots",
  scale    = 1L,
  height   = 9L / ar,
  width    = 9L,
  units    = "in",
  dpi      = "retina"
)

# Plot the sample airports onto a world map
ggplot() +
  geom_sf(data = world, fill = "gray") +
  coord_sf(expand = FALSE) +
  # Define the scales
  scale_x_continuous(breaks = c(-180L, 180L)) +
  scale_y_continuous(
    breaks = unique(unlist(x = geo, use.names = FALSE)),
    limits = c(-90L, 90L)
  ) +
  scale_color_viridis(
    breaks    = breaks,
    direction = -1L,
    labels    = trans_format(trans = "log10", format = math_format(10^.x)),
    limits    = c(
      dt_pop$traffic[which.min(dt_pop$traffic)],
      dt_pop$traffic[which.max(dt_pop$traffic)]
    ),
    name      = "PPA",
    trans     = "log"
  ) +
  scale_size_continuous(name = "traffic", guide = "none") +
  # Add the airports
  geom_point(
    data    = dt_smp[!duplicated(dt_smp$icao), ],
    mapping = aes(x = lon, y = lat, color = traffic, size = traffic),
    shape   = 20L,
  ) +
  # Add parallels
  geom_hline(
    color      = "black",
    linewidth  = .25,
    yintercept = c(
      dt_smp$lat[which.max(dt_smp$lat)],                 # Max latitude
      dt_smp$lat[which.min(dt_smp$lat)],                 # Min latitude
      mean(dt_smp[!duplicated(dt_smp$icao), ]$lat),      # Mean latitude
      as.numeric(
        crossprod(
          dt_smp[!duplicated(dt_smp$icao), ]$traffic,
          dt_smp[!duplicated(dt_smp$icao), ]$lat
        ) /
          sum(dt_smp[!duplicated(dt_smp$icao), ]$traffic)
      ),                                                 # PPA-weighted latitude
      median(dt_smp[!duplicated(dt_smp$icao), ]$lat)     # Median latitude
    )
  ) +
  # Add parallel labels
  geom_text(
    data  = world,
    color = "black",
    hjust = 1L,
    label = paste(
      "Max. latitude ",
      sprintf(
        fmt = "%.2f",
        round(x = dt_smp$lat[which.max(dt_smp$lat)], digits = 2L)
      ),
      "°",
      sep = ""
    ),
    size  = 1.5,
    x     = 179L,
    y     = dt_smp$lat[which.max(dt_smp$lat)] + 2L
  ) +
  geom_text(
    data  = world,
    color = "black",
    hjust = 1L,
    label = paste(
      "Min. latitude ",
      sprintf(
        fmt = "%.2f",
        round(x = dt_smp$lat[which.min(dt_smp$lat)], digits = 2L)
      ),
      "°",
      sep = ""
    ),
    size  = 1.5,
    x     = 179L,
    y     = dt_smp$lat[which.min(dt_smp$lat)] + 2L
  ) +
  geom_text(
    data  = world,
    color = "black",
    hjust = 1L,
    label = paste(
      "Mean latitude ",
      sprintf(
        fmt = "%.2f",
        round(x = mean(dt_smp[!duplicated(dt_smp$icao), ]$lat), digits = 2L)
      ),
      "°",
      sep = ""
    ),
    size  = 1.5,
    x     = 179L,
    y     = mean(dt_smp[!duplicated(dt_smp$icao), ]$lat) - 1.5
  ) +
  geom_text(
    data  = world,
    color = "black",
    hjust = 1L,
    label = paste(
      "PPA-weighted mean latitude ",
      sprintf(
        fmt = "%.2f",
        round(
          x      = as.numeric(crossprod(
            dt_smp[!duplicated(dt_smp$icao), ]$traffic,
            dt_smp[!duplicated(dt_smp$icao), ]$lat
          ) /
            sum(dt_smp[!duplicated(dt_smp$icao), ]$traffic)
          ),
          digits = 2L)
      ),
      "°",
      sep = ""
    ),
    size  = 1.5,
    x     = 179L,
    y     = as.numeric(
      crossprod(
        dt_smp[!duplicated(dt_smp$icao), ]$traffic,
        dt_smp[!duplicated(dt_smp$icao), ]$lat
      ) /
        sum(dt_smp[!duplicated(dt_smp$icao), ]$traffic)
    ) - 1.35
  ) +
  geom_text(
    data  = world,
    color = "black",
    hjust = 1L,
    label = paste(
      "Median latitude ",
      sprintf(
        fmt = "%.2f",
        round(x = median(dt_smp[!duplicated(dt_smp$icao), ]$lat), digits = 2L)
      ),
      "°",
      sep = ""
    ),
    size  = 1.5,
    x     = 179L,
    y     = median(dt_smp[!duplicated(dt_smp$icao), ]$lat) + 2L
  ) +
  geom_text(
    data  = world,
    color = "gray",
    hjust = 0L,
    label = "Arctic circle",
    size  = 1.5,
    x     = -179L,
    y     = unique(unlist(x = geo, use.names = FALSE))[5] - 2L
  ) +
  geom_text(
    data  = world,
    color = "gray",
    hjust = 0L,
    label = "Tropic of Cancer",
    size  = 1.5,
    x     = -179L,
    y     = unique(unlist(x = geo, use.names = FALSE))[4] - 2L
  ) +
  geom_text(
    data  = world,
    color = "gray",
    hjust = 0L,
    label = "Tropic of Capricorn",
    size  = 1.5,
    x     = -179L,
    y     = unique(unlist(x = geo, use.names = FALSE))[3] - 2L
  ) +
  geom_text(
    data  = world,
    color = "gray",
    hjust = 0L,
    label = "Antarctic circle",
    size  = 1.5,
    x     = -179L,
    y     = unique(unlist(x = geo, use.names = FALSE))[2] - 2L
  ) +
  # Add zonal labels
  geom_text(
    angle = 90L,
    data  = world,
    color = "gray",
    hjust = .5,
    label = unique(names(geo))[1],
    size  = 2.5,
    x     = -175L,
    y     = mean(
      c(
        unique(unlist(x = geo, use.names = FALSE))[1],
        unique(unlist(x = geo, use.names = FALSE))[2]
      )
    )
  ) +
  geom_text(
    angle = 90L,
    data  = world,
    color = "gray",
    hjust = .5,
    label = unique(names(geo))[2],
    size  = 2.5,
    x     = -175L,
    y     = mean(
      c(
        unique(unlist(x = geo, use.names = FALSE))[2],
        unique(unlist(x = geo, use.names = FALSE))[3]
      )
    )
  ) +
  geom_text(
    angle = 90L,
    data  = world,
    color = "gray",
    hjust = .5,
    label = unique(names(geo))[3],
    size  = 2.5,
    x     = -175L,
    y     = mean(
      c(
        unique(unlist(x = geo, use.names = FALSE))[3],
        unique(unlist(x = geo, use.names = FALSE))[4]
      )
    )
  ) +
  geom_text(
    angle = 90L,
    data  = world,
    color = "gray",
    hjust = .5,
    label = unique(names(geo))[2],
    size  = 2.5,
    x     = -175L,
    y     = mean(
      c(
        unique(unlist(x = geo, use.names = FALSE))[4],
        unique(unlist(x = geo, use.names = FALSE))[5]
      )
    )
  ) +
  geom_text(
    angle = 90L,
    data  = world,
    color = "gray",
    hjust = .5,
    label = unique(names(geo))[1],
    size  = 2.5,
    x     = -175L,
    y     = mean(
      c(
        unique(unlist(x = geo, use.names = FALSE))[5],
        unique(unlist(x = geo, use.names = FALSE))[6]
      )
    )
  ) +
  theme_light() +
  theme(
    axis.title      = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    legend.key.size = unit(.2, "in"),
    legend.title    = element_text(size = 5L),
    legend.text     = element_text(size = 5L),
    plot.margin     = margin(-.8, 0L, -.8, 0L, "in")
  )

# Save the plot
ggsave(
  filename = "2_map_of_sample_airports.png",
  plot     = last_plot(),
  device   = "png",
  path     = "plots",
  scale    = 1L,
  height   = 9L / ar,
  width    = 9L,
  units    = "in",
  dpi      = "retina"
)

# Build a histogram of airports by latitude
ggplot() +
  geom_histogram(
    mapping  = aes(x = dt_pop[!duplicated(dt_pop$icao), ]$lat),
    fill     = "black",
    alpha    = .5,
    binwidth = 10L,
    na.rm    = TRUE
  ) +
  geom_histogram(
    mapping  = aes(x = dt_smp[!duplicated(dt_smp$icao), ]$lat),
    fill     = "black",
    alpha    = .5,
    binwidth = 10L,
    na.rm    = TRUE
  ) +
  scale_x_continuous(
    name     = "Latitude",
    breaks   = seq(-90L, 90L, 10L),
    limits   = c(-90L, 90L)
  ) +
  scale_y_continuous(
    name = "Count of airports"
  ) +
  theme_light() +
  theme(panel.grid.minor = element_blank())

# Save the plot
ggsave(
  filename = "2_histogram_of_airports_by_latitude.png",
  plot     = last_plot(),
  device   = "png",
  path     = "plots",
  scale    = 1L,
  width    = 4L,
  height   = 4L,
  units    = "in",
  dpi      = "retina"
)

# Build a histogram of traffic by latitude
ggplot() +
  geom_histogram(
    mapping  = aes(
      x      = dt_pop[!duplicated(dt_pop$icao), ]$lat,
      weight = dt_pop[!duplicated(dt_pop$icao), ]$traffic
    ),
    fill     = "black",
    alpha    = 0.5,
    binwidth = 10L,
    na.rm    = TRUE
  ) +
  geom_histogram(
    mapping  = aes(
      x      = dt_smp[!duplicated(dt_smp$icao), ]$lat,
      weight = dt_smp[!duplicated(dt_smp$icao), ]$traffic
    ),
    fill     = "black",
    alpha    = 0.5,
    binwidth = 10L,
    na.rm    = TRUE
  ) +
  scale_x_continuous(
    name     = "Latitude",
    breaks   = seq(-90L, 90L, 10L),
    limits   = c(-90L, 90L)
  ) +
  scale_y_continuous(
    name     = "Sum of traffic",
    breaks   = seq(0L, 10^10, 5L * 10^8),
    labels   = label_number(scale_cut = cut_short_scale())
  ) +
  theme_light() +
  theme(panel.grid.minor = element_blank())

# Save the plot
ggsave(
  filename = "2_histogram_of_traffic_by_latitude.png",
  plot     = last_plot(),
  device   = "png",
  path     = "plots",
  scale    = 1L,
  width    = 4L,
  height   = 4L,
  units    = "in",
  dpi      = "retina"
)

# ==============================================================================
# 5 Calculate the Köppen-Geiger climate zones for population & sample airports
# ==============================================================================

# Pick a resolution for the KGC package processing, either "fine" or "course"
# (yes, "coarse" is misspelled in the package's source code).
res <- "course"

# Prepare the population data
df_kgc_pop <- dt_pop[
  !duplicated(dt_pop$icao),
  c("icao", "lon", "lat", "traffic")
] %>%
  mutate(rndCoord.lon = RoundCoordinates(lon, res = res, latlong = "lon")) %>%
  mutate(rndCoord.lat = RoundCoordinates(lat, res = res, latlong = "lat"))

# Compute the Köppen-Geiger climate zone for the population data
df_kgc_pop <- data.frame(
  df_kgc_pop,
  kgc = LookupCZ(df_kgc_pop, res = res, rc = FALSE)
)

# Summarize the Köppen-Geiger climate zonal distribution for the population data
df_kgc_pop <- df_kgc_pop %>%
  group_by(kgc) %>%
  dplyr::summarize(pop.airports = n(), pop.traffic = sum(traffic))

# Prepare the sample data
df_kgc_smp <- dt_smp[
  !duplicated(dt_smp$icao),
  c("icao", "lon", "lat", "traffic")
] %>%
  mutate(rndCoord.lon = RoundCoordinates(lon, res = res, latlong = "lon")) %>%
  mutate(rndCoord.lat = RoundCoordinates(lat, res = res, latlong = "lat"))

# Compute the Köppen-Geiger climate zone for the sample data
df_kgc_smp <- data.frame(df_kgc_smp,
  kgc = LookupCZ(df_kgc_smp, res = res, rc = FALSE)
)

# Summarize the Köppen-Geiger climate zonal distribution for the sample data
df_kgc_smp <- df_kgc_smp %>%
  group_by(kgc) %>%
  dplyr::summarize(smp.airports = n(), smp.traffic = sum(traffic))

# Merge the population and sample counts for row-wise comparison
df_kgc <- merge(df_kgc_pop, df_kgc_smp, by = "kgc", all = TRUE)

# Recode NAs with 0
df_kgc[is.na(df_kgc)] <- 0L

# De-factorize
df_kgc$kgc <- as.character(df_kgc$kgc)

# Recode missing Köppen-Geiger climate zones with Z
df_kgc$kgc[df_kgc$kgc == "Climate Zone info missing"] <- "Z"

# Re-factorize
df_kgc$kgc <- as.factor(df_kgc$kgc)

# Summarize the airport and traffic distribution by Köppen-Geiger climate zone
df_kgc %>%
  group_by(group = substr(kgc, 1L, 1L)) %>%
  dplyr::summarize(
    pop.airports     = sum(pop.airports),
    pop.airports.per = sum(pop.airports) / sum(df_kgc$pop.airports),
    pop.traffic      = sum(pop.traffic),
    pop.traffic.per  = sum(pop.traffic)  / sum(df_kgc$pop.traffic),
    smp.airports     = sum(smp.airports),
    smp.airports.per = sum(smp.airports) / sum(df_kgc$smp.airports),
    smp.traffic      = sum(smp.traffic),
    smp.traffic.per  = sum(smp.traffic)  / sum(df_kgc$smp.traffic)
  )

# Plot the airport distribution by Köppen-Geiger climate zone
ggplot(data = df_kgc) +
  geom_bar(
    mapping = aes(x = kgc, weight = pop.airports),
    fill    = "black",
    alpha   = .5,
    width   = 1L
  ) +
  geom_bar(
    mapping = aes(x = kgc, weight = smp.airports),
    fill    = "black",
    alpha   = .5,
    width   = 1L
  ) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2L)) +
  scale_y_continuous(trans = "log1p", breaks = c(2^(0:8))) +
  labs(x = "Köppen-Geiger climate zones", y = "Airport count") +
  theme_light() +
  theme(panel.grid.minor = element_blank())

# Save the plot
ggsave(
  filename = "2_histogram_of_airports_by_koppen_geiger_zone.png",
  plot     = last_plot(),
  device   = "png",
  path     = "plots",
  scale    = 1L,
  width    = 4.4,
  height   = 4.4,
  units    = "in",
  dpi      = "retina"
)

# Plot the traffic distribution by Köppen-Geiger climate zone
ggplot(data = df_kgc) +
  geom_bar(
    mapping  = aes(x = kgc, weight = pop.traffic),
    fill     = "black",
    alpha    = .5,
    width    = 1L
  ) +
  geom_bar(
    mapping  = aes(x = kgc, weight = smp.traffic),
    fill     = "black",
    alpha    = .5,
    width    = 1L
  ) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2L)) +
  scale_y_continuous(
    breaks   = seq(from = 0L, to = 10^10, by = 5L * 10^8),
    labels   = label_number(scale_cut = cut_short_scale())
  ) +
  labs(x = "Köppen-Geiger climate zones", y = "Sum of traffic") +
  theme_light() +
  theme(panel.grid.minor = element_blank())

# Save the plot
ggsave(
  filename = "2_histogram_of_traffic_by_koppen_geiger_zone.png",
  plot     = last_plot(),
  device   = "png",
  path     = "plots",
  scale    = 1L,
  width    = 4.4,
  height   = 4.4,
  units    = "in",
  dpi      = "retina"
)

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Stop the script timer
Sys.time() - start_time

# EOF
