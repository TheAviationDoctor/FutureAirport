# ==============================================================================
#    NAME: scripts/2_sample.R
#   INPUT: 8,817 population airports created in 1_population.R
# ACTIONS: Subset the airport population data based on a set traffic threshold
#          Plot sample characteristics
#  OUTPUT: Plots saved to disk
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2022
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load required libraries
library(DBI)
library(geosphere)
library(kgc)
library(maps)
library(rgeos)
library(rnaturalearth)
library(tidyverse)

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
df_pop <- fn_sql_qry(
  statement = paste(
    "SELECT * FROM ", dat$pop, ";", sep = ""
  )
)

# Describe the population
str(df_pop)
summary(df_pop)

# ==============================================================================
# 2 Subset the sample from the population based on a minimum traffic threshold
# ==============================================================================

# Sample airports above the minimum traffic threshold in passengers
df_smp <- subset(df_pop, traffic >= sim$pop_thr)

# Describe the sample
str(df_smp)
summary(df_smp)

# ==============================================================================
# 3 Test that the sample is representative of the population's traffic
# ==============================================================================

# Absolute count of airports in the sample
length(unique(df_smp$icao))

# Absolute count of runways in the sample
nrow(df_smp)

# Absolute count of passengers in the sample
sum(df_smp$traffic[!rev(duplicated(rev(df_smp$icao)))])

# Relative count of airports in the sample
length(unique(df_smp$icao)) / length(unique(df_pop$icao)) * 100L

# Relative count of runways in the sample
nrow(df_smp) / nrow(df_pop) * 100L

# Relative count of passengers in the sample
sum(df_smp$traffic[!rev(duplicated(rev(df_smp$icao)))]) /
  sum(df_pop$traffic[!rev(duplicated(rev(df_pop$icao)))]) * 100L

# ==============================================================================
# 4 Test that the sample is representative of the population's latitudes
# ==============================================================================

# Define the world object from the Natural Earth package
world <- ne_countries(scale = "small", returnclass = "sf")

# Define the Earth's five geographical zones
geo_zones <- c(-90, -66.5635, -23.4365, 23.4365, 66.5635, 90)
geo_labels <- c(
  "Antarctic", "South temperate zone", "Tropics",
  "North temperate zone", "Arctic"
)

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

# Describe the population's latitude variable
length(df_pop$lat[!rev(duplicated(rev(df_pop$icao)))])
summary(df_pop$lat[!rev(duplicated(rev(df_pop$icao)))])

# Describe the sample's latitude variable
length(df_smp$lat[!rev(duplicated(rev(df_smp$icao)))])
summary(df_smp$lat[!rev(duplicated(rev(df_smp$icao)))])

# Calculate the distance from the equator to the population's median
distm(
  c(0L, median(df_pop$lat[!rev(duplicated(rev(df_pop$icao)))])),
  c(0L, 0L),
  fun = distHaversine
) / 1000L

# Calculate the distance from the equator to the population's mean
distm(
  c(0L, mean(df_pop$lat[!rev(duplicated(rev(df_pop$icao)))])),
  c(0L, 0L),
  fun = distHaversine
) / 1000L

# Calculate the distance from the equator to the sample's median
distm(
  c(0L, median(df_smp$lat[!rev(duplicated(rev(df_smp$icao)))])),
  c(0L, 0L),
  fun = distHaversine
) / 1000L

# Calculate the distance from the equator to the sample's mean
distm(
  c(0L, mean(df_smp$lat[!rev(duplicated(rev(df_smp$icao)))])),
  c(0L, 0L),
  fun = distHaversine
) / 1000L

# Calculate the distance from the median to the population's mean
distm(
  c(0L, median(df_pop$lat[!rev(duplicated(rev(df_pop$icao)))])),
  c(0L, mean(df_pop$lat[!rev(duplicated(rev(df_pop$icao)))])),
  fun = distHaversine
) / 1000L

# Calculate the distance from the median to the sample's mean
distm(
  c(0L, median(df_smp$lat[!rev(duplicated(rev(df_smp$icao)))])),
  c(0L, mean(df_smp$lat[!rev(duplicated(rev(df_smp$icao)))])),
  fun = distHaversine
) / 1000L

# Find the population's northernmost airport
df_pop[which.max(df_pop$lat), c(4L, 5L)]

# Find the population's southernmost airport
df_pop[which.min(df_pop$lat), c(4L, 5L)]

# Find the sample's northernmost airport
df_smp[which.max(df_smp$lat), c(4L, 5L)]

# Find the sample's southernmost airport
df_smp[which.min(df_smp$lat), c(4L, 5L)]

# Bin the population airports (not runways) by passenger traffic and geo. zones
df_pop_binned <- df_pop[!duplicated(df_pop$icao), ] %>%
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
      breaks         = geo_zones,
      labels         = geo_labels,
      include.lowest = TRUE,
      right          = FALSE
    )
  )

# Bin the sample airports (not runways) by passenger traffic and geo. zones
df_smp_binned <- df_smp[!duplicated(df_smp$icao), ] %>%
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
      breaks         = geo_zones,
      labels         = geo_labels,
      include.lowest = TRUE,
      right          = FALSE
    )
  )

# Count the population airports by geographical zone
df_pop_binned %>%
  group_by(geo) %>%
  dplyr::summarize(n = n()) %>%
  mutate(per = percent(n / nrow(df_pop_binned), accuracy = .01))

# Count the sample airports by geographical zone
df_smp_binned %>%
  group_by(geo) %>%
  dplyr::summarize(n = n()) %>%
  mutate(per = percent(n / nrow(df_smp_binned), accuracy = .01))

# Sum the population traffic by geographical zone
df_pop_binned %>%
  group_by(geo) %>%
  summarize(n = sum(traffic)) %>%
  mutate(per = percent(n / sum(df_pop[!duplicated(df_pop$icao), ]$traffic),
    accuracy = .01
  ))

# Sum the sample traffic by geographical zone
df_smp_binned %>%
  group_by(geo) %>%
  summarize(n = sum(traffic)) %>% # Sum
  mutate(per = percent(n / sum(df_smp[!duplicated(df_smp$icao), ]$traffic),
    accuracy = .01
  ))

# Build a world map and plot the airports
(ggplot() +
  geom_sf(data = world, color = NA, fill = "gray", alpha = .5) +
  scale_y_continuous(breaks = geo_zones) +
  coord_sf(expand = FALSE) +
  geom_point(
    data     = df_pop[!duplicated(df_pop$icao), ],
    mapping  = aes(x = lon, y = lat),
    color    = "black",
    shape    = 20L,
    size     = 1.5
  ) +
  geom_point(
    data     = df_smp[!duplicated(df_smp$icao), ],
    mapping  = aes(x = lon, y = lat),
    color    = "purple",
    shape    = 20L,
    size     = 1.5
  ) +
  geom_hline(
    aes(yintercept = df_pop$lat[which.max(df_pop$lat)]),
    color    = "black"
  ) +
  geom_hline(
    aes(yintercept = df_smp$lat[which.max(df_smp$lat)]),
    color    = "purple"
  ) +
  geom_hline(
    aes(yintercept = df_pop$lat[which.min(df_pop$lat)]),
    color    = "black"
  ) +
  geom_hline(
    aes(yintercept = df_smp$lat[which.min(df_smp$lat)]),
    color    = "purple"
  ) +
  geom_hline(
    aes(yintercept = mean(df_pop[!duplicated(df_pop$icao), ]$lat)),
    color    = "black"
  ) +
  geom_hline(
    aes(yintercept = mean(df_smp[!duplicated(df_smp$icao), ]$lat)),
    color    = "purple"
  ) +
  geom_hline(
    aes(yintercept = median(df_pop[!duplicated(df_pop$icao), ]$lat)),
    color    = "black"
  ) +
  geom_hline(
    aes(yintercept = median(df_smp[!duplicated(df_smp$icao), ]$lat)),
    color    = "purple"
  ) +
  geom_hline(
    aes(yintercept = 66.5635),
    color    = "gray",
    linetype = "dashed"
  ) +
  geom_hline(
    aes(yintercept = 23.4365),
    color    = "gray",
    linetype = "dashed"
  ) +
  geom_hline(
    aes(yintercept = -23.4365),
    color    = "gray",
    linetype = "dashed"
  ) +
  geom_hline(
    aes(yintercept = -66.5635),
    color    = "gray",
    linetype = "dashed"
  ) +
  theme_light() +
  theme(axis.title = element_blank(), axis.text.x = element_blank())) %>%
  ggsave(
    filename = "2_world.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 6L,
    height   = NA,
    units    = "in",
    dpi      = "print"
  )

# Build a histogram of the airport count by latitude
(ggplot() +
  geom_histogram(
    mapping  = aes(x = df_pop[!duplicated(df_pop$icao), ]$lat),
    fill     = "black",
    alpha    = 0.5,
    binwidth = 10L,
    na.rm    = TRUE
  ) +
  geom_histogram(
    mapping  = aes(x = df_smp[!duplicated(df_smp$icao), ]$lat),
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
    name = "Count of airports"
  ) +
  theme_light() +
  theme(panel.grid.minor = element_blank())) %>%
  ggsave(
    filename = "2_hist_apt_cnt.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 6L,
    height   = NA,
    units    = "in",
    dpi      = "print"
  )

# Build a histogram of the airport traffic by latitude
(ggplot() +
  geom_histogram(
    mapping = aes(
      x      = df_pop[!duplicated(df_pop$icao), ]$lat,
      weight = df_pop[!duplicated(df_pop$icao), ]$traffic
    ),
    fill = "black",
    alpha = 0.5,
    binwidth = 10L,
    na.rm = TRUE
  ) +
  geom_histogram(
    mapping = aes(
      x      = df_smp[!duplicated(df_smp$icao), ]$lat,
      weight = df_smp[!duplicated(df_smp$icao), ]$traffic
    ),
    fill = "black",
    alpha = 0.5,
    binwidth = 10L,
    na.rm = TRUE
  ) +
  scale_x_continuous(
    name     = "Latitude",
    breaks   = seq(-90L, 90L, 10L),
    limits   = c(-90L, 90L)
  ) +
  scale_y_continuous(
    name     = "Sum of traffic",
    breaks   = seq(0L, 10^10, 5L * 10^8),
    labels   = label_number_si(accuracy = 0.1)
  ) +
  theme_light() +
  theme(panel.grid.minor = element_blank())) %>%
  ggsave(
    filename = "2_hist_apt_tra.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 6L,
    height   = NA,
    units    = "in",
    dpi      = "print"
  )

# ==============================================================================
# 5 Calculate the Köppen-Geiger climate zones for population & sample airports
# ==============================================================================

# Pick a resolution for the KGC package processing, either "fine" or "course"
# (yes, "coarse" is misspelled in the package's source code).
res <- "course"

# Prepare the population data
df_kgc_pop <- df_pop[
  !duplicated(df_pop$icao),
  c("icao", "lon", "lat", "traffic")
] %>%
  mutate(rndCoord.lon = RoundCoordinates(lon, res = res, latlong = "lon")) %>%
  mutate(rndCoord.lat = RoundCoordinates(lat, res = res, latlong = "lat"))

# Compute the climate zone for the population data
df_kgc_pop <- data.frame(df_kgc_pop,
  kgc = LookupCZ(df_kgc_pop, res = res, rc = FALSE)
)

# Summarize the climate distribution for the population data
df_kgc_pop <- df_kgc_pop %>%
  group_by(kgc) %>%
  dplyr::summarize(pop.airports = n(), pop.traffic = sum(traffic))

# Prepare the sample data
df_kgc_smp <- df_smp[
  !duplicated(df_smp$icao),
  c("icao", "lon", "lat", "traffic")
] %>%
  mutate(rndCoord.lon = RoundCoordinates(lon, res = res, latlong = "lon")) %>%
  mutate(rndCoord.lat = RoundCoordinates(lat, res = res, latlong = "lat"))

# Compute the climate zone for the sample data
df_kgc_smp <- data.frame(df_kgc_smp,
  kgc = LookupCZ(df_kgc_smp, res = res, rc = FALSE)
)

# Summarize the climate distribution for the sample data
df_kgc_smp <- df_kgc_smp %>%
  group_by(kgc) %>%
  dplyr::summarize(smp.airports = n(), smp.traffic = sum(traffic))

# Merge the population and sample counts for row-wise comparison
df_kgc <- merge(df_kgc_pop, df_kgc_smp, by = "kgc", all = TRUE)

# Recode NAs with 0
df_kgc[is.na(df_kgc)] <- 0L

# De-factorize
df_kgc$kgc <- as.character(df_kgc$kgc)

# Recode missing climate zones with Z
df_kgc$kgc[df_kgc$kgc == "Climate Zone info missing"] <- "Z"

# Re-factorize
df_kgc$kgc <- as.factor(df_kgc$kgc)

# View the summarized table of main climate groups
df_kgc %>%
  group_by(group = substr(kgc, 1L, 1L)) %>%
  dplyr::summarize(
    pop.airports = sum(pop.airports),
    pop.airports.per = percent(
      sum(pop.airports) / sum(df_kgc$pop.airports),
      accuracy = .01
    ),
    pop.traffic = sum(pop.traffic),
    pop.traffic.per = percent(
      sum(pop.traffic) / sum(df_kgc$pop.traffic),
      accuracy = .01
    ),
    smp.airports = sum(smp.airports),
    smp.airports.per = percent(
      sum(smp.airports) / sum(df_kgc$smp.airports),
      accuracy = .01
    ),
    smp.traffic = sum(smp.traffic),
    smp.traffic.per = percent(
      sum(smp.traffic) / sum(df_kgc$smp.traffic),
      accuracy = .01
    )
  ) %>%
  print()

# Plot the airport distribution across climate zones
(ggplot(data = df_kgc) +
  geom_bar(
    mapping = aes(x = kgc, weight = pop.airports),
    fill    = "black",
    alpha   = 0.5,
    width   = 1L
  ) +
  geom_bar(
    mapping = aes(x = kgc, weight = smp.airports),
    fill    = "black",
    alpha   = 0.5,
    width   = 1L
  ) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2L)) +
  scale_y_continuous(trans = "log1p", breaks = c(2^(0:8))) +
  labs(x = "Köppen-Geiger climate zones", y = "Airport count") +
  theme_light() +
  theme(panel.grid.minor = element_blank())) %>%
  ggsave(
    filename = "2_apt_dist_cz.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 6L,
    height   = NA,
    units    = "in",
    dpi      = "print"
  )

# Plot the traffic distribution across climate zones
(ggplot(data = df_kgc) +
  geom_bar(
    mapping = aes(x = kgc, weight = pop.traffic),
    fill    = "black",
    alpha   = 0.5,
    width   = 1L
  ) +
  geom_bar(
    mapping = aes(x = kgc, weight = smp.traffic),
    fill    = "black",
    alpha   = 0.5,
    width   = 1L
  ) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2L)) +
  scale_y_continuous(
    breaks = seq(from = 0L, to = 10^10, by = 5L * 10^8),
    labels = label_number_si(accuracy = 0.1)
  ) +
  labs(x = "Köppen-Geiger climate zones", y = "Sum of traffic") +
  theme_light() +
  theme(panel.grid.minor = element_blank())) %>%
  ggsave(
    filename = "2_tra_dist_cz.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 6L,
    height   = NA,
    units    = "in",
    dpi      = "print"
  )

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF
