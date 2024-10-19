# ==============================================================================
#    NAME: scripts/1_airports.R
#   INPUT: CSV files of passenger traffic and airport coordinates
# ACTIONS: Assemble the airport population
#  OUTPUT: 907 rows written to a .csv file
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2024
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Clear the environment
rm(list = ls())

# Load the required libraries
library(tidyverse)

# Files to read from
fls <- list(
  "geo" = "data/apt/geolocation.csv", # Airport coordinates from OurAirports.com
  "tra" = "data/apt/traffic.csv",     # 2019 traffic by airport from IATA
  "apt" = "data/apt/airports.csv"     # Output list of airports
)

# Parameters
par <- list(
  "ft_to_m" = .3048, # Number of m in one ft
  "pop_thr" = 10^6   # Minimum passenger traffic for airport sample
)

# Latitudinal boundaries of the Earth's climate zones
geo <- list(
  "Frigid"    = c(-90L,     -66.5635), # Antarctic zone
  "Temperate" = c(-66.5635, -23.4365), # South temperate zone
  "Tropical"  = c(-23.4365,  23.4365), # Tropical zone
  "Temperate" = c( 23.4365,  66.5635), # North temperate zone
  "Frigid"    = c( 66.5635,  90L)      # Arctic zone
)

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Transform the airport traffic, and geolocation data
# ==============================================================================

# Load the traffic data for airports above the threshold
df_tra <- read.csv(
  file       = fls$tra,
  header     = TRUE,
  colClasses = c("character", "integer")
) |> subset(traffic > par$pop_thr)

# Load the geolocation data
df_geo <- read.csv(file = fls$geo, header = TRUE) |>
  # Keep only open airports with IATA & ICAO codes
  subset(endsWith(type, "airport") & icao != "" & nchar(iata) == 3L) |>
  # Keep only relevant columns
  select("name", "lat", "lon", "icao", "iata") |>
  # Assign each airport to a climate zone based on its latitude
  mutate(zone = names(
    x = geo[
      findInterval(x = lat, vec = unique(unlist(x = geo, use.names = FALSE)))
    ]
  )
)

# Left join the traffic and geolocation datasets
df_apt <- merge(
  x     = df_tra,
  y     = df_geo,
  by.x  = "iata",
  by.y  = "iata",
  all.x = TRUE
)

# Transfer traffic from two large airports to another that absorbed them
df_tra$traffic[df_tra$iata == "BER"] <- df_tra$traffic[df_tra$iata == "SXF"] +
  df_tra$traffic[df_tra$iata == "TXL"]

# Remove those two larger airports
df_tra <- subset(x = df_tra, !(iata %in% c("SXF", "TXL")))

# Manually rename one larger airport whose IATA code changed
df_tra$iata[df_tra$iata == "TSE"] <- "NQZ"

# Remove a duplicate (same airport, different names)
df_apt <- subset(x = df_apt, name != "Liuting Airport")

# Remove a duplicate (same IATA code, different airports)
df_apt <- subset(x = df_apt, icao != "UTHU")

# Order the rows and columns
df_apt <- df_apt[order(df_apt$traffic, decreasing = TRUE), ] |>
  select("iata", "icao", "name", "lat", "lon", "zone")

# Reset the row index
row.names(df_apt) <- NULL

# Save to file 
write.csv(df_apt, file = fls$apt)

# ==============================================================================
# 2 Housekeeping
# ==============================================================================

# Stop the script timer
Sys.time() - start_time

# EOF