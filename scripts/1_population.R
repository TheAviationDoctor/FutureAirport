# ==============================================================================
#    NAME: scripts/1_population.R
#   INPUT: CSV files for traffic, runways, and geolocation
# ACTIONS: Merge and de-duplicate the input files
#          Write the resulting dataset of airports and runways to the database
#          Plot population characteristics
#          Index the database table to speed up subsequent queries
#  OUTPUT: Plots saved to disk
#          8,817 rows of airport-runway pairs written to the database
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load required libraries
library(DBI)
library(scales)
library(tidyverse)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Examine the runway dataset
# ==============================================================================

# Load the runway data
df_rwy <- read.csv(
  file = f$rwy,
  header = TRUE,
  na.strings = c(0, "NULL"),
  colClasses = c(rep("character", 2), "integer")
)

# Describe the data
str(df_rwy)

# Count missing TODAs
sum(is.na(df_rwy$toda))

# Remove the missing TODAs
df_rwy <- na.omit(df_rwy)

# Convert the TODAs from feet to meters
df_rwy$toda <- floor(df_rwy$toda / 3.280839895)

# Count unique runways
length(unique(df_rwy$icao))

# Calculate mean count of runway headings per airport
nrow(df_rwy) / length(unique(df_rwy$icao))

# ==============================================================================
# 2 Examine the traffic dataset
# ==============================================================================

# Load the traffic data
df_tra <- read.csv(
  file       = f$tra,
  header     = TRUE,
  colClasses = c("character", "integer")
)

# Describe the data
str(df_tra)

# Sum up the total traffic
sum(df_tra$traffic)

# Describe the traffic variable
summary(df_tra$traffic)

# Define the traffic bins
breaks <- c(10^0, 10^1, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7, 10^8, 10^9)

# Define the bin labels
labels <- c(
  "[1–10)", "[10–100)", "[100–1K)", "[1K–10K)", "[10K–100K)",
  "[100K–1M)", "[1M–10M)", "[10M–100M)", "[100M–1B)"
)

# Bin the airports by passenger traffic
df_bin <- df_tra %>%
  mutate(
    bin = cut(
      x              = df_tra$traffic,
      breaks         = breaks,
      labels         = labels,
      include.lowest = TRUE,
      right          = FALSE
    )
  ) %>%
  group_by(bin) %>%
  summarize(
    airports = n(),
    traffic  = sum(traffic)
  ) %>%
  arrange(-row_number()) %>%
  mutate(
    airports_cum = cumsum(airports),
    airports_per = cumsum(airports) / sum(airports),
    traffic_cum  = cumsum(traffic),
    traffic_per  = cumsum(traffic) / sum(traffic)
  ) %>%
  relocate(
    bin, airports, airports_cum, airports_per,
    traffic, traffic_cum, traffic_per
  )

# Display the traffic distribution table
df_bin

# Define a coefficient to scale the secondary y axis proportionally to the first
coeff <- max(df_bin$traffic_per) / max(df_bin$airports)

# Plot the Pareto chart of passenger traffic by airport bin
(ggplot(data = df_bin) +
  geom_col(mapping = aes(x = bin, y = airports)) +
  geom_text(
    mapping  = aes(
      x      = bin,
      y      = airports,
      label  = scales::comma(airports, accuracy = 1)
    ),
    vjust    = 1.5,
    color    = ifelse(df_bin$airports < 100, "black", "white"),
    size     = 3.5
  ) +
  geom_point(
    mapping  = aes(x = bin, y = traffic_per / coeff),
    shape    = "square",
    size     = 2
  ) +
  geom_text(
    mapping  = aes(
      x      = bin,
      y      = traffic_per / coeff,
      label  = scales::percent(traffic_per, accuracy = 0.01)
    ),
    nudge_x  = -.35,
    nudge_y  = 50,
    color    = "black",
    size     = 3.5
  ) +
  geom_path(
    mapping  = aes(x = bin, y = traffic_per / coeff, group = 1),
    lty      = 1,
    size     = 0.9
  ) +
  scale_x_discrete(
    name     = "2019 passenger traffic bins",
    limits   = rev,
    guide    = guide_axis(n.dodge = 2)
  ) +
  scale_y_continuous(
    name     = "Count of airports (bars)",
    labels   = scales::comma,
    breaks   = c(0, 300, 600, 900, 1200),
    sec.axis = sec_axis(~ . * coeff,
      name   = "Cumulative percentage of 2019 passenger traffic (line)",
      labels = percent
    )
  ) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )) %>%
  ggsave(
    filename = "pop_bin.png",
    device   = "png",
    path     = d$plt,
    scale    = 1,
    width    = 6,
    height   = NA,
    units    = "in",
    dpi      = "print"
  )

# Plot the density of traffic by airport size
(ggplot(data = df_tra, mapping = aes(x = traffic)) +
  geom_density(alpha = .75, fill = "lightgray") +
  geom_vline(xintercept = mean(df_tra$traffic), color = "black") +
  geom_vline(
    xintercept = median(df_tra$traffic),
    color      = "black",
    linetype   = "dashed"
  ) +
  scale_x_continuous(
    name   = "2019 passenger traffic",
    breaks = breaks,
    trans  = "log10"
  ) +
  scale_y_continuous(name = "Density") +
  theme_light() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )) %>%
  ggsave(
    filename = "pop_tra.png",
    device   = "png",
    path     = d$plt,
    scale    = 1,
    width    = 6,
    height   = NA,
    units    = "in",
    dpi      = "print"
  )

# ==============================================================================
# 3 Examine the geolocation dataset
# ==============================================================================

# Load the geolocation data
df_geo <- read.csv(file = f$geo, header = TRUE)

# Describe the data
str(df_geo)

# Keep only non-closed airports with an IATA code
df_geo <- subset(df_geo,
  type %in% c("small_airport", "medium_airport", "large_airport") &
    icao != "" & nchar(iata) == 3,
  select = c("name", "lat", "lon", "icao", "iata")
)

# Count the remaining observations
nrow(df_geo)

# ==============================================================================
# 4 Combine the traffic and geolocation datasets into an airport dataset
# ==============================================================================

# Left join the traffic and geolocation datasets
df_apt <- merge(
  x = df_tra, y = df_geo, by.x = "iata", by.y = "iata",
  all.x = TRUE
)

# Count the resulting observations
nrow(df_apt)

# Check for missing ICAO codes
count(df_apt[!complete.cases(df_apt$icao), ])

# Describe the larger airports (>= sim$pop_thr passengers) missing an ICAO code
str(df_apt$iata[!complete.cases(df_apt$icao) & df_apt$traffic >= sim$pop_thr])

# Select the smaller airports (< sim$pop_thr passengers) missing an ICAO code
df_sma <- df_apt[!complete.cases(df_apt$icao) &
  df_apt$traffic < sim$pop_thr, ]

# Describe the smaller airports
str(df_sma$iata)

# Calculate the traffic at the smaller airports
sum(df_sma$traffic)

# Calculate the traffic share at the smaller airports
sum(df_sma$traffic) / sum(df_tra$traffic) * 100

# Transfer traffic from two large airports to another that absorbed them
df_tra$traffic[df_tra$iata == "BER"] <- df_tra$traffic[df_tra$iata == "SXF"] +
  df_tra$traffic[df_tra$iata == "TXL"]

# Remove those two larger airports
df_tra <- subset(df_tra, !(iata %in% c("SXF", "TXL")))

# Manually rename one larger airport whose IATA code changed
df_tra$iata[df_tra$iata == "TSE"] <- "NQZ"

# Remove the smaller airports
df_tra <- subset(df_tra, !(iata %in% df_sma$iata))

# Merge again now that the traffic dataset has been adjusted
df_apt <- merge(
  x = df_tra, y = df_geo, by.x = "iata", by.y = "iata",
  all.x = TRUE
)

# Count the resulting observations
nrow(df_apt)

# Check for duplicated IATA codes
df_apt[duplicated(df_apt$iata) | duplicated(df_apt$iata, fromLast = TRUE), ]

# Remove three false duplicates (i.e. different airports, same IATA code)
df_apt <- subset(df_apt, name != "Liuting Airport" &
  name != "Dewadaru - Kemujan Island" &
  name != "Yibin Caiba Airport")

# Remove first occurrence only of strict duplicates (i.e. keep one of each)
df_apt <- df_apt[!rev(duplicated(rev(df_apt$iata))), ]

# Count the resulting observations
nrow(df_apt)

# Order the population by decreasing traffic size
df_apt <- df_apt[order(df_apt$traffic, decreasing = TRUE), ]

# Reset the row index
row.names(df_apt) <- NULL

# Examine the resulting population
str(df_apt)

# ==============================================================================
# 5 Combine the airport and runway datasets into the population dataset
# ==============================================================================

# Left join the resulting airport dataset and runway dataset
df_pop <- merge(
  x = df_apt, y = df_rwy, by.x = "icao", by.y = "icao",
  all.x = TRUE
)

# Describe the data
str(df_pop)

# Check for and remove missing runways
count(df_pop[!complete.cases(df_pop$rwy), ])
df_pop <- subset(df_pop, complete.cases(df_pop$rwy))

# Count the resulting runways
nrow(df_pop)

# Count the resulting airports
length(unique(df_pop$icao))

# Count the resulting traffic
sum(df_pop$traffic[!rev(duplicated(rev(df_pop$icao)))])

# Create column to identify unique runways (i.e. reciprocal headings
# sharing the same physical surface and same TODA at a given airport).
df_pop_unique <- df_pop %>%
  mutate(rwy.recip = if_else(
    parse_number(rwy) <= 18L,
    paste("RW",
      formatC(parse_number(rwy) + 18L, width = 2, format = "d", flag = "0"),
      case_when(
        str_sub(rwy, -1, -1) == "L" ~ "R",
        str_sub(rwy, -1, -1) == "R" ~ "L",
        str_sub(rwy, -1, -1) == "C" ~ "C", TRUE ~ ""
      ),
      sep = ""
    ),
    paste("RW",
      formatC(parse_number(rwy) - 18L, width = 2, format = "d", flag = "0"),
      case_when(
        str_sub(rwy, -1, -1) == "L" ~ "R",
        str_sub(rwy, -1, -1) == "R" ~ "L",
        str_sub(rwy, -1, -1) == "C" ~ "C", TRUE ~ ""
      ),
      sep = ""
    )
  )) %>%
  mutate(rwy.concat = pmap_chr(list(rwy, rwy.recip), ~ paste(
    sort(c(...)),
    collapse = "-"
  ))) %>%
  unite("rwy.unique", c("icao", "rwy.concat", "toda"),
    sep = "-",
    remove = FALSE
  ) %>%
  arrange(desc(traffic), rwy.unique) %>%
  select(rwy.unique)

# Count unique runways
length(unique(df_pop_unique$rwy.unique)) # Unique count

# Count unique percentage
length(unique(df_pop_unique$rwy.unique)) / nrow(df_pop)

# Count non-unique runways
nrow(df_pop) - length(unique(df_pop_unique$rwy.unique))

# Non-unique percentage
(nrow(df_pop) - length(unique(df_pop_unique$rwy.unique))) / nrow(df_pop)

# Describe the runway length variable
summary(df_pop$len)

# Describe the TODA variable
summary(df_pop$toda)

# Order the merged dataset by decreasing traffic size and ICAO code
df_pop <- df_pop[order(df_pop$traffic, df_pop$icao, decreasing = TRUE), ]

# Reset the row index
row.names(df_pop) <- NULL

# ==============================================================================
# 6 Save the population to a database
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build the query to drop the population table if it exists
db_qry <- paste("DROP TABLE IF EXISTS ", tolower(db$pop), ";", sep = "")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Create the population table
db_qry <- paste("CREATE TABLE ",
  tolower(db$pop),
  "(id SMALLINT NOT NULL AUTO_INCREMENT,
  icao CHAR(4) NOT NULL,
  iata CHAR(3) NOT NULL,
  traffic INT NOT NULL,
  name CHAR(", max(nchar(df_pop$name)), ") NOT NULL,
  lat FLOAT NOT NULL,
  lon FLOAT NOT NULL,
  rwy CHAR(5) NOT NULL,
  toda SMALLINT NOT NULL,
  PRIMARY KEY (id));",
  sep = ""
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Write the population data to the database
dbWriteTable(
  conn      = db_con,
  name      = tolower(db$pop),
  value     = df_pop,
  append    = TRUE,
  row.names = FALSE
)

# ==============================================================================
# 7 Index the database table
# ==============================================================================

# Build the query to create the index
db_qry <- paste(
  "CREATE INDEX idx ON", tolower(db$pop), "(icao, traffic, lat, lon);",
  sep = " "
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# ==============================================================================
# 8 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF
