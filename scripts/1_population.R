################################################################################
#    NAME: scripts/1_population.R                                              #
#   INPUT: CSV files located in data/population (traffic, runways, geolocation)#
# ACTIONS: Assemble the input files using merge and deduplication              #
#          Write the resulting dataset of airports and runways to the database #
#          Plot population characteristics to charts in plots/pop_*            #
#          Index the database table pop to speed up subsequent queries         #
#  OUTPUT: 8,817 rows of airport-runway pairs written to the database table pop#
# RUNTIME: ~2 seconds on the researcher's config (https://bit.ly/3ChCBAP)      #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(DBI)
library(dplyr)
library(ggplot2)
library(magrittr)
library(purrr)
library(readr)
library(scales)
library(stringr)
library(tidyr)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

################################################################################
# Examine the runway dataset                                                   #
################################################################################

# Load the runway data from a CSV file
df_rwy <- read.csv(file = paste(path_pop, pop_rwy, sep = "/"), header = TRUE, na.strings = c(0, "NULL"), col.names = c("id", "area", "icao", "country", "rwy", "len", "lat", "lon", "toda", "toda.night", "unit"), colClasses = c("integer", "factor", "character", "factor", "character", "integer", "character", "character", "integer", "integer", "factor"))

# Describe the data
str(df_rwy)

# Retain only the variables of interest
df_rwy <- subset(df_rwy, select = c("icao", "rwy", "len", "toda"))

# Count missing TODAs
sum(is.na(df_rwy$toda))

# Remove the missing TODAs
df_rwy <- na.omit(df_rwy)

# Count unique observations (runways)
length(unique(df_rwy$icao))

# Calculate mean count of observations (runway headings) per airport
nrow(df_rwy) / length(unique(df_rwy$icao))

################################################################################
# Examine the traffic dataset                                                  #
################################################################################

# Load the traffic data from a CSV file
df_tra <- read.csv(file = paste(path_pop, pop_tra, sep = "/"), header = TRUE, col.names = c("iata", "traffic"), colClasses = c("character", "integer"))

# Describe the data
str(df_tra)

# Sum up the total traffic
sum(df_tra$traffic)

# Describe the traffic variable
summary(df_tra$traffic)

# Define traffic bins and their labels
breaks <- c(10^0, 10^1, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7, 10^8, 10^9)
labels <- c("[1–10)", "[10–100)", "[100–1K)", "[1K–10K)", "[10K–100K)", "[100K–1M)", "[1M–10M)", "[10M–100M)", "[100M–1B)")

# Bin the airports by passenger traffic
df_tra_binned <- df_tra %>%
  mutate(bin = cut(df_tra$traffic, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)) %>% # Add the bin to each airport
  group_by(bin) %>% # Group the airports by bin
  summarize(airports = n(), traffic = sum(traffic)) %>% # Summarize the airport count and traffic by bin
  arrange(-row_number()) %>% # Reverse the order of rows to start with the largest bin
  mutate( # Add columns to track the cumulative count of airports and traffic
    airports_cum = cumsum(airports),
    airports_per = cumsum(airports) / sum(airports),
    traffic_cum = cumsum(traffic),
    traffic_per = cumsum(traffic) / sum(traffic)
  ) %>%
  relocate(bin, airports, airports_cum, airports_per, traffic, traffic_cum, traffic_per) # Reorder the columns

# Display the traffic distribution table
df_tra_binned

# Plot the Pareto chart of passenger traffic by airport bin
coeff <- max(df_tra_binned$traffic_per)/max(df_tra_binned$airports) # Define a coefficient to scale the secondary y axis proportionally to the first
plot_bin <- ggplot(data = df_tra_binned) +
  geom_col(mapping = aes(x = bin, y = airports)) +
  geom_text(mapping = aes(x = bin, y = airports, label = comma(airports, accuracy = 1)), vjust = 1.5, color = ifelse(df_tra_binned$airports < 100, "black", "white"), size = 3.5) +
  geom_point(mapping = aes(x = bin, y = traffic_per / coeff), shape = "square", size = 2) +
  geom_text(mapping = aes(x = bin, y = traffic_per / coeff, label = percent(traffic_per, accuracy = 0.01)), nudge_x = -.35, nudge_y = 50, color = "black", size = 3.5) +
  geom_path(mapping = aes(x = bin, y = traffic_per / coeff, group = 1), lty = 1, size = 0.9) +
  scale_x_discrete(name = "2019 passenger traffic bins", limits = rev) +
  scale_y_continuous(name = "Count of airports (bars)", labels = comma, breaks = c(0, 300, 600, 900, 1200), sec.axis = sec_axis(~ . * coeff, name = "Cumulative percentage of 2019 passenger traffic (line)", labels = percent)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

# Save the plot to a file
ggsave(
  paste("pop_bin.png"),
  plot = plot_bin,
  device = "png",
  path = path_plt,
  scale = 1,
  width = 6,
  height = NA,
  units = "in",
  dpi = "print",
  limitsize = TRUE,
  bg = NULL
)

# Plot the density plot of traffic by airport size
plot_tra <- ggplot(data = df_tra, mapping = aes(x = traffic)) +
  geom_density(alpha = .75, fill = "lightgray") +
  geom_vline(xintercept = mean(df_tra$traffic), color = "black") +
  geom_vline(xintercept = median(df_tra$traffic), color = "black", linetype = "dashed") +
  scale_x_continuous(name = "2019 passenger traffic", breaks = breaks, trans = "log10") +
  scale_y_continuous(name = "Density") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# Save the plot to a file
ggsave(
  paste("pop_tra.png"),
  plot = plot_tra,
  device = "png",
  path = path_plt,
  scale = 1,
  width = 6,
  height = NA,
  units = "in",
  dpi = "print",
  limitsize = TRUE,
  bg = NULL
)

################################################################################
# Examine the geolocation dataset                                              #
################################################################################

# Load the geolocation data from a CSV file
df_geo <- read.csv(file = paste(path_pop, pop_geo, sep = "/"), header = TRUE, col.names = c("id", "ident", "type", "name", "lat", "lon", "elev", "cont", "country", "region", "city", "sched", "icao", "iata", "local", "website", "wiki", "keywords"), colClasses = c("integer", "character", "factor", "character", "numeric", "numeric", "numeric", "factor", "factor", "factor", "character", "factor", "character", "character", "character", "character", "character", "character"))

# Describe the data
str(df_geo)

# Keep only non-closed airports with an IATA code and count the resulting observations
df_geo <- subset(df_geo, type %in% c("small_airport", "medium_airport", "large_airport") & icao != "" & nchar(iata) == 3, select = c("name", "lat", "lon", "icao", "iata"))
nrow(df_geo)

################################################################################
# Combine the traffic and geolocation datasets into an airport dataset         #
################################################################################

# Left join the traffic and geolocation datasets
df_apt <- merge(x = df_tra, y = df_geo, by.x = "iata", by.y = "iata", all.x = TRUE)

# Count the resulting observations
nrow(df_apt)

# Check for missing ICAO codes
count(df_apt[!complete.cases(df_apt$icao),])

# Describe the larger airports (>= 10^6 passengers) missing an ICAO code
str(df_apt$iata[!complete.cases(df_apt$icao) & df_apt$traffic >= 10^6])

# Describe the smaller airports (< 10^6 passengers) missing an ICAO code
missing_small <- df_apt[!complete.cases(df_apt$icao) & df_apt$traffic < 10^6,]
str(missing_small$iata) # Describe
sum(missing_small$traffic) # Calculate traffic
sum(missing_small$traffic) / sum(df_tra$traffic) * 100 # Calculate traffic share

# Impute or remove the airports missing an ICAO code from the upstream traffic dataset
df_tra$traffic[df_tra$iata == "BER"] <- df_tra$traffic[df_tra$iata == "SXF"] + df_tra$traffic[df_tra$iata == "TXL"] # Manually transfer traffic from two large airports to another that absorbed them
df_tra <- subset(df_tra, !(iata %in% c("SXF", "TXL"))) # Remove those two larger airports
df_tra$iata[df_tra$iata == "TSE"] <- "NQZ" # Manually rename one larger airport whose IATA code changed
df_tra <- subset(df_tra, !(iata %in% missing_small$iata)) # Remove the smaller airports

# Merge again now that the traffic dataset has been adjusted
df_apt <- merge(x = df_tra, y = df_geo, by.x = "iata", by.y = "iata", all.x = TRUE)

# Count the resulting observations
nrow(df_apt)

# Check for duplicated IATA codes
df_apt[duplicated(df_apt$iata) | duplicated(df_apt$iata, fromLast = TRUE),]

# Remove duplicates and count the resulting cases
df_apt <- subset(df_apt, name != "Liuting Airport" & name != "Dewadaru - Kemujan Island" & name != "Yibin Caiba Airport") # Remove three occurrences of false duplicates (i.e. two different airports with the same IATA code)
df_apt <- df_apt[!rev(duplicated(rev(df_apt$iata))),] # Remove first occurrence only of strict duplicates (i.e. keep one of each)
nrow(df_apt) # Count

# Order the population by decreasing traffic size and reset the row index
df_apt <- df_apt[order(df_apt$traffic, decreasing = TRUE),] # Order
row.names(df_apt) <- NULL # Reset row

# Examine the resulting population
str(df_apt)

################################################################################
# Combine the airport and runway datasets into the population dataset          #
################################################################################

# Left join the resulting airport dataset and runway dataset
df_pop <- merge(x = df_apt, y = df_rwy, by.x = "icao", by.y = "icao", all.x = TRUE)

# Describe the data
str(df_pop)

# Check for and remove missing runways
count(df_pop[!complete.cases(df_pop$rwy),])
df_pop <- subset(df_pop, complete.cases(df_pop$rwy))

# Count the resulting cases and mean number of runways per airport
nrow(df_pop) # Runways
length(unique(df_pop$icao)) # Airports
sum(df_pop$traffic[!rev(duplicated(rev(df_pop$icao)))]) # Traffic

# Create column to uniquely identify unique runways (i.e. reciprocal headings sharing the same physical surface and same TODA at a given airport)
df_pop_unique <- df_pop %>%
  mutate(rwy.recip = if_else( # Calculate reciprocal heading for each runway
    parse_number(rwy) <= 18,  # 180 degrees
    paste("RW", formatC(parse_number(rwy) + 18, width = 2, format = "d", flag = "0"), case_when(str_sub(rwy, -1, -1) == "L" ~ "R", str_sub(rwy, -1, -1) == "R" ~ "L", str_sub(rwy, -1, -1) == "C" ~ "C", TRUE ~ ""), sep = ""),
    paste("RW", formatC(parse_number(rwy) - 18, width = 2, format = "d", flag = "0"), case_when(str_sub(rwy, -1, -1) == "L" ~ "R", str_sub(rwy, -1, -1) == "R" ~ "L", str_sub(rwy, -1, -1) == "C" ~ "C", TRUE ~ ""), sep = "")
  )) %>%
  mutate(rwy.concat = pmap_chr(list(rwy, rwy.recip), ~ paste(sort(c(...)), collapse = "-"))) %>%
  unite("rwy.unique", c("icao", "rwy.concat", "toda"), sep = "-", remove = FALSE) %>%
  arrange(desc(traffic), rwy.unique) %>%
  select(rwy.unique)

# Count unique and non-unique runways in the population (i.e. reciprocal headings sharing the same physical surface and same TODA at a given airport)
length(unique(df_pop_unique$rwy.unique)) # Unique count
length(unique(df_pop_unique$rwy.unique)) / nrow(df_pop) # Unique percentage
nrow(df_pop) - length(unique(df_pop_unique$rwy.unique)) # Non-unique count
(nrow(df_pop) - length(unique(df_pop_unique$rwy.unique))) / nrow(df_pop) # Non-unique percentage

# Describe the runway length and TODA statistics of the population
summary(df_pop$len)
summary(df_pop$toda)

# Order the merged dataset by decreasing traffic size and ICAO code
df_pop <- df_pop[order(df_pop$traffic, df_pop$icao, decreasing = TRUE), ]

# Reset the row index
row.names(df_pop) <- NULL

################################################################################
# Save the population data to a database                                       #
# Remember to execute the query 'SET GLOBAL local_infile = 1;' as root user    #
#  on the DBMS as MySQL requires it to copy data locally                       #
################################################################################

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

# Drop the population table if it exists
db_qry <- paste("DROP TABLE IF EXISTS ", tolower(db_pop), ";", sep = "")
db_res <- dbSendQuery(db_con, db_qry)
dbClearResult(db_res)

# Create the population table
db_qry <- paste("CREATE TABLE ", tolower(db_pop), " (id INT NOT NULL AUTO_INCREMENT, icao CHAR(4) NOT NULL, iata CHAR(3) NOT NULL, traffic INT NOT NULL, name CHAR(", max(nchar(df_pop$name)),") NOT NULL, lat FLOAT NOT NULL, lon FLOAT NOT NULL, rwy CHAR(5) NOT NULL, len SMALLINT NOT NULL, toda SMALLINT NOT NULL, PRIMARY KEY (id));", sep = "")
db_res <- dbSendQuery(db_con, db_qry)
dbClearResult(db_res)

# Write the population data to the population table
# Here we use the deprecated RMySQL::MySQL() driver instead of the newer RMariaDB::MariaDB()) driver because it was found to be ~2.8 times faster here
dbWriteTable(conn = db_con, name = tolower(db_pop), value = df_pop, append = TRUE, row.names = FALSE)

# Create a composite index on the icao, traffic, lat, and lon columns (after the bulk insert above, not before for performance reasons) to speed up subsequent searches
db_idx <- "idx" # Set index name
db_qry <- paste("CREATE INDEX ", tolower(db_idx), " ON ", tolower(db_pop), " (icao, traffic, lat, lon);", sep = "")
db_res <- dbSendQuery(db_con, db_qry)
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF