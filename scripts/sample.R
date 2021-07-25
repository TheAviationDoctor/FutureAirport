###############################################################################
# /scripts/sample.R                                                           #
# Builds the research sample from the population of airports and runways      #
###############################################################################

# Load required libraries
library(dplyr)
library(geosphere)
library(ggplot2)
library(kgc)
library(magrittr)
library(maps)
library(rgeos)
library(rnaturalearth)
library(scales)

# Clear the console and plots
cat("\014")

###############################################################################
# Load and describe the population                                            #
###############################################################################

# Load the population data from a CSV file
filepath <- "data/population/outputs"
filename <- "population.csv"
df_pop <- read.csv(file = file.path(filepath, filename, fsep = "/"), header = TRUE, na.strings = c(0, "NULL"), colClasses = c("factor", "factor", "integer", "character", "numeric", "numeric", "character", "integer", "integer"))

# Describe the population
str(df_pop)

# Count the observations
nrow(df_pop) # Runways
length(unique(df_pop$our.icao)) # Airports
sum(df_pop$iata.traffic[!rev(duplicated(rev(df_pop$our.icao)))]) # Traffic

###############################################################################
# Build the sample from the population                                        #
###############################################################################

# Set the airport size threshold for sampling
threshold <- 10^6

# Sample airports above the threshold
df_smp <- subset(df_pop, iata.traffic >= threshold)

# Describe the sample
str(df_smp)

# Save the sample to a CSV file
filepath <- "data/population/outputs"
filename <- "sample.csv"
write.csv(df_smp, file = file.path(filepath, filename, fsep = "/"), row.names = FALSE)

###############################################################################
# Test that the sample is representative of the population's traffic          #
###############################################################################

# Sample size in absolute terms
length(unique(df_smp$our.icao)) # Airports
nrow(df_smp) # Runways
sum(df_smp$iata.traffic[!rev(duplicated(rev(df_smp$our.icao)))]) # Traffic

# Sample size relative to the population
length(unique(df_smp$our.icao)) / length(unique(df_pop$our.icao)) * 100 # Airports
nrow(df_smp) / nrow(df_pop) * 100 # Runways
sum(df_smp$iata.traffic[!rev(duplicated(rev(df_smp$our.icao)))]) / sum(df_pop$iata.traffic[!rev(duplicated(rev(df_pop$our.icao)))]) * 100 # Traffic

###############################################################################
# Test that the sample is representative of the population's latitudes        #
###############################################################################

# Define the world object from the Natural Earth package
world <- ne_countries(scale = "small", returnclass = "sf")
# Define the Earth's five geographical zones
geo_zones <- c(-90, -66.5635, -23.4365, 23.4365, 66.5635, 90)
geo_labels <- c("Antarctic", "South temperate zone", "Tropics", "North temperate zone", "Arctic")

# Describe the population's latitude variable
length(df_pop$our.lat[!rev(duplicated(rev(df_pop$our.icao)))])
summary(df_pop$our.lat[!rev(duplicated(rev(df_pop$our.icao)))])

# Describe the sample's latitude variable
length(df_smp$our.lat[!rev(duplicated(rev(df_smp$our.icao)))])
summary(df_smp$our.lat[!rev(duplicated(rev(df_smp$our.icao)))])

# Calculate the distance from the equator
distm(c(0, median(df_pop$our.lat[!rev(duplicated(rev(df_pop$our.icao)))])), c(0, 0), fun = distHaversine) / 1000 # To the population's median
distm(c(0, mean(df_pop$our.lat[!rev(duplicated(rev(df_pop$our.icao)))])), c(0, 0), fun = distHaversine) / 1000 # To the population's mean
distm(c(0, median(df_smp$our.lat[!rev(duplicated(rev(df_smp$our.icao)))])), c(0, 0), fun = distHaversine) / 1000 # To the sample's median
distm(c(0, mean(df_smp$our.lat[!rev(duplicated(rev(df_smp$our.icao)))])), c(0, 0), fun = distHaversine) / 1000 # To the sample's mean

# Calculate the distance from the median to the mean
distm(c(0, median(df_pop$our.lat[!rev(duplicated(rev(df_pop$our.icao)))])), c(0, mean(df_pop$our.lat[!rev(duplicated(rev(df_pop$our.icao)))])), fun = distHaversine) / 1000 # Population
distm(c(0, median(df_smp$our.lat[!rev(duplicated(rev(df_smp$our.icao)))])), c(0, mean(df_smp$our.lat[!rev(duplicated(rev(df_smp$our.icao)))])), fun = distHaversine) / 1000 # Sample

# Find the population's northernmost and southermost airports
df_pop[which.max(df_pop$our.lat),c(4,5)]
df_pop[which.min(df_pop$our.lat),c(4,5)]

# Find the sample's northernmost and southermost airports
df_smp[which.max(df_smp$our.lat),c(4,5)]
df_smp[which.min(df_smp$our.lat),c(4,5)]

# Bin the population airports (not runways) by passenger traffic and geographical zones
df_pop_binned <- df_pop[!duplicated(df_pop$our.icao),] %>% # Summarize observations to just one per airport
  mutate(bin = cut(iata.traffic, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)) %>%
  mutate(geo = cut(our.lat, breaks = geo_zones, labels = geo_labels, include.lowest = TRUE, right = FALSE))

# Bin the sample airports (not runways) by passenger traffic and geographical zones
df_smp_binned <- df_smp[!duplicated(df_smp$our.icao),] %>% # Summarize observations to just one per airport
  mutate(bin = cut(iata.traffic, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)) %>%
  mutate(geo = cut(our.lat, breaks = geo_zones, labels = geo_labels, include.lowest = TRUE, right = FALSE))

# Count the population airports by geographical zone
df_pop_binned %>%
  group_by(geo) %>%
  dplyr::summarize(n = n()) %>% # Count. We have to call out the dplyr package explicitly because summarize() also exists in plyr and is being masked
  mutate(per = percent(n / nrow(df_pop_binned), accuracy = .01)) # Percentage

# Count the sample airports by geographical zone
df_smp_binned %>%
  group_by(geo) %>%
  dplyr::summarize(n = n()) %>% # Count. We have to call out the dplyr package explicitly because summarize() also exists in plyr and is being masked
  mutate(per = percent(n / nrow(df_smp_binned), accuracy = .01)) # Percentage

# Sum the population traffic by geographical zone
df_pop_binned %>%
  group_by(geo) %>%
  summarize(n = sum(iata.traffic)) %>% # Sum
  mutate(per = percent(n / sum(df_pop[!duplicated(df_pop$our.icao),]$iata.traffic), accuracy = .01)) # Percentage

# Sum the sample traffic by geographical zone
df_smp_binned %>%
  group_by(geo) %>%
  summarize(n = sum(iata.traffic)) %>% # Sum
  mutate(per = percent(n / sum(df_smp[!duplicated(df_smp$our.icao),]$iata.traffic), accuracy = .01)) # Percentage

# Build a world map and plot the airports
ggplot() +
  geom_sf(data = world, color = NA, fill = "gray", alpha = .5) + # Draw the world map
  scale_y_continuous(breaks = geo_zones) + # Add the Arctic Circle, Tropic of Cancer, Tropic of Capricorn, and Antarctic Circle to the y axis
  coord_sf(expand = FALSE) + # Show the axis labels
  geom_point(data = df_pop[!duplicated(df_pop$our.icao),], mapping = aes(x = our.lon, y = our.lat), color = "black", shape = 20, size = 1.5) + # Draw the population airport points
  geom_point(data = df_smp[!duplicated(df_smp$our.icao),], mapping = aes(x = our.lon, y = our.lat), color = "purple", shape = 20, size = 1.5) + # Draw the sample airport points
  geom_hline(aes(yintercept = df_pop$our.lat[which.max(df_pop$our.lat)]), color = "black") + # Draw the population's northernmost parallel
  geom_hline(aes(yintercept = df_smp$our.lat[which.max(df_smp$our.lat)]), color = "purple") + # Draw the sample's northernmost parallel
  geom_hline(aes(yintercept = df_pop$our.lat[which.min(df_pop$our.lat)]), color = "black") + # Draw the population's southernmost parallel
  geom_hline(aes(yintercept = df_smp$our.lat[which.min(df_smp$our.lat)]), color = "purple") + # Draw the sample's southernmost parallel
  geom_hline(aes(yintercept = mean(df_pop[!duplicated(df_pop$our.icao),]$our.lat)), color = "black") +
  geom_hline(aes(yintercept = mean(df_smp[!duplicated(df_smp$our.icao),]$our.lat)), color = "purple") +
  geom_hline(aes(yintercept = median(df_pop[!duplicated(df_pop$our.icao),]$our.lat)), color = "black") +
  geom_hline(aes(yintercept = median(df_smp[!duplicated(df_smp$our.icao),]$our.lat)), color = "purple") +
  geom_hline(aes(yintercept = 66.5635), color = "gray", linetype = "dashed") + # Draw the Arctic Circle
  geom_hline(aes(yintercept = 23.4365), color = "gray", linetype = "dashed") + # Draw the Tropic of Cancer
  geom_hline(aes(yintercept = -23.4365), color = "gray", linetype = "dashed") + # Draw the Tropic of Capricorn
  geom_hline(aes(yintercept = -66.5635), color = "gray", linetype = "dashed") + # Draw the Antarctic Circle
  theme_minimal() + # Apply a minimal theme that hides the oceans
  theme(axis.title = element_blank(), axis.text.x = element_blank()) # Hide axis titles

# Build a histogram of the airport count by latitude
ggplot() +
  geom_histogram(mapping = aes(x = df_pop[!duplicated(df_pop$our.icao),]$our.lat), fill="black", alpha=0.5, binwidth = 10, na.rm = TRUE) +
  geom_histogram(mapping = aes(x = df_smp[!duplicated(df_smp$our.icao),]$our.lat), fill="black", alpha=0.5, binwidth = 10, na.rm = TRUE) +
  scale_x_continuous(name = "Latitude", breaks = seq(-90, 90, 10), limits = c(-90, 90)) +
  scale_y_continuous(name = "Airport count") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Build a histogram of the airport traffic by latitude
ggplot() +
  geom_histogram(mapping = aes(x = df_pop[!duplicated(df_pop$our.icao),]$our.lat, weight = df_pop[!duplicated(df_pop$our.icao),]$iata.traffic), fill="black", alpha=0.5, binwidth = 10, na.rm = TRUE) +
  geom_histogram(mapping = aes(x = df_smp[!duplicated(df_smp$our.icao),]$our.lat, weight = df_smp[!duplicated(df_smp$our.icao),]$iata.traffic), fill="black", alpha=0.5, binwidth = 10, na.rm = TRUE) +
  scale_x_continuous(name = "Latitude", breaks = seq(-90, 90, 10), limits = c(-90, 90)) +
  scale_y_continuous(name = "Sum of traffic", breaks = seq(0, 10^10, 5 * 10^8), labels = label_number_si(accuracy = 0.1)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

###############################################################################
# Calculate the Köppen-Geiger climate zones for population & sample airports  #
###############################################################################

# Pick a resolution for the KGC package processing, either "fine" or "course" (yes, "coarse" is misspelled in the package's source code)
resolution <- "course"

# Prepare the population data
df_kgc_pop <- df_pop[!duplicated(df_pop$our.icao),c("our.icao", "our.lon", "our.lat", "iata.traffic")] %>% # Select unique airport locations and coordinates from the population
  mutate(rndCoord.lon = RoundCoordinates(our.lon, res = resolution, latlong = "lon")) %>% # Round longitudes to the nearest KGC grid coordinate
  mutate(rndCoord.lat = RoundCoordinates(our.lat, res = resolution, latlong = "lat")) # Round latitudes to the nearest KGC grid coordinate

# Compute the climate zone for the population data
df_kgc_pop <- data.frame(df_kgc_pop, kgc = LookupCZ(df_kgc_pop, res = resolution, rc = FALSE))

# Summarize the climate distribution for the population data
df_kgc_pop <- df_kgc_pop %>%
  group_by(kgc) %>%
  dplyr::summarize(pop.airports = n(), pop.traffic = sum(iata.traffic)) # Count. We have to call out the dplyr package explicitly because summarize() also exists in plyr and is being masked

# Prepare the sample data
df_kgc_smp <- df_smp[!duplicated(df_smp$our.icao),c("our.icao", "our.lon", "our.lat", "iata.traffic")] %>% # Select unique airport locations and coordinates from the sample
  mutate(rndCoord.lon = RoundCoordinates(our.lon, res = resolution, latlong = "lon")) %>% # Round longitudes to the nearest KGC grid coordinate
  mutate(rndCoord.lat = RoundCoordinates(our.lat, res = resolution, latlong = "lat")) # Round latitudes to the nearest KGC grid coordinate

# Compute the climate zone for the sample data
df_kgc_smp <- data.frame(df_kgc_smp, kgc = LookupCZ(df_kgc_smp, res = resolution, rc = FALSE))

# Summarize the climate distribution for the sample data
df_kgc_smp <- df_kgc_smp %>%
  group_by(kgc) %>%
  dplyr::summarize(smp.airports = n(), smp.traffic = sum(iata.traffic)) # Count. We have to call out the dplyr package explicitly because summarize() also exists in plyr and is being masked

# Merge the population and sample counts for row-wise comparison
df_kgc <- merge(df_kgc_pop, df_kgc_smp, by = "kgc", all = TRUE)

# Recode NAs with 0
df_kgc[is.na(df_kgc)] <- 0

# Recode missing climate zones with Z
df_kgc$kgc <- as.character(df_kgc$kgc) # De-factorize
df_kgc$kgc[df_kgc$kgc == "Climate Zone info missing"] <- "Z"
df_kgc$kgc <- as.factor(df_kgc$kgc) # Re-factorize

# View the summarized table of main climate groups
df_kgc %>%
  group_by(group = substr(kgc, 1, 1)) %>%
  dplyr::summarize(pop.airports = sum(pop.airports), pop.airports.per = percent(sum(pop.airports) / sum(df_kgc$pop.airports), accuracy = .01), pop.traffic = sum(pop.traffic), pop.traffic.per = percent(sum(pop.traffic) / sum(df_kgc$pop.traffic), accuracy = .01), smp.airports = sum(smp.airports), smp.airports.per = percent(sum(smp.airports) / sum(df_kgc$smp.airports), accuracy = .01), smp.traffic = sum(smp.traffic), smp.traffic.per = percent(sum(smp.traffic) / sum(df_kgc$smp.traffic), accuracy = .01)) %>% # Count. We have to call out the dplyr package explicitly because summarize() also exists in plyr and is being masked
  print()

# Plot the airport distribution across climate zones
ggplot(data = df_kgc) +
  geom_bar(mapping = aes(x = kgc, weight = pop.airports), fill="black", alpha=0.5, width = 1) +
  geom_bar(mapping = aes(x = kgc, weight = smp.airports), fill="black", alpha=0.5, width = 1) +
  scale_y_continuous(trans = "log1p", breaks = c(2^(0:8))) + # Pseudo-log scale for legibility
  labs(x = "Köppen-Geiger climate zones", y = "Airport count") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Plot the traffic distribution across climate zones
ggplot(data = df_kgc) +
  geom_bar(mapping = aes(x = kgc, weight = pop.traffic), fill="black", alpha=0.5, width = 1) +
  geom_bar(mapping = aes(x = kgc, weight = smp.traffic), fill="black", alpha=0.5, width = 1) +
  scale_y_continuous(breaks = seq(from = 0, to = 10^10, by = 5 * 10^8), labels = label_number_si(accuracy = 0.1)) +
  labs(x = "Köppen-Geiger climate zones", y = "Sum of traffic") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# EOF