# ==============================================================================
#    NAME: scripts/3_download.R
#   INPUT: Search criteria for the climate models, defined within the script
# ACTIONS: Query the Earth System Grid Federation (ESGF) for fitting NetCDF data
#          The NetCDF files are then downloaded manually using wget as per
#          https://esgf.github.io/esgf-user-support/user_guide.html
#  OUTPUT: CSV file containing the list of NetCDF files that meet the criteria
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2022
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load required libraries
library(dplyr)
library(epwshiftr)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Perform the query
# ==============================================================================

# Query the ESGF server
nc_files <- rbind(
  # First query for the main climate variables of interest
  esgf_query(
    activity   = "ScenarioMIP",
    variable   = c("ps", "tas", "uas", "vas"),
    frequency  = c("6hrPt"),
    experiment = c("ssp126", "ssp245", "ssp370", "ssp585"),
    source     = "MPI-ESM1-2-HR",
    variant    = "r1i1p1f1",
    replica    = FALSE,
    latest     = TRUE,
    resolution = "100 km",
    type       = "File",
    limit      = 10000L,
    data_node  = NULL
  ),
  # Separate query for 'hurs', which is only available at the 6hr frequency
  esgf_query(
    activity   = "ScenarioMIP",
    variable   = c("hurs"),
    frequency  = c("6hr"),
    experiment = c("ssp126", "ssp245", "ssp370", "ssp585"),
    source     = "MPI-ESM1-2-HR",
    variant    = "r1i1p1f1",
    replica    = FALSE,
    latest     = TRUE,
    resolution = "100 km",
    type       = "File",
    limit      = 10000L,
    data_node  = NULL
  )
)

# ==============================================================================
# 2 Parse the results
# ==============================================================================

# Remove duplicate tracking ids (there is a weird ESGF server-side issue with a
# few identical hurs files appearing twice with the same tracking_id).
nc_files <- nc_files[!rev(duplicated(rev(nc_files$tracking_id))), ]

# Count number of unique datasets
length(unique(nc_files$dataset_id))

# Sum size of combined dataset in GB
sum(nc_files$file_size) / 10^9

# Average size per file
mean(nc_files$file_size)

# Count number of files
nrow(nc_files)

# Display the number of files by experiment (SSP) and variable
nc_files %>%
  group_by(experiment_id) %>%
  summarize(
    hurs = sum(variable_id == "hurs"),
    ps   = sum(variable_id == "ps"),
    tas  = sum(variable_id == "tas"),
    uas  = sum(variable_id == "uas"),
    vas  = sum(variable_id == "vas")
  )

# Save the query results to a file for later reference
write.csv(x = nc_files, file = "data/netcdf.csv")

# ==============================================================================
# 3 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF
