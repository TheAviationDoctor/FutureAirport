################################################################################
# /scripts/esgf_query.R                                                        #
# Query the Earth System Grid Federation (ESGF) for suitable climate models    #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(dplyr)
library(epwshiftr)

# Clear the console
cat("\014")

################################################################################
# Perform the query                                                            #
################################################################################

# Query the Lawrence Livermore National Laboratory (LLNL) node of the ESGF
nc_files <- rbind(
  esgf_query( # First query for the main climate variables of interest
    activity = "ScenarioMIP",
    variable = c("ps", "tas", "sfcWind", "uas", "vas"),
    frequency = c("6hrPt"),
    experiment = c("ssp126", "ssp245", "ssp370", "ssp585"),
    source = "MPI-ESM1-2-HR",
    variant = "r1i1p1f1",
    replica = FALSE,
    latest = TRUE,
    resolution = "100 km",
    type = "File",
    limit = 10000L,
    data_node = NULL
  ),
  esgf_query( # Separate query for 'hurs', which is only available at the 6hr frequency
    activity = "ScenarioMIP",
    variable = c("hurs"),
    frequency = c("6hr"),
    experiment = c("ssp126", "ssp245", "ssp370", "ssp585"),
    source = "MPI-ESM1-2-HR",
    variant = "r1i1p1f1",
    replica = FALSE,
    latest = TRUE,
    resolution = "100 km",
    type = "File",
    limit = 10000L,
    data_node = NULL
  )
)

################################################################################
# Parse the results                                                            #
################################################################################

# Remove duplicate tracking ids (there is a weird ESGF server-side issue with a few identical hurs files appearing twice with the same tracking_id and .nc_0 extensions)
nc_files <- nc_files[!rev(duplicated(rev(nc_files$tracking_id))),]

# Describe the query results
length(unique(nc_files$dataset_id))                                             # Count number of unique datasets
sum(nc_files$file_size) / 10^9                                                  # Sum size of combined dataset in GB
mean(nc_files$file_size)                                                        # Average size per file
nrow(nc_files)                                                                  # Count number of files

# Display the number of files by experiment (SSP) and variable
nc_files %>%
  group_by(experiment_id) %>%
  summarize(hurs = sum(variable_id == "hurs"), ps = sum(variable_id == "ps"), tas = sum(variable_id == "tas"), sfcWind = sum(variable_id == "sfcWind"), uas = sum(variable_id == "uas"), vas = sum(variable_id == "vas"))

# Save the query results to a file for later reference
filepath <- "data/climate/1_esgf_query"
filename <- "esgf.csv"
write.csv(x = nc_files, file = file.path(filepath, filename, fsep = "/"))

# The climate model outputs are then downloaded separately using wget as per https://esgf.github.io/esgf-user-support/user_guide.html#download-data-from-esgf-using-wget

# EOF