###############################################################################
# /scripts/esgf.R                                                             #
# Import and wrangle climate data from the Earth System Grid Federation (ESGF)#
###############################################################################

# Load required libraries
library(dplyr)
library(epwshiftr)

# Clear the console and plots
cat("\014")

# Search the Lawrence Livermore National Laboratory (LLNL) node of the ESGF for available CMIP6 model outputs that meet our research requirements
nc_files <- rbind(
  epwshiftr::esgf_query( # First query
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
  epwshiftr::esgf_query( # Second query because hurs is only available at the 6hr frequency, not 6hrPt
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

# Remove duplicate tracking ids (there is a weird ESGF server-side issue with a few identical hurs files appearing twice with the same tracking_id, but with .nc and .nc_0 extensions)
nc_files <- nc_files[!rev(duplicated(rev(nc_files$tracking_id))),]

# Count number of unique datasets
length(unique(nc_files$dataset_id))

# Sum size of combined dataset in GB
sum(nc_files$file_size) / 10^9

# Count number of files
nrow(nc_files)

# Group and summarize files
nc_files %>%
  dplyr::group_by(experiment_id) %>%
  dplyr::summarize(hurs = sum(variable_id == "hurs"), ps = sum(variable_id == "ps"), tas = sum(variable_id == "tas"), sfcWind = sum(variable_id == "sfcWind"), uas = sum(variable_id == "uas"), vas = sum(variable_id == "vas"))

# The climate model outputs are then downloaded separately using wget as per https://esgf.github.io/esgf-user-support/user_guide.html#download-data-from-esgf-using-wget

# EOF