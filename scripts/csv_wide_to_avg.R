################################################################################
# /scripts/climate.R                                                           #
# Describes the climate statistics                                             #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(ggplot2)
library(R.utils)

# Clear the console
cat("\014")

################################################################################
# List csv files to be processed                                               #
################################################################################

csv_in_path  <- "data/climate/4_csv_wide"
csv_in_list  <- list.files(path = csv_in_path, pattern = "\\.csv.gz$", full.names = FALSE, recursive = TRUE)
csv_out_path <- "data/climate/5_csv_averages"
csv_starter  <- "starter.csv"
csv_out_name <- "rho.csv.gz"

################################################################################
# Load the starter file that contains the time series and experiments (SSPs)   #
################################################################################

# Load the starter file
starter <- fread(      
  file = file.path(csv_out_path, csv_starter, fsep = "/"),
  header = TRUE,
  colClasses = c("POSIXct", "factor"),
  col.names = c("nc.time", "nc.exp"),
  data.table = TRUE,
  key = c("nc.time", "nc.exp")
)

################################################################################
# Read and merge each airport file sequentially to the starter file            #
################################################################################

# Initialize the starting data.table
output <- starter

# for(i in 1:3) {
for(i in 1:length(csv_in_list)) {
  
  # Output progress to the console
  print(paste("   Processing file ", i, " of ", length(csv_in_list), "...", sep = ""))

  # Load the current file into a data.table
  input <- fread(      
    file = file.path(csv_in_path, csv_in_list[i], fsep = "/"),
    header = TRUE,
    colClasses = c("POSIXct", "factor", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"),
    col.names = c("nc.time", "nc.exp", "nc.hurs", "nc.ps", "nc.sfcWind", "nc.tas", "nc.uas", "nc.vas", "nc.rho"),
    data.table = TRUE,
    key = c("nc.time", "nc.exp")
  )

  output <- merge(x = output, y = input[, c("nc.time", "nc.exp", "nc.rho")], by = c("nc.time", "nc.exp"))
  setnames(output, "nc.rho", substr(csv_in_list[i], 1, 4))

}

# Calculate the global average air density and append it to a new column
output[, global := rowMeans(output[, !c("nc.time", "nc.exp")])]

################################################################################
# Save the output file                                                         #
################################################################################

# Write the results to a compressed CSV file
fwrite(x = output, file = file.path(csv_out_path, csv_out_name, fsep = "/"), append = FALSE, na = NA, compress = "gzip")
