################################################################################
# /scripts/dcast.R                                                             #
# Pivot the per-airport climate files generated in netcdf.R from long to wide  #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(R.utils)

# Clear the console
cat("\014")

################################################################################
# List climate files to be processed                                           #
################################################################################

filepath <- "data/climate/outputs"
filelist <- list.files(path = filepath, pattern = "\\.csv.gz$", full.names = TRUE, recursive = TRUE)

################################################################################
# Read and process each file sequentially                                      #
################################################################################
for(i in 1:length(filelist)) {
  
  # Load the current file into a data.table
  file <- data.table::fread(      
    file = filelist[i],
    header = TRUE,
    colClasses = c("POSIXct", "factor", "numeric"),
    col.names = c("nc.time", "nc.var", "nc.val"),
    data.table = TRUE,
    key = c("nc.var", "nc.time")
  )

  # Convert the current file from long to wide
  file <- data.table::dcast.data.table(
    data = file,
    formula = nc.time ~ nc.var,
    value.var = "nc.val"
  )

  # Overwrite the long file with the wide file
  data.table::fwrite(x = file, file = filelist[i], append = FALSE, na = NA, compress = "gzip")
  
}

# EOF