###############################################################################
# /scripts/dcast.R                                                            #
# Pivot the per-airport climate files generated in netcdf.R from long to wide #
###############################################################################

# Load required libraries
library(data.table)
library(dplyr)
library(R.utils)
library(zoo)

# Clear the console
cat("\014")

# Start a timer for the script
timer_total <- Sys.time()

###############################################################################
# List climate files to be processed                                          #
###############################################################################
filepath <- "data/climate/outputs/ssp585"
filelist <- list.files(path = filepath, pattern = "\\.csv.gz$", all.files = FALSE, full.names = TRUE, recursive = TRUE)

###############################################################################
# Read and process each file sequentially                                     #
###############################################################################
for(i in 1:1) { # length(filelist)
  
  # Start a timer for the loop
  timer_loop <- Sys.time() 
  
  # Load each file as a data.table
  file <- data.table::fread(      
    file = filelist[i],
    header = TRUE,
    colClasses = c("factor", "POSIXct", "factor", "numeric"),
    col.names = c("our.icao", "nc.time", "nc.var", "nc.val"),
    data.table = TRUE,
    key = c("our.icao", "nc.var", "nc.time")
  )

  # # Convert each file from long to wide
  # file <- data.table::dcast.data.table(
  #   data = file,
  #   formula = our.icao + nc.time ~ nc.var,
  #   value.var = "nc.val"
  # )
  # 
  # # Overwrite the long file with the wide file
  # data.table::fwrite(x = file, file = filelist[i], append = FALSE, na = NA, compress = "gzip")
  
  # Display the timer
  print(paste("Processed file", i, "of", length(filelist), "in", signif(Sys.time() - timer_loop, digits = 2), "seconds.", sep = " "))
  
}

print(paste("Processed the entire script in", signif(Sys.time() - timer_total, digits = 2), "seconds.", sep = " ")) # Print the time it took to process the entire script

# EOF