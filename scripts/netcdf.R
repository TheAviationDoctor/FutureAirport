###############################################################################
# /scripts/netcdf.R                                                           #
# Import and wrangle the NetCDF files downloaded from esgf.R                  #
###############################################################################

# Load required libraries
library(data.table)
library(dplyr)
library(magrittr)
library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
library(stringr)
library(tidync)
library(tidyr)

# Clear the console
cat("\014")

# Start a timer for the whole script
timer_total <- Sys.time()

###############################################################################
# Import the sample of runways defined in sample.R                            #
###############################################################################

# Read the CSV file of runways in the sample
filepath <- "data/population/outputs"
filename <- "sample.csv"
df_smp <- read.csv(file = file.path(filepath, filename, fsep = "/"), header = TRUE, colClasses = c("factor", "factor", "integer", "character", "numeric", "numeric", "factor", "integer", "integer"))

# Select unique airports (same ICAO code, same latitude, and same longitude)
df_apt <- dplyr::distinct(df_smp, our.icao, our.lat, our.lon)

###############################################################################
# List all NetCDF files previously downloaded from esgf.R                     #
###############################################################################

# List the NetCDF files and parse out their characteristics from their file name
nc_path <- "data/climate/inputs" # Set the relative path to the NetCDF files
nc_files <- data.frame(filename = list.files(path = nc_path, pattern = "\\.nc$")) %>%
  tidyr::separate(col = filename, sep = "_", into = c("variable", "table", "source", "experiment", "member", "grid", "timerange"), remove = FALSE) %>% # Split the file name into its constituents as per the file naming convention here: https://docs.google.com/document/d/1h0r8RZr_f3-8egBMMh7aqLwy3snpD6_MrDz1q8n5XUk/
  tidyr::separate(col = timerange, sep = "-", into = c("start", "end")) %>% # Further split the time range into start and end dates
  dplyr::mutate(start = stringr::str_sub(string = start, start = 1L, end = 8L)) %>% # Subset the start date string
  dplyr::mutate(end = stringr::str_sub(string = end, start = 1L, end = 8L)) # Subset the end date string

###############################################################################
# Process each NetCDF in sequence                                             #
###############################################################################

# Outer loop: for each NetCDF file
for(i in 1:nrow(nc_files)) { # Replace with 'for(i in 1:nrow(nc_files)) {' once ready to roll-out the code to the whole list of .nc files
  timer_outer <- Sys.time() # Start a timer for the outer loop
  nc <- ncdf4::nc_open(file.path(nc_path, nc_files[i, "filename"])) # Open the NetCDF file
  nc_atts  <- ncdf4::ncatt_get(nc, 0) # Extract the NetCDF file's attributes
  nc_lat   <- ncdf4::ncvar_get(nc = nc, varid = "lat") # Extract the NetCDF file's 1D latitude array
  nc_lon   <- ncdf4::ncvar_get(nc = nc, varid = "lon") - 180 # Extract the NetCDF file's 1D longitude array. Subtract 180 because the NetCDF longitude convention is 0°-360° but the airports' longitudes are in -180° to 180°
  nc_var   <- ncdf4::ncvar_get(nc = nc, varid = ncdf4::ncatt_get(nc, 0)$variable_id) # Extract the 3D climate array from the NetCDF file. Needed only for method 2 explained further below
  nc_time  <- ncdf4.helpers::nc.get.time.series(f = nc, v = nc_atts$variable_id, time.dim.name = "time") # Extract the NetCDF file's 1D time array in a clean PCICt (POSIXct-like) format
  # Output the NetCDF file's salient attributes to the console
  print("------------------------------------------------------------------------------------------")
  print(paste("Processing NetCDF file ", i, " of ", nrow(nc_files), "...", sep = ""))
  print(paste("Name            ", nc_files[i, "filename"], sep = ""))
  print(paste("Institution     ", nc_atts$institution, sep = ""))
  print(paste("Model           ", nc_atts$title, sep = ""))
  print(paste("Activity        ", nc_atts$activity_id, sep = ""))
  print(paste("Experiment      ", nc_atts$experiment_id, " (", nc_atts$experiment, ")", sep = ""))
  print(paste("Member          ", nc_atts$variant_label, sep = ""))
  print(paste("Dimensions      ", stringr::str_c(attributes(nc$dim)$names, collapse = ", "), sep = ""))
  print(paste("Time range      From ", min(nc_time), " to ", max(nc_time), sep = ""))
  print(paste("Time units      ", ncdf4::ncatt_get(nc, attributes(nc$dim)$names[1])$units, " (", ncdf4::ncatt_get(nc, attributes(nc$dim)$names[1])$calendar, ")", sep = "")) # Gregorian is 365.2425 days in a year
  print(paste("Grid            ", nc_atts$grid_label, sep = ""))
  print(paste("Resolution      ", nc_atts$nominal_resolution, sep = ""))
  print(paste("Variable        ", nc_atts$variable_id, " (", ncdf4::ncatt_get(nc, attributes(nc$var)$names[grep(nc_atts$variable_id, attributes(nc$var)$names)])$long_name, " in ", ncdf4::ncatt_get(nc, attributes(nc$var)$names[grep(nc_atts$variable_id, attributes(nc$var)$names)])$units, ")", sep = "")) # Rather than referencing the list item by position (e.g. 5), we look for the variable_id (which is a global attribute of the NetCDF file) in the attributes(nc$var)$names character vector, in order to dynamically return the position of the variable in the vector
  print("------------------------------------------------------------------------------------------")

  # Inner loop: for each airport in the sample
  for(j in 1:nrow(df_apt)) { # Replace with 'for(j in 1:nrow(df_apt)) {' once ready to roll-out the code to the whole list of airports
    timer_inner <- Sys.time()                     # Start a timer for the inner loop
    apt_icao <- df_apt[j, "our.icao"]             # Extract the current airport's ICAO code
    apt_lat  <- df_apt[j, "our.lat"]              # Extract the current airport's latitude
    apt_lon  <- df_apt[j, "our.lon"]              # Extract the current airport's longitude
    lat_index <- which.min(abs(nc_lat - apt_lat)) # Find the latitude index nearest to the current airport
    lon_index <- which.min(abs(nc_lon - apt_lon)) # Find the longitude index nearest to the current airport
    # This is where the magic happens, i.e. the climate data are extracted for the current airport.
    # Two methods were identified to subset the 3D climate array for just the time series nearest the spatial coordinates of the current airport.
    # Method 1 uses the 'nc.get.var.subset.by.axes' function from the 'ncdf4.helpers' package, and was clocked at ~7 seconds for one airport.
    # Method 2 first saves the whole 3D climate array for the NetCDF file's veriable (hurs, tas, etc.) to an R variable (nc_var) in the outer loop (which adds a tiny bit of overhead there), but then references the array's lon/lat indices directly and very efficiently. It was clocked at 0.0005 second for one airport.
    # Method 2 was retained since it is vastly more efficient. Method 1 is still provided below (in commented form) for reference.
    # nc_method1 <- ncdf4.helpers::nc.get.var.subset.by.axes(f = nc, v = nc_atts$variable_id, axis.indices = list(X = lon_index, Y = lat_index))[,1,] # Method 1
    nc_method2 <- nc_var[lon_index, lat_index, ] # Method 2
    # Assemble the results into a data table
    nc_out <- data.table::data.table(
      our.icao = apt_icao,
#      nc.exp = nc_atts$experiment_id,
      nc.time = PCICt::as.POSIXct.PCICt(nc_time),
      nc.var = nc_atts$variable_id,
      nc.val = as.vector(nc_method2)
    )
    
    # Write the data table to a CSV file
    filepath <- paste("data/climate/outputs", nc_atts$experiment_id, sep = "/") # Temporarily save to an external drive
    # filename <- paste(nc_atts$experiment_id, nc_atts$variable_id, "csv.gz", sep = ".") # Write to a different file for each experiment (SSP) (important for dcast.R)
    # filename <- "climate.csv.gz" # Write to a single file (important for dcast.R)
    filename <- paste(apt_icao, "csv.gz", sep = ".") # Set up one file for each airport-experiment pair
    data.table::fwrite(x = nc_out, file = file.path(filepath, filename, fsep = "/"), append = TRUE, na = NA, compress = "gzip") # Write the data to the file using the 'fwrite' function from the 'data.table' package because of its speed but also ability to compress. The .gzip format was found here to reduce file size by ~80% relative to an uncompressed .csv
    # Print the time it took to process the inner loop
    print(paste("   Processed ", nc_atts$variable_id," data for airport ", j, " of ", nrow(df_apt), " (", df_apt[j, "our.icao"], ") in ", signif(Sys.time() - timer_inner, digits = 2), " seconds.", sep = ""))
  } # End of inner loop

  # Print the time it took to process the outer loop
  print(paste("Finished processing NetCDF file ", i, " of ", nrow(nc_files), " in ", signif(Sys.time() - timer_outer, digits = 2), " seconds.", sep = ""))
  # Release the NetCDF file from memory
  ncdf4::nc_close(nc)

} # End of outer loop

print(paste("Processed the entire script in", signif(Sys.time() - timer_total, digits = 2), "seconds.", sep = " ")) # Print the time it took to process the entire script

# EOF