################################################################################
# /scripts/netcdf_to_csv.R                                                     #
# Wrangle the NetCDF files downloaded from esgf.R into CSV files per airport   #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
library(tidync)

# Clear the console
cat("\014")

################################################################################
# Import the sample airports defined in sample.R so we can loop through them   #
################################################################################

smp_path <- "data/population/outputs"                                           # Set the sample's file path
smp_file <- "sample.csv"                                                        # Set the sample's file name
dt_smp <- data.table::fread(                                                    # Load the sample into a data table
  file = file.path(smp_path, smp_file, fsep = "/"),
  header = TRUE,
  colClasses = c("factor", "factor", "integer", "character", "numeric", "numeric", "factor", "integer", "integer"))
dt_smp <- unique(x = dt_smp, by = "our.icao")                                   # Select unique airports from the sample

################################################################################
# List all NetCDF files previously downloaded from esgf.R                      #
################################################################################

nc_path  <- "data/climate/2_netcdf_downloads"                                               # Set the file path
nc_files <- list.files(path = nc_path, pattern = "\\.nc$", full.names = TRUE)   # List the files

################################################################################
# Process each NetCDF file in the list (outer loop)                            #
################################################################################

for(i in 1:length(nc_files)) {

  print(paste("Processing NetCDF file ", i, " of ", length(nc_files), "...", sep = "")) # Output progress to the console
  
  # Parse out the NetCDF file's attributes and dimensions
  nc       <- ncdf4::nc_open(nc_files[i])                                       # Open the NetCDF file
  nc_atts  <- ncdf4::ncatt_get(nc, 0)                                           # Extract the NetCDF file's attributes
  nc_lat   <- ncdf4::ncvar_get(nc = nc, varid = "lat")                          # Extract the NetCDF file's 1D latitude array
  nc_lon   <- ncdf4::ncvar_get(nc = nc, varid = "lon") - 180                    # Extract the NetCDF file's 1D longitude array. Subtract 180 because the NetCDF longitude convention is 0°-360° but the airports' longitudes are in -180° to 180°
  nc_time  <- ncdf4.helpers::nc.get.time.series(f = nc, v = nc_atts$variable_id, time.dim.name = "time") # Extract the NetCDF file's 1D time array in a clean PCICt (POSIXct-like) format
  nc_var   <- ncdf4::ncvar_get(nc = nc, varid = nc_atts$variable_id) # Extract the 3D climate array from the NetCDF file. Needed only for method 2 explained further below
  
  ##############################################################################
  # Process each airport in the sample (inner loop)                            #
  ##############################################################################
  
  for(j in 1:nrow(dt_smp)) {

    # Output progress to the console
    print(paste("   Processing airport ", j, " (", dt_smp[j, our.icao], ") of ", nrow(dt_smp), "...", sep = ""))

    # Parse out the airport's spatial coordinates
    smp_lat <- dt_smp[j, our.lat]
    smp_lon <- dt_smp[j, our.lon]

    # Find the NetCDF file's row indices of the spatial coordinates nearest to the current airport
    lat_index <- which.min(abs(nc_lat - smp_lat))
    lon_index <- which.min(abs(nc_lon - smp_lon))

    # Extract the climate variable's time series at those spatial indices for the current airport
    nc_val <- nc_var[lon_index, lat_index, ]

    # Assemble the results into a data table
    nc_out <- data.table::data.table(
      nc.time = PCICt::as.POSIXct.PCICt(nc_time),                               # Time series
      nc.exp = nc_atts$experiment_id,                                           # Experiment (shared socio-economic pathway)
      nc.var = nc_atts$variable_id,                                             # Climate variable name (repeated down, i.e. long format)
      nc.val = as.vector(nc_val)                                                # Climate variable values
    )

    # All climate variables except 'hurs' are 6-hourly mean samples at 06:00 (i.e. a mean of 03:00-09:00), 12:00 (i.e. a mean of 09:00-15:00), 18:00 (i.e. a mean of 15:00-21:00), and 00:00 (i.e. a mean of 21:00-03:00)
    # 'hurs' is instead sampled 6-hourly at a specified time point within the time period (03:00, 09:00, 15:00, 21:00)
    # For the observation times to line up with those of other variables, 'hurs' must be normalized
    # To do so, the rolling average of both time and value is computed for every row pair of 'hurs' data
    if (nc_atts$variable_id == "hurs") {                                        # Only execute if the climate variable is hurs
      nc_out[, nc.time := nc.time - 3600 * 3]                                   # Roll back the time by 3 hours (which is the same as averaging the times of the current and previous six-hourly observations)
      nc_out[, nc.val := data.table::frollmean(x = nc.val, n = 2)]              # Average the current and previous observation values
      nc_out <- na.omit(object = nc_out, cols = "nc.val")                       # The first value of 'hurs' would be empty since it does not have a previous observation, so we remove it
    }

    # Write the results to a compressed CSV file
    out_path <- "data/climate/3_csv_long"                                       # Set the file path
    out_file <- paste(dt_smp[j, our.icao], "csv.gz", sep = ".")                 # Set up one file for each airport-experiment pair
    data.table::fwrite(x = nc_out, file = file.path(out_path, out_file, fsep = "/"), append = TRUE, na = NA, compress = "gzip") # Write the data to the file using the 'fwrite' function from the 'data.table' package because of its speed but also ability to compress. The .gzip format was found here to reduce file size by ~80% relative to an uncompressed .csv

  } # End of inner loop

  # Release the NetCDF file from memory
  ncdf4::nc_close(nc)

} # End of outer loop

# EOF