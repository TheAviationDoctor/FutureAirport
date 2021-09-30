################################################################################
# scripts/4_netcdf.R                                                           #
# Extracts airport-level climatic variables from the NetCDF files              #
#  and saves them to a database for later processing                           #
#  Took ~8.25 hours to run on the researcher's config (https://bit.ly/3ChCBAP) #
#  of which ~2.5 were spent indexing the tables at the end                     #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(DBI)
library(parallel)

# Import the constants
source("scripts/0_constants.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

################################################################################
# Import the sample airports defined in sample.R                               #
################################################################################

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

# Build a query to retrieve the sample airports
db_qry <- paste("SELECT icao, lat, lon FROM", db_pop, "WHERE traffic >", pop_thr, "GROUP BY icao;", sep = " ")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the restults to a data table
dt_smp <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "icao", check.names = TRUE))

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

################################################################################
# List the climate variables contained in the NetCDF files                     #
################################################################################

# List all the NetCDF files (ending in .nc)
nc_files <- list.files(path = nc_path, pattern = "\\.nc$", full.names = TRUE)

# List all the climatic variables from the NetCDF file names
nc_vars  <- unique(lapply(strsplit(basename(nc_files), "_"), "[", 1))

################################################################################
# Function to parse the NetCDF file in parallel across multiple workers        #
# Each worker is a CPU node that gets assigned one climatic variable to parse  #
################################################################################

# Declare the function with the climatic variable as input parameter
fn_parse <- function(nc_var) {
  
  # Import the constants
  source("scripts/0_constants.R")
  
  ##############################################################################
  # Process each NetCDF file (outer loop)                                      #
  ##############################################################################
  
  # List only the NetCDF files corresponding to the climatic variable being parsed by this worker
  nc_select <- list.files(nc_path, pattern = paste("^", nc_var, "[^\\]*\\.nc$", sep = ""), full.names = TRUE)
  
  # For each NetCDF file
  for(j in 1:1) {
    
    # Output the worker's progress to the log file defined in makeCluster()
    print(paste(Sys.time(), " Worker ", Sys.getpid(), " is parsing ", nc_var, " NetCDF file ", j, "/", length(nc_select), " (", nc_select[j], ")...", sep = ""))
    
    # Pull the experiment (SSP) from the NetCDF file's name
    nc_exp <- unique(lapply(strsplit(basename(nc_select[j]), "_"), "[", 4))
    
    # Open the NetCDF file
    nc <- nc_open(nc_select[j])                                           
    
    # Parse out the NetCDF file's attributes and dimensions
    nc_atts  <- ncatt_get(nc, 0)                                                # Extract the NetCDF file's attributes
    nc_lat   <- ncvar_get(nc = nc, varid = "lat")                               # Extract the NetCDF file's 1D latitude array
    nc_lon   <- ncvar_get(nc = nc, varid = "lon") - 180                         # Extract the NetCDF file's 1D longitude array. Subtract 180 because the NetCDF longitude convention is 0°-360° but the airports' longitudes are in -180° to 180°
    nc_obs   <- nc.get.time.series(f = nc, v = nc_atts$variable_id, time.dim.name = "time") # Extract the NetCDF file's 1D time array in a clean PCICt (POSIXct-like) format
    nc_arr   <- ncvar_get(nc = nc, varid = nc_atts$variable_id)                 # Extract the 3D climate array from the NetCDF file
    
    # Release the NetCDF file from memory
    nc_close(nc)
    
    # Initialize a data table to store the climate data in the inner loop
    nc_dt <- data.table()
    
    ############################################################################
    # Extract the climatic variables for each sample airport (inner loop)      #
    ############################################################################
    
    # For each airport in the sample
    for(k in 1:2) {
      
      # Parse out the airport's spatial coordinates
      smp_lat <- dt_smp[k, lat]
      smp_lon <- dt_smp[k, lon]
      
      # Find the NetCDF file's row indices of the spatial coordinates nearest to the current airport
      lat_index <- which.min(abs(nc_lat - smp_lat))
      lon_index <- which.min(abs(nc_lon - smp_lon))
      
      # Extract the climate variable's time series at those spatial indices for the current airport
      nc_val <- nc_arr[lon_index, lat_index, ]
      
      # Append the results to the data table
      nc_apt <- data.table(
        obs  = PCICt::as.POSIXct.PCICt(nc_obs, format = "%Y-%m-%d %H:%M:%S"),   # Time series of the observations. The database won't accept POSIXct as DATETIME so we must simplify the format here
        icao = dt_smp[k, icao],                                                 # Airport ICAO code
        exp  = as.character(nc_exp),                                            # Experiment (SSP)
        val  = as.vector(nc_val)                                                # Climate variable value for each observation
      )

      # All climate variables except 'hurs' are 6-hourly mean samples at 06:00 (i.e. a mean of 03:00-09:00), 12:00 (i.e. a mean of 09:00-15:00), 18:00 (i.e. a mean of 15:00-21:00), and 00:00 (i.e. a mean of 21:00-03:00)
      # 'hurs' is instead sampled 6-hourly at a specified time point within the time period (03:00, 09:00, 15:00, 21:00)
      # For the observation times to line up with those of other variables, 'hurs' must be normalized
      # To do so, the rolling average of both time and value is computed for every row pair of 'hurs' data
      if (nc_var == "hurs") {                                      # Only execute if the climate variable is hurs
        nc_apt[, obs := obs + 3600 * 3]                            # Advance the observation time by 3 hours (which is the same as averaging the times of the current and next six-hourly observations)
        nc_apt[, val := frollmean(x = val, n = 2, align = "left")] # Average the current and next observation values
        # nc_apt[!complete.cases(val), val:= shift(val, 1L, type = "lag")]
        setnafill(nc_apt, type = "locf", cols = "val")             # The last value of 'hurs' for every NetCDF file and airport (365.25 days x 5 years x 4 daily observations = every 7,305th observation) would be empty as a result of the left-centered rolling mean, so we impute it by carrying the last observation forward ("locf")
        # na.locf
      }
      
      # Append the results to the data table initialized earlier
      nc_dt <- rbindlist(list(nc_dt, nc_apt))
      
    } # End of the inner loop

    # Save the results to a CSV file
    filepath <- "test/"
    filename <- paste(nc_var, ".csv", sep = "")
    fwrite(nc_dt, file = file.path(filepath, filename, fsep = "/"), append = TRUE, row.names = FALSE)
    
  } # End of the outer loop

} # End of the fn_parse function definition

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################

# Set the number of cores/workers to use in the cluster. Here we set as many workers as there are climatic variables in the NetCDF files
cores <- length(nc_vars)

# Set and clear the output file for cluster logging
close(file(log_net, open = "w"))

# Build the cluster of workers and select a file in which to log progress (which can't be printed to the console on the Windows version of RStudio)
cl <- makeCluster(cores, outfile = log_net)

# Have each worker load the libraries that they need to handle the fn_parse function defined above
clusterEvalQ(cl, {
  library(data.table)
  library(DBI)
  library(ncdf4)
  library(ncdf4.helpers)
  library(PCICt)
})

# Pass the required variables from the main scope to the workers' scope
clusterExport(cl, c("dt_smp", "nc_vars"))

# Distribute the parallel parsing of NetCDF files across the workers, with the climate variables as input parameter
parLapply(cl, nc_vars, fn_parse)

# Terminate the cluster once finished
stopCluster(cl)

################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF