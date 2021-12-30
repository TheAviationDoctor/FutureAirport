# ==============================================================================
#    NAME: scripts/4_import.R
#   INPUT: NetCDF files downloaded manually after 3_download.R
# ACTIONS: Import the NetCDF files into R
#          Parse airport-level climatic variables from the NetCDF files
#          Write the parsed climatic data to the database
#          Index the database table
#  OUTPUT: 2,213,847,280 rows of climate data written to the database
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load required libraries
library(DBI)
library(parallel)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Import the sample airports defined in 2_sample.R
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build the query to retrieve the sample airports
db_qry <- paste(
  "SELECT icao, lat, lon FROM", db$pop,
  "WHERE traffic >", sim$pop_thr,
  "GROUP BY icao;",
  sep = " "
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results
df_smp <- suppressWarnings(dbFetch(db_res, n = Inf))

# Release the database resource
dbClearResult(db_res)

# ==============================================================================
# 2 Set up the database table to store the results (long format)
# ==============================================================================

# Build the query to drop the table, if it exists
db_qry <- paste("DROP TABLE IF EXISTS ", tolower(db$imp), ";", sep = "")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Build the query to create the table
db_qry <- paste(
  "CREATE TABLE",
  tolower(db$imp),
  "(id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  obs DATETIME NOT NULL,
  icao CHAR(4) NOT NULL,
  exp CHAR(6) NOT NULL,
  var CHAR(4) NOT NULL,
  val FLOAT NOT NULL,
  PRIMARY KEY (id));",
  sep = " "
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# ==============================================================================
# 3 List the NetCDF files (ending in .nc) to be processed
# ==============================================================================

nc_files <- list.files(path = d$cli, pattern = "\\.nc$", full.names = TRUE)

# ==============================================================================
# 4 Function to import data from the NetCDF files to a database table
# ==============================================================================

fn_import <- function(nc_file) {

  # ============================================================================
  # 4.1 Parse the NetCDF file
  # ============================================================================

  # Inform the log file
  print(
    paste(
      Sys.time(),
      " pid ", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      " is parsing ", basename(nc_file),
      "...",
      sep = ""
    )
  )

  # Open the NetCDF file
  nc <- nc_open(nc_file)

  # Extract the NetCDF file's attributes
  nc_atts <- ncatt_get(nc, 0)

  # Extract the name of the climatic variable
  nc_var <- nc_atts$variable_id

  # Extract the experiment (SSP)
  nc_exp <- nc_atts$experiment_id

  # Extract the 1D latitude array
  nc_lat <- ncvar_get(nc = nc, varid = "lat")

  # Extract the 1D longitude array. Subtract 180 because the NetCDF longitude
  # convention is 0°-360° but the airports' longitudes are in -180° to 180°.
  nc_lon <- ncvar_get(nc = nc, varid = "lon") - 180

  # Extract the 1D time array in a clean PCICt (POSIXct-like) format
  nc_obs <- nc.get.time.series(f = nc, v = nc_var, time.dim.name = "time")

  # Extract the 3D climate array from the NetCDF file
  nc_arr <- ncvar_get(nc = nc, varid = nc_var)

  # Release the NetCDF file from memory
  nc_close(nc)

  # Define a list to hold the data in each iteration of the loop
  nc_list <- list()

  # ============================================================================
  # 4.2 Extract the climatic variables for each sample airport (inner loop)
  # ============================================================================

  # For each airport in the sample
  for (i in 1:nrow(df_smp)) {

    # Parse out the airport's latitude
    smp_lat <- df_smp[i, 2]

    # Parse out the airport's longitude
    smp_lon <- df_smp[i, 3]

    # Find the row index of the nearest latitude
    lat_index <- which.min(abs(nc_lat - smp_lat))

    # Find the row index of the nearest longitude
    lon_index <- which.min(abs(nc_lon - smp_lon))

    # Extract the climate variable's time series at those spatial indices
    nc_val <- nc_arr[lon_index, lat_index, ]

    # Assemble the results into a data table
    nc_apt <- data.table(
      # Time series of the observations. The database won't accept POSIXct as
      # DATETIME so we must simplify the format here.
      obs  = PCICt::as.POSIXct.PCICt(nc_obs, format = "%Y-%m-%d %H:%M:%S"),
      icao = df_smp[i, 1], # Airport ICAO code
      exp  = as.factor(nc_exp), # Experiment (SSP)
      var  = as.factor(nc_var), # Climatic variable name
      val  = as.vector(nc_val) # Climatic variable value
    )

    # All climate variables except 'hurs' are 6-hourly mean samples at 06:00
    # (i.e. a mean of 03:00-09:00), 12:00 (i.e. a mean of 09:00-15:00), 18:00
    # (i.e. a mean of 15:00-21:00), and 00:00 (i.e. a mean of 21:00-03:00).
    # 'hurs' is instead sampled 6-hourly at a specified time point within the
    # time period (03:00, 09:00, 15:00, 21:00). For the observation times to
    # line up with those of other variables, hurs must be normalized. To do so,
    # the rolling mean of time and value is computed for every row pair of hurs.

    # Only execute if the climate variable is hurs
    if (nc_var == "hurs") {
      # Advance the observation time by 3 hours (which is the same as averaging
      # the times of the current and next six-hourly observations).
      nc_apt[, obs := obs + 3600 * 3]
      # Average the current and next observation values
      nc_apt[, val := frollmean(x = val, n = 2, align = "left")]
      # The last value of 'hurs' for every NetCDF file and airport (365.25 days
      # x 5 years x 4 daily observations = every 7,305th observation) would be
      # empty as a result of the left-centered rolling mean, so we impute it by
      # carrying the last observation forward ("locf").
      setnafill(nc_apt, type = "locf", cols = "val")
    }

    # Append the results to the list initialized earlier
    nc_list[[i]] <- nc_apt
  } # End of the airport loop

  # ============================================================================
  # 4.3 Consolidate the outputs and write them to the database
  # ============================================================================

  # Combine the outputs of all airport iterations into a data table
  dt_nc <- rbindlist(nc_list)

  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

  # Write to the database
  dbWriteTable(
    conn = db_con,
    name = tolower(db$imp),
    value = dt_nc,
    append = TRUE,
    row.names = FALSE
  )

  # Disconnect the worker from the database
  dbDisconnect(db_con)
} # End of the fn_import function

# ==============================================================================
# 5 Handle the parallel computation across multiple cores
# ==============================================================================

# Set the number of workers to use in the cluster
cores <- 8

# Clear the log file
close(file(description = f$out, open = "w"))

# Build the cluster of workers
cl <- makeCluster(spec = cores, outfile = f$out)

# Have each worker load the libraries that they need
clusterEvalQ(cl, {
  library(data.table)
  library(DBI)
  library(ncdf4)
  library(ncdf4.helpers)
  library(PCICt)
  library(stringr)
})

# Pass the required variables from the main scope to the workers' scope
clusterExport(cl = cl, varlist = c("db", "df_smp"))

# Distribute the parallel parsing of NetCDF files across the workers
parLapply(cl = cl, X = nc_files, FUN = fn_import)

# Terminate the cluster once finished
stopCluster(cl)

# ==============================================================================
# 6 Add a composite index to the database table
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build the query to create the index
db_qry <- paste("CREATE INDEX idx ON ", tolower(db$imp), " (icao);", sep = "")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# ==============================================================================
# 7 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF
