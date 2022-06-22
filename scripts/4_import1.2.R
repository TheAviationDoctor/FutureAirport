# ==============================================================================
#    NAME: scripts/4_import.R
#   INPUT: NetCDF files downloaded manually after 3_download.R
# ACTIONS: Import the NetCDF files into R
#          Parse airport-level climatic variables from the NetCDF files
#          Write the parsed climate data to the database
#          Index the database table
#  OUTPUT: 2,213,847,280 rows of climate data written to the database
# RUNTIME: ~7.8 hours (3.8 GHz CPU / 128 GB RAM)
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2022
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load required libraries
library(data.table)
library(DBI)
library(parallel)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Set up the database table to store the script outputs
# ==============================================================================

# # Drop the table if it exists
# fn_sql_qry(
#   statement = paste("DROP TABLE IF EXISTS ", tolower(dat$imp), ";", sep = "")
# )
# 
# # Create the table
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE", tolower(dat$imp),
#     "(id INT UNSIGNED NOT NULL AUTO_INCREMENT,
#     obs DATETIME NOT NULL,
#     icao CHAR(4) NOT NULL,
#     zone CHAR(10) NOT NULL,
#     exp CHAR(6) NOT NULL,
#     var CHAR(4) NOT NULL,
#     val FLOAT NOT NULL,
#     PRIMARY KEY (id));",
#     sep = " "
#   )
# )
# 
# # ==============================================================================
# # 2 Fetch the data that we need
# # ==============================================================================
# 
# # Fetch the list of sample airports
# dt_smp <- fn_sql_qry(
#   statement = paste(
#     "SELECT icao, lat, lon, zone FROM", dat$pop,
#     "WHERE traffic >", sim$pop_thr,
#     "GROUP BY icao;",
#     sep = " "
#   )
# )
# 
# # List the NetCDF files from which to extract the airport climatic conditions
# nc_files <- list.files(path = dir$cli, pattern = "\\.nc$", full.names = TRUE)
# 
# # ==============================================================================
# # 3 Parse the NetCDF files
# # ==============================================================================
# 
# fn_import <- function(nc_file) {
# 
#   # ============================================================================
#   # 3.1 Parse the current NetCDF file
#   # ============================================================================
# 
#   # Offset the start of each worker by a random duration to spread disk I/O load
#   Sys.sleep(time = sample(1:120, 1L))
# 
#   # Inform the log file
#   print(
#     paste(
#       Sys.time(),
#       " pid ", stringr::str_pad(
#         Sys.getpid(),
#         width = 5L,
#         side  = "left",
#         pad   = " "
#       ),
#       " is parsing ", basename(nc_file),
#       "...",
#       sep = ""
#     )
#   )
# 
#   # Open the NetCDF file
#   nc <- ncdf4::nc_open(
#     filename  = nc_file,
#     write     = FALSE,
#     readunlim = FALSE
#   )
# 
#   # Read the NetCDF file's attributes
#   nc_att <- ncdf4::ncatt_get(nc = nc, varid = 0L)
# 
#   # Read the name of the file's climatic variable
#   nc_var <- nc_att$variable_id
# 
#   # Read the file's experiment variable (SSP)
#   nc_exp <- nc_att$experiment_id
# 
#   # Read the 1D latitude array
#   nc_lat <- ncdf4::ncvar_get(nc = nc, varid = "lat")
# 
#   # Read the 1D longitude array. Subtract 180 because the NetCDF longitude
#   # convention is 0°-360° but the airports' longitudes are from -180° to 180°.
#   nc_lon <- ncdf4::ncvar_get(nc = nc, varid = "lon") - 180L
# 
#   # Read the 1D time array in PCICt (POSIXct-like) format
#   nc_obs <- ncdf4.helpers::nc.get.time.series(
#     f = nc,
#     v = nc_var,
#     time.dim.name = "time"
#   )
# 
#   # Read the 3D climate array
#   nc_arr <- ncdf4::ncvar_get(nc = nc, varid = nc_var)
# 
#   # Release the NetCDF file from memory
#   ncdf4::nc_close(nc = nc)
# 
#   # Define a list to hold the data in each iteration of the loop
#   nc_lst <- list()
# 
#   # ============================================================================
#   # 3.2 Extract the climatic variables for each sample airport (inner loop)
#   # ============================================================================
# 
#   # For each airport in the sample
#   for (i in 1:nrow(dt_smp)) {
# 
#     # Find the row index of the latitude nearest to the airport's
#     lat_idx <- which.min(abs(nc_lat - dt_smp[i, lat]))
# 
#     # Find the row index of the longitude nearest to the airport's
#     lon_idx <- which.min(abs(nc_lon - dt_smp[i, lon]))
# 
#     # Extract the climate variable's time series at those spatial indices
#     nc_val <- nc_arr[lon_idx, lat_idx, ]
# 
#     # Assemble the results into a data table
#     dt_apt <- data.table(
#       obs  = PCICt::as.POSIXct.PCICt(nc_obs, format = "%Y-%m-%d %H:%M:%S"),
#       icao = as.factor(dt_smp[i, icao]), # Airport's ICAO code
#       zone = as.factor(dt_smp[i, zone]), # Airport's climate zone
#       exp  = as.factor(nc_exp),          # Experiment (SSP)
#       var  = as.factor(nc_var),          # Climatic variable name
#       val  = as.vector(nc_val)           # Climatic variable value
#     )
#     
#     # All climate variables except 'hurs' are 6-hourly mean samples at 06:00
#     # (i.e. a mean of 03:00-09:00), 12:00 (i.e. a mean of 09:00-15:00), 18:00
#     # (i.e. a mean of 15:00-21:00), and 00:00 (i.e. a mean of 21:00-03:00).
#     # 'hurs' is instead sampled 6-hourly at a specified time point within the
#     # time period (03:00, 09:00, 15:00, 21:00). For the observation times to
#     # line up with those of other variables, hurs must be normalized. To do so,
#     # the rolling mean of time and value is computed for every row pair of hurs.
#     if (nc_var == "hurs") {
#       # Advance the observation time by 3 hours (which is the same as averaging
#       # the times of the current and next six-hourly observations).
#       set(x = dt_apt, j = "obs", value = dt_apt[, obs] + 3600L * 3L)
#       # Average the current and next observation values
#       set(
#         x = dt_apt,
#         j = "val",
#         value = frollmean(x = dt_apt[, val], n = 2L, align = "left")
#       )
#       # The last value of 'hurs' for every NetCDF file and airport (365.25 days
#       # x 5 years x 4 daily observations = every 7,305th observation) would be
#       # empty as a result of the left-centered rolling mean, so we impute it by
#       # carrying the last observation forward ("locf").
#       setnafill(x = dt_apt, type = "locf", cols = "val")
#     }
# 
#     # Append the results to the list initialized earlier
#     nc_lst[[i]] <- dt_apt
# 
#   } # End of the airport loop
# 
#   # ============================================================================
#   # 3.3 Consolidate the outputs and write them to the database
#   # ============================================================================
# 
#   # Combine the outputs of all airport iterations into a data table
#   dt_nc <- rbindlist(nc_lst)
# 
#   # Connect the worker to the database
#   conn <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)
# 
#   # Write to the database
#   dbWriteTable(
#     conn      = conn,
#     name      = tolower(dat$imp),
#     value     = dt_nc,
#     append    = TRUE,
#     row.names = FALSE
#   )
# 
#   # Disconnect the worker from the database
#   dbDisconnect(conn)
# 
# } # End of the fn_import function
# 
# # ==============================================================================
# # 4 Handle the parallel computation across multiple cores
# # ==============================================================================
# 
# # Distribute the work across the cluster
# fn_par_lapply(
#   crs = 16L,
#   pkg = c("data.table", "DBI", "ncdf4", "ncdf4.helpers", "PCICt", "stringr"),
#   lst = nc_files,
#   fun = fn_import
# )

# ==============================================================================
# 5 Add a composite index to the database table
# ==============================================================================

fn_sql_qry(
  statement = paste(
    "DELETE FROM", tolower(dat$imp),
    "WHERE obs = '2101-01-01 00:00:00';",
    sep = " ")
)

# fn_sql_qry(
#   statement = paste(
#     "CREATE INDEX idx ON", tolower(dat$imp), "(icao);", sep = " "
#   )
# )

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF
