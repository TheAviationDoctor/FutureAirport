################################################################################
# scripts/7_headwind.R                                                         #
# Calculates headwind speed and identifies active runway for each observation  #
#  Took ~X hours to run on the researchers' config (https://bit.ly/3ChCBAP)    #
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

# Build the query to retrieve airports above the passenger traffic threshold
db_qry <- paste("SELECT icao, rwy, toda FROM", db_pop, "WHERE traffic >", pop_thr, "ORDER BY icao;", sep = " ")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a data table
dt_smp <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = c("icao", "rwy"), check.names = FALSE))

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# Replace the runway name (e.g., RW26R) with the magnetic heading in degrees (e.g., 260) for later headwind calculation
dt_smp[, rwy := as.numeric(substr(rwy, 3, 4)) * 10]

# For two runways with the same magnetic heading at a given airport (e.g., RWY26R and RWY26L), keep only the one with the longest TODA (i.e., the most favorable case)
dt_smp <- dt_smp[, .SD[which.max(toda)], by = .(icao, rwy)]

# Count how many unique runway headings remain in the sample before we choose which ones have the strongest headwind for each airport
nrow(dt_smp)

################################################################################
# Define a function to return wind direction, headwind speed, and active runway#
# Each worker is a CPU node that gets assigned one climate experiment (SSP)    #
################################################################################

# Declare the function with the experiment (SSP) as input parameter
fn_wnd <- function(nc_exp, i) {

    # Import the constants
  source("scripts/0_constants.R")
  
  ##############################################################################
  # Connect this worker to the database                                        #
  ##############################################################################

  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

  ##############################################################################
  # Retrieve the eastward and northward wind speeds from the database          #
  ##############################################################################

  # Set the names of the tables where the eastward and northward wind data are stored (l[[1]] is the item of the list l that contains the experiment names)
  db_uas <- paste("uas_", tolower(nc_exp), sep = "")
  db_vas <- paste("vas_", tolower(nc_exp), sep = "")

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste("[1/4] ", Sys.time(), " Worker ", Sys.getpid(), " is reading wind data from tables ", db_uas, " and ", db_vas, "...", sep = ""))

  # Count the rows in the eastward wind table
  db_qry <- paste("SELECT COUNT(*) FROM", db_uas, ";", sep = " ")

  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)

  # Return the results to a variable
  df_cnt <- suppressWarnings(as.integer(dbFetch(db_res, n = Inf)))

  # Release the database resource
  dbClearResult(db_res)
  
  # Build the query to retrieve the wind data for every airport/observation pair. We use LIMIT and OFFSET combined with the split variable defined further down to increase parallelization
  db_qry <- paste("SELECT uas.obs, uas.icao, uas.val AS uas, vas.val AS vas FROM", db_uas, "AS uas,", db_vas, "AS vas WHERE uas.id = vas.id ORDER BY uas.icao, uas.obs LIMIT", ceiling(df_cnt / split) - i, "OFFSET", ceiling(df_cnt * i / split) + i, ";", sep = " ")

  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)

  # Return the results to a data table
  dt_wnd <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = c("icao", "obs"), check.names = FALSE))

  # Release the database resource
  dbClearResult(db_res)

  # Count how many observations exist in the combined wind table
  nrow(dt_wnd)

  ##############################################################################
  # Define a function to return the headwind speed for a given runway          #
  ##############################################################################

  fn_headwind <- function(rwy) {

    # Calculate the angle (in degrees) between the runway heading and the direction that the wind is coming from
    alpha <- abs(rwy - wnd_dir)

    # Calculate and return the headwind speed (in m/s)
    wnd_spd * cos(alpha * pi / 180)

  }

  ##############################################################################
  # Calculate the headwind for each runway at each observation                 #
  ##############################################################################

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste("[2/4] ", Sys.time(), " Worker ", Sys.getpid(), " is calculating the headwind speed and active runway for each observation in ", nc_exp, "...", sep = ""))

  # For each observation
  for (i in 1:nrow(dt_wnd)) {

    # Output the worker's progress to the log file defined in makeCluster() at every arbitrarily-sized batch of observations processed
    if(i %% 10^6 == 0) { print(paste("Processing observation ", format(i, big.mark = ","), " of ", format(nrow(dt_wnd), big.mark = ","), "...", sep = "")) }

    # Calculate the wind speed at this observation's airport
    wnd_spd <- sqrt(dt_wnd[i, uas]^2 + dt_wnd[i, vas]^2)

    # Calculate the direction that the wind is coming from at this observation's airport
    wnd_dir <- (180 + (180 / pi) * atan2(dt_wnd[i, uas], dt_wnd[i, vas])) %% 360

    # Extract the list of unique runways for this observation's airport
    rwys <- dt_smp[icao == dt_wnd[i, icao], rwy]

    # Calculate the headwind speed for each runway at this observation's airport
    l <- lapply(rwys, fn_headwind)

    # Return the maximum headwind speed across all runways at this observation's airport
    val <- max(unlist(l))

    # Return the runway that has the maximum headwind speed (presumed to be the active runway at the time of the observation)
    rwy <- rwys[which.max(unlist(l))]

    # Add the headwind and active runway to the data table
    dt_wnd[i, val := val]
    dt_wnd[i, rwy := rwy]

  } # End of loop

  ##############################################################################
  # Write headwind speed and active runway to the database                     #
  ##############################################################################

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste("[3/4] ", Sys.time(), " Worker ", Sys.getpid(), " is writing the headwind speed and active runway to table ", tolower(db_wnd), "_", tolower(nc_exp), "...", sep = ""))

  # Drop the table corresponding to the headwind variable and current experiment (SSP), if it exists
  db_qry <- paste("DROP TABLE IF EXISTS ", tolower(db_wnd), "_", tolower(nc_exp), ";", sep = "")
  db_res <- dbSendQuery(db_con, db_qry)
  dbClearResult(db_res)

  # Create the table corresponding to the headwind variable and current experiment (SSP)
  db_qry <- paste("CREATE TABLE ", tolower(db_wnd), "_", tolower(nc_exp), " (id INT NOT NULL AUTO_INCREMENT, obs DATETIME NOT NULL, icao CHAR(4) NOT NULL, rwy CHAR(3) NOT NULL, val FLOAT NOT NULL, PRIMARY KEY (id));", sep = "")
  db_res <- dbSendQuery(db_con, db_qry)
  dbClearResult(db_res)

  # Choose which data table columns to write to the database table
  db_cols <- c("obs", "icao", "rwy", "val")

  # FOR TESTING ONLY
  print(str(dt_wnd[, ..db_cols]))

  # Write to the headwind table
  # Here we use the deprecated RMySQL::MySQL() driver instead of the newer RMariaDB::MariaDB()) driver because it was found to be ~2.8 times faster here
  dbWriteTable(conn = db_con, name = paste(tolower(db_wnd), "_", tolower(nc_exp), sep = ""), value = dt_wnd[, ..db_cols], append = TRUE, row.names = FALSE)

  ##############################################################################
  # Add indices to the experiment table                                        #
  ##############################################################################

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste("[4/4] ", Sys.time(), " Worker ", Sys.getpid(), " is indexing the headwind table ", tolower(db_wnd), "_", tolower(nc_exp), "...", sep = ""))

  # Create a composite index on the icao and obs columns (after the bulk insert above, not before for performance reasons) to speed up subsequent searches
  db_idx <- "idx" # Set index name
  db_qry <- paste("CREATE INDEX ", tolower(db_idx), " ON ", tolower(db_wnd), "_", tolower(nc_exp), " (icao, obs);", sep = "")
  db_res <- dbSendQuery(db_con, db_qry)
  dbClearResult(db_res)

  # Disconnect the worker from the database
  dbDisconnect(db_con)

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), " Worker ", Sys.getpid(), " has completed its work for ", nc_exp, ".", sep = ""))
  
} # End of fn_wnd function definition

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################

# Define across how many workers each experiment (SSPs) should be further split to increase parallelization
split <- 4

# Set the number of cores/workers to use in the cluster
cores <- length(nc_exps) * split

# Set and clear the output file for cluster logging
close(file(log_wnd, open = "w"))

# Build the cluster of workers and select a file in which to log progress (which can't be printed to the console on the Windows version of RStudio)
cl <- makeCluster(cores, outfile = log_wnd)

# Have each worker load the libraries that they need to handle the nc_parse function defined above
clusterEvalQ(cl, {
  library(data.table)
  library(DBI)
})

# Pass the required variables from the main scope to the workers' scope
clusterExport(cl, c("dt_smp", "split"))

# Distribute the parallel parsing of NetCDF files across the workers, with the list of experiments (SSPs) and the range of splits as parameters
parLapply(cl, nc_exps, fn_wnd, 0:(split-1))

# Terminate the cluster once finished
stopCluster(cl)

################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF