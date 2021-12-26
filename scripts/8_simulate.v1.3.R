#===============================================================================
#    NAME: scripts/8_simulate.R
#   INPUT: 442,769,456 rows from database table cli created in 5_transform.R
# ACTIONS: Perform simulated takeoffs and return the takeoff distance required
#          Write the resulting TODR to the database table cli
#  OUTPUT: 442,769,456 rows of database table tko containing takeoff data
#===============================================================================

#===============================================================================
# 0 Housekeeping
#===============================================================================

# Load required libraries
library(data.table)
library(DBI)
library(parallel)

# Import the common settings
source("scripts/0_common.R")

# Import the simulation functions
source("scripts/6_model.v1.4.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

#===============================================================================
# 1 Prepare the simulation
#===============================================================================

#===============================================================================
# 1.1 Set up the lookup table to store the simulation results
#===============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

# Build the query to drop the table, if it exists
db_qry <- paste("DROP TABLE IF EXISTS ", tolower(db_tko), ";", sep = "")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Build the query to create the table
db_qry <- paste(
  "CREATE TABLE IF NOT EXISTS",
  tolower(db_tko),
  "(id INT UNSIGNED NOT NULL AUTO_INCREMENT,
    obs DATETIME NOT NULL,
    icao CHAR(4) NOT NULL,
    lat FLOAT NOT NULL,
    lon FLOAT NOT NULL,
    exp CHAR(6) NOT NULL,
    type CHAR(4) NOT NULL,
    mtom MEDIUMINT NOT NULL,
    hurs FLOAT NOT NULL,
    ps FLOAT NOT NULL,
    tas FLOAT NOT NULL,
    rho FLOAT NOT NULL,
    hdw FLOAT NOT NULL,
    rwy CHAR(5) NOT NULL,
    toda SMALLINT NOT NULL, 
    m MEDIUMINT NOT NULL,
    rto SMALLINT NOT NULL,
    todr SMALLINT NOT NULL,
    PRIMARY KEY (id));",
  sep = " ")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

#===============================================================================
# 1.2 Set the simulation variables
#===============================================================================

# Set the takeoff thrust reduction to its simulation value
reg_rto <- sim$sim_rto

# Set the regulatory margin for takeoff distance to its simulation value
reg_dis <- sim$sim_dis

#===============================================================================
# 1.3 Assemble the simulation data
#===============================================================================

#===============================================================================
# 1.3.1 Import the aircraft characteristics (from Sun et al., 2020)
#===============================================================================

dt_act <- fread(
  file = paste(path_aer, aer_act, sep = "/"),
  header = TRUE,
  colClasses = c(rep("factor", 3), rep("integer", 3), rep("numeric", 8)),
  drop = c("name", "eng"),
  key = "type"
)

#===============================================================================
# 1.3.2 Import the takeoff performance calibration data
#===============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

# Build a query to retrieve the calibration data
db_qry <- paste("SELECT type, m, cl, cd FROM", db_cal, ";", sep = " ")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a data table
dt_cal <- suppressWarnings(
  setDT(
    dbFetch(db_res, n = Inf), key = c("type", "m")
  )
)

# Convert the type column to factor
dt_cal[, type := as.factor(type)]

# Restore proper capitalization given that MySQL stores columns in lowercase
setnames(dt_cal, c("cl", "cd"), c("cL", "cD"))

# Release the database resource
dbClearResult(db_res)

#===============================================================================
# 1.3.3 Import the list of sample airports, excluding those already processed
#===============================================================================

# Build a query to retrieve the sample airports above the traffic threshold
db_qry <- paste(
  "SELECT DISTINCT icao FROM", db_pop,
  # "SELECT icao, lat, lon FROM", db_pop,
  # "GROUP BY(icao)", 
  "WHERE traffic >", pop_thr, ";",
  sep = " "
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a data table
dt_apt <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "icao"))

# Release the database resource
dbClearResult(db_res)

# Build a query to check if simulation outputs already exists in the database
db_qry <- paste(
  "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = '",
  db_grp,
  "' AND table_name = '", db_tko, "';",
  sep = ""
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a boolean
tbl_exists <- suppressWarnings(as.logical(dbFetch(db_res, n = Inf)))

# Release the database resource
dbClearResult(db_res)

# If the takeoff table already exists in the database
if (tbl_exists == TRUE) {
  
  # Build a query to retrieve the airports that were already processed
  db_qry <- paste("SELECT DISTINCT icao FROM", db_tko, ";", sep = " ")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Return the results to a data table of airports to exclude
  dt_exc <- suppressWarnings(
    setDT(
      dbFetch(db_res, n = Inf),
      key = "icao",
      check.names = FALSE
    )
  )
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Remove the airports already processed (inverse left outer join)
  dt_apt <- dt_apt[!dt_exc]
  
} # End if

# Disconnect from the database
dbDisconnect(db_con)

#===============================================================================
# 2 Define a function to simulate takeoffs at each airport
#===============================================================================

# TEST AIRPORT
# apt <- "BIKF"

fn_simulate <- function(icao) {
  
  #=============================================================================
  # 2.1 Import the climatic observations for the current airport
  #=============================================================================

  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to retrieve the takeoff conditions
  db_qry <- paste(
    "SELECT obs, exp, icao, tas, ps, rho, hdw, rwy, toda FROM ", db_cli,
    " WHERE icao = '", icao, "';",
    sep = ""
  )
  
  # Send the query to the database
  db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))
  
  # Return the results to a data table
  dt_cli <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "toda"))
  
  # Convert the runway column to factor
  dt_cli[, obs  := as.POSIXct(obs)]
  dt_cli[, exp  := as.factor(exp)]
  dt_cli[, icao := as.factor(icao)]
  dt_cli[, rwy  := as.factor(rwy)]
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect the worker from the database
  dbDisconnect(db_con)
  
  str(dt_cli)
  
  #=============================================================================
  # Perform final assembly of the simulation data
  #=============================================================================

  # Combine climatic observations & aircraft characteristics (Cartesian product)
  dt_tko <- dt_cli[, as.list(dt_act), by = dt_cli]

  # Set the starting mass in kg to the maximum takeoff mass
  dt_tko[, m := mtom]

  # Set the starting takeoff thrust reduction to its simulation value
  dt_tko[, rto := sim$sim_rto]

  # Add the calibration data (cL and cD) to the takeoff inputs
  dt_tko <- dt_cal[dt_tko, on = c("type", "m")]

  # Initialize a counter to track the number of iterations of each takeoff
  dt_tko[, i := 1L]

  # Initialize the starting TODR to a value greater than the max TODA
  # dt_tko[, todr := max(toda) + 1]

  # #=============================================================================
  # # Calculate the initial takeoff distance at the starting mass and thrust
  # #=============================================================================
  # 
  # print(
  #   paste(
  #     Sys.time(), "pid", Sys.getpid(), icao, "- Simulating",
  #     format(nrow(dt_tko), big.mark = ","), "takeoffs...", sep = " "
  #   )
  # )
  # 
  # fn_todr(dt_tko)
  # 
  # #=============================================================================
  # # Keep iterating the calculation for those observations where TODR > TODA
  # #=============================================================================
  # 
  # repeat {
  #   
  #   # Count how many observations have a TODR greater than TODA
  #   obs <- dim(dt_tko[todr > toda])[1]
  #   
  #   # Work only on observations where TODR remains greater than TODA
  #   if (obs > 0) {
  #     
  #     print(
  #       paste(
  #         Sys.time(), "pid", Sys.getpid(), icao, "- Iterating",
  #         format(obs, big.mark = ","), "takeoffs...", sep = " "
  #       )
  #     )
  #     
  #     # Decrease the thrust reduction by 1 percentage point (up to max. thrust)
  #     dt_tko[todr > toda & rto >  0, rto := rto - 1]
  #     
  #     # If still not enough, decrease the mass by 1 kg
  #     dt_tko[todr > toda & rto == 0, m   := m   - 1]
  #     
  #     # Remove the existing cL and cD values
  #     dt_tko[, c("cL", "cD") := NULL]
  #     
  #     # Add the calibration data (cL and cD) to the takeoff inputs
  #     dt_tko <- dt_cal[dt_tko, on = c("type", "m")]
  #     
  #     # Increment the iteration counter
  #     dt_tko[, i := i + 1]
  #     
  #     # Calculate the takeoff distance again
  #     dt_tko[todr > toda, todr := fn_todr(dt_tko[todr > toda])]
  #     
  #   # Once there are no more observations where TODR > TODA
  #   } else {
  #     
  #     # End the repeat loop
  #     break
  #     
  #   } # End if-else
  # 
  # } # End repeat
  # 
  # #=============================================================================
  # # Write results to the database
  # #=============================================================================
  # 
  # # TEMPORARY
  # dt_tko[, lat = 1]
  # dt_tko[, lon = 1]
  # 
  # # Connect the worker to the database
  # db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  # 
  # # Select which columns to write to the database and in which order
  # cols <- c("obs", "icao", "lat", "lon", "exp", "type", "mtom", "hurs", "ps", "tas", "rho", "hdw", "rwy", "toda", "m", "rto", "todr")
  # 
  # # Connect the worker to the database
  # db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  # 
  # # Write the data
  # # Here we use the deprecated RMySQL::MySQL() driver instead of the newer RMariaDB::MariaDB()) driver because it was found to be ~2.8 times faster here
  # dbWriteTable(conn = db_con, name = tolower(db_tko), value = dt_tko[, ..cols], append = TRUE, row.names = FALSE)
  # 
  # # Disconnect the worker from the database
  # dbDisconnect(db_con)
  
} # End of the fn_simulate function

#===============================================================================
# Handle the parallel computation across multiple cores                        #
#===============================================================================
  
# Set the number of workers to use in the cluster
cores <- 6

# Set the log file for the cluster defined in 0_common.R
outfile <- log_8

# Clear the log file
close(file(outfile, open = "w"))

# Build the cluster of workers
cl <- makeCluster(cores, outfile = outfile)

# Have each worker load the libraries that they need
clusterEvalQ(cl, {
  library(data.table)
  library(DBI)
})

# Pass the required variables from the main scope to the workers' scope
clusterExport(cl, c("db_cnf", "db_cli", "db_grp", "dt_act", "dt_cal", "fn_todr", "sim"))

# LIMIT NUMBER OF AIRPORTS FOR TESTING ONLY - REMOVE BEFORE GO-LIVE
# dt_apt <- head(dt_apt, cores)

# Distribute the unique airports across the workers
parLapply(cl, dt_apt, fn_simulate)

# Terminate the cluster once finished
stopCluster(cl)
  
#===============================================================================
# Add an index to the database table
#===============================================================================

# # Set the index name
# db_idx <- "idx"
# 
# # Build the query to create the index
# db_qry <- paste("CREATE INDEX ", tolower(db_idx), " ON ", tolower(db_tko), " (exp, icao);", sep = "")
# 
# # Send the query to the database
# db_res <- dbSendQuery(db_con, db_qry)
# 
# # Release the database resource
# dbClearResult(db_res)
  
#===============================================================================
# Housekeeping                                                                 #
#===============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF