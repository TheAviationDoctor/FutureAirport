################################################################################
#    NAME: scripts/7_simulate.R                                                #
#   INPUT: 442,769,456 rows from database table cli created in 5_transform.R   #
# ACTIONS: Estimate relationship between thrust and airspeed from Blake (2009) #
#          Calculate the takeoff distance required (TODR)                      #
#          Write the resulting TODR to the database table cli                  #
#  OUTPUT: 442,769,456 rows of database table tko containing takeoff data      #
# RUNTIME: ~X hours on the researcher's config (https://bit.ly/3ChCBAP)        #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(DBI)
library(parallel)

# Import the common settings
source("scripts/0_common.R")

# Import the simulation functions
source("scripts/6_model.v1.1.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

################################################################################
# Assemble the simulation data                                                 #
################################################################################
  
  ##############################################################################
  # Import the aircraft characteristics (from Sun et al., 2020)                #
  ##############################################################################
  
  dt_act <- fread(
    file = paste(path_aer, aer_act, sep = "/"),
    header = TRUE,
    colClasses = c(rep("factor", 3), rep("integer", 3), rep("numeric", 8))
  )
  
  ##############################################################################
  # Import the takeoff performance calibration data                            #
  ##############################################################################
  
  # Connect to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to retrieve the calibration data
  db_qry <- paste("SELECT type, m, cl FROM", db_cal, ";", sep = " ")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Return the results to a data table
  dt_cal <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = c("type", "m")))
  
  # Restore proper capitalization given that MySQL stores columns in lowercase
  setnames(dt_cal, c("cl", "cd"), c("cL", "cD"))
  
  # Release the database resource
  dbClearResult(db_res)
  
  ##############################################################################
  # Import the list of sample airports, excluding those already processed      #
  ##############################################################################
  
  # Build a query to retrieve the sample airports above the traffic threshold
  db_qry <- paste(
    "SELECT DISTINCT icao FROM", db_pop,
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
    dt_exc <- suppressWarnings(setDT(
      dbFetch(db_res, n = Inf),
      key = "icao",
      check.names = FALSE
    ))
    
    # Release the database resource
    dbClearResult(db_res)
    
    # Remove the airports already processed (inverse left outer join)
    dt_apt <- dt_apt[!dt_exc]
    
  }
  
  # Disconnect from the database
  dbDisconnect(db_con)

################################################################################
# Define a function to simulate takeoffs at each airport                       #
################################################################################

fn_simulate <- function(apt) {

  ##############################################################################
  # Import the climatic observations for the current airport                   #
  ##############################################################################
  
  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), Sys.getpid(), "Started loading", apt, sep = " "))
  
  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to retrieve the takeoff conditions
  db_qry <- paste(
    "SELECT id, obs, exp, tas, ps, rho, hdw, rwy, toda FROM ", db_cli,
    " WHERE icao = '", apt, "';",
    sep = ""
  )
  
  # Send the query to the database
  db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))
  
  # Return the results to a data table
  dt_cli <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "id"))
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect the worker from the database
  dbDisconnect(db_con)
  
  ##############################################################################
  # Perform final assembly of the simulation data                              #
  ##############################################################################
  
  # Merge the climatic observations and aircraft characteristics
  dt <- dt_cli[, as.list(dt_act), by = dt_cli]
  
  # Set the starting mass in kg to the maximum takeoff mass
  dt[, m := mtom]
  
  # Set the aircraft weight in N based upon the starting mass
  dt[, W := sim$g * m]
  
  # Set the starting takeoff thrust reduction
  dt[, rto := sim$reg_rto]
  
  # Initialize columns for the lift and drag coefficients
  dt[, cL := 0]
  dt[, cD := 0]

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), Sys.getpid(), "Finished loading", apt, sep = " "))

  ##############################################################################
  # Perform the takeoff simulation on every climatic observation               #
  ##############################################################################

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), Sys.getpid(), "Started simulating", apt, sep = " "))

  # REDUCE DATASET FOR TESTING ONLY
  dt <- head(dt, 10^1)
  print(str(dt))

  fn_simulate <- function(i) {

    # Repeat each takeoff simulation until TODR <= TODA
    repeat {
      
      print(dt[i, type])
      print(dt[i, m])
      print(dt_cal[type == dt[i, type] & m == dt[i, m], ])
      
      # Fetch lift and drag coefficient values from the calibration lookup table
      dt[i, cL := dt_cal[type == dt[i, type] & m == dt[i, m], cL]]
      dt[i, cD := dt_cal[type == dt[i, type] & m == dt[i, m], cD]]
      
      print(dt[i, ])
      
      # Calculate the TODR
      todr <- fn_todr(dt[i, ])

      # Check if the takeoff distance required fits within that available
      if (todr > dt[i, toda]) {

        # If not, decrease the thrust reduction by 1 per. point until max thrust
        # If still not enough, decrease mass by 1 kg at a time
        ifelse(dt[i, rto] > 0, dt[i, rto := rto - 1], dt[i, m := m - 1])

      } else {

        # Once TODR <= TODA, break out of the repeat loop
        break;

      } # End of the if loop

    } # End of the repeat loop

  }

  # Calculate the regulatory takeoff distance required in m
  dt[, todr := sapply(1:nrow(dt), fn_simulate)]
  
  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), Sys.getpid(), "Finished simulating", apt, sep = " "))
  
} # End of the fn_simulate function

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################
  
  ##############################################################################
  # Prepare the cluster                                                        #
  ##############################################################################
  
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
  dt_apt <- head(dt_apt, 1)
  
  # Distribute the unique airports across the workers
  parLapply(cl, dt_apt, fn_simulate)
  
  # Terminate the cluster once finished
  stopCluster(cl)
  
################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF