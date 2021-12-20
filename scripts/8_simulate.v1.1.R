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
# Import the aircraft characteristics                                          #
################################################################################

# Import the CSV file (from Sun et al., 2020) into a data table
dt_act <- fread(
  file = paste(path_aer, aer_act, sep = "/"),
  header = TRUE,
  colClasses = c(rep("factor", 3), rep("integer", 3), rep("numeric", 8))
)

################################################################################
# Define a function to simulate takeoffs at each airport                       #
################################################################################

fn_simulate <- function(apt) {
  
  # Start a timer for the current airport
  apt_time <- Sys.time()
  
  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(
    Sys.time(),
    " Worker ",
    Sys.getpid(),
    " started simulating takeoffs at airport ",
    apt,
    "...",
    sep = ""
  ))
  
  ##############################################################################
  # Import the climatic observations from the database                         #
  ##############################################################################
  
  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to retrieve the takeoff conditions
  db_qry <- paste(
    "SELECT id, obs, exp, tas, ps, rho, hdw, rwy, toda FROM ",
    db_cli,
    " WHERE icao = '",
    apt,
    "' LIMIT 5;",
    sep = ""
  )
  
  # Send the query to the database
  db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))
  
  # Return the results to a data table
  dt_cli <- suppressWarnings(setDT(
    dbFetch(db_res, n = Inf),
    key = "id",
    check.names = FALSE)
  )
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect the worker from the database
  dbDisconnect(db_con)
  
  # Define a list to hold the data in each iteration of the loop
  tko_list <- list()
  
  # For each aircraft
  for (i in 1:nrow(dt_act)) {

    # For each climatic observation at the airport
    for (j in 1:nrow(dt_cli)) {
      
      ############################################################################
      # Simulate the takeoff from first principles                               #
      # The 'repeat' loop serves to increase engine thrust and decrease payload  #
      #  until the takeoff distance required fits within the distance available  #
      ############################################################################
      
      # Initialize a counter to track the number of simulation iterations
      k <- 0
      
      # Set the initial percentage of takeoff thrust reduction
      # to that permissible under FAA AC 25-13 (1988).
      rto <- 25L
      
      # Set the initial takeoff mass in kg to the aircraft's maximum takeoff mass
      m   <- dt_act[i, mtom]
      
      repeat {
        
        ##########################################################################
        # Increment the iteration counter                                        #
        ##########################################################################
        
        k <- k + 1
        
        todr <- ceiling(fn_sim_tko(dt_act[i, ], dt_cli[j, ]))
        
        ##########################################################################
        # Check if the takeoff distance required fits within that available      #
        # If not, decrease the thrust reduction percentage by 1% until max thrust#
        # If still not enough, decrease mass by 1 kg at a time                   #
        # Once TODR <= TODA, break out of the repeat loop                        #
        ##########################################################################
        
        if (todr > dt_cli[j, toda]) {
          
          ifelse(rto > 0, rto <- rto - 1, m <- m - 1)
          
        } else {
          
          break;
          
        } # End of the if loop
      
      } # End of the repeat loop
      
      ############################################################################
      # Return results to the log file (for calibration, testing, and debugging) #
      # This section can safely be commented out to speed up script execution    #
      ############################################################################
      
      # # Assemble the simulation outputs into a data table
      dt_out <- data.table(
        j        = j,                                     # Simulated takeoff number
        obs      = dt[j, obs],                        # Date and time of the simulated takeoff
        exp      = toupper(dt[j, exp]),               # Climate experiment
        icao     = apt,                                   # Airport ICAO code
        act      = toupper(dt_act[i, type]),               # Aircraft type
        rwy      = dt[j, rwy],                        # Active runway
        hdw      = round(dt[j, hdw], digits = 1), # Headwind in m/s
        tas      = round(dt[j, tas], digits = 1),     # Outside air temperature in K
        ps       = round(dt[j, ps], digits = 1),      # Barometric pressure in Pa
        Vgnd     = round(V$gnd, digits = 1),              # Groundspeed in m/s
        Vtas     = round(V$tas, digits = 1),              # Airspeed in m/s
        Fmax     = round(F$max, digits = 1),              # Max propulsive force in N
        Frto     = round(F$rto, digits = 1),              # Reduced propulsive force in N
        rto      = rto,                                   # Percentage of takeoff thrust reduction
        MTOM     = round(dt_act[i, m], digits = 1),        # Maximum takeoff mass in kg
        m        = round(m, digits = 1),                  # Actual takeoff mass in kg
        rem      = round(dt_act[i, m] - m, digits = 1),    # Mass in kg of payload that had to be removed from the aircraft
        W        = round(W, digits = 1),                  # Takeoff weight in N
        L        = round(L, digits = 1),                  # Lift in N
        D        = round(D, digits = 1),                  # Drag in N
        q        = round(q, digits = 1),                  # Dynamic pressure
        a        = round(a, digits = 1),                  # Acceleration in m/s²
        # inc      = round(dis$inc, digits = 1),            # Incremental takeoff distance in m
        # cum      = round(dis$cum, digits = 1),            # Cumulative takeoff distance in m
        # todr     = round(dis$todr, digits = 1),           # Regulatory takeoff distance required in m
        todr     = todr,           # Regulatory takeoff distance required in m
        toda     = dt[j, toda]                        # Takeoff distance available in m
      )
  
      # Set a large column width to avoid table wrap in the log file
      width <- 200
      options(width = width)
  
      # Output to the log file
      print(paste(rep("#", width), collapse = ""))
      print(dt_out)
      
      ############################################################################
      # Append the results to a list before running through the loop again       #
      ############################################################################
      
      dt_res <- data.table(
        obs      = dt[j, obs],                 # Date and time of the simulated takeoff
        icao     = apt,                            # Airport ICAO code
        exp      = dt[j, exp],                 # Climate experiment
        act      = dt_act[i, type],                 # Aircraft type
        rto      = rto,                            # Percentage of takeoff thrust reduction
        rem      = dt_act[i, m] - m,                # Mass in kg of payload that had to be removed from the aircraft
        k        = k,                              # Number of simulation iterations
      )
      
      print(dt_res)
      
      tko_list[[j]] <- dt_res
      
    } # End of the per-climate-observation loop
  
  } # End of the per-aircraft loop
  
  ############################################################################
  # Write the the takeoff performance calculations to the database           #
  ############################################################################
  
  # # Combine the outputs of all takeoff simulations at the current airport into a data table
  # dt_tko <- rbindlist(tko_list)
  # 
  # # Connect the worker to the database
  # db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Write the data
  # Here we use the deprecated RMySQL::MySQL() driver instead of the newer RMariaDB::MariaDB()) driver because it was found to be ~2.8 times faster here
  # dbWriteTable(conn = db_con, name = tolower(db_tko), value = dt_tko, append = TRUE, row.names = FALSE)
  
  # Disconnect the worker from the database
  # dbDisconnect(db_con)
  
} # End of the fn_simulate function

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################
  
  ##############################################################################
  # Import the list of airports from the database that are above the threshold #
  ##############################################################################
  
  # Connect to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to retrieve the sample airports above the traffic threshold
  db_qry <- paste(
    "SELECT DISTINCT icao FROM", db_pop,
    "WHERE traffic >", pop_thr, ";",
    sep = " "
  )
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Return the results to a data table
  dt_apt <- suppressWarnings(setDT(
    dbFetch(db_res, n = Inf),
    key = "icao",
    check.names = FALSE
  ))
  
  # Release the database resource
  dbClearResult(db_res)
  
  ##############################################################################
  # Exclude airports that were already processed by earlier simulations        #
  ##############################################################################
  
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
  
  ##############################################################################
  # Prepare the cluster                                                        #
  ##############################################################################
  
  # Set the number of workers to use in the cluster
  cores <- 1
  
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
  clusterExport(cl, c("db_cnf", "db_grp", "db_cli", "dt_apt", "dt_act", "fn_sim_tko"))
  
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