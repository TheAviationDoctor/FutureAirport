################################################################################
#    NAME: scripts/7_takeoff.R                                                 #
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
source("scripts/6_model.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# Import the aircraft data
dt_act <- fn_imp_act()

# Create a database table to store the simulation outputs 
fn_set_tbl(db_tko)

# Import the simulation airports from the database
dt_apt <- fn_imp_apt()

################################################################################
# Define a function to simulate takeoffs at each airport                       #
################################################################################

# For a given airport (one per cluster)
fn_takeoff <- function(apt) {
  
  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), " Worker ", Sys.getpid(), " started simulating takeoffs at airport ", apt, "...", sep = ""))
  
  ##############################################################################
  # Import the climatic observations from the database                         #
  ##############################################################################
  
  # Retrieve the climatic observations at the current airport
  dt <- fn_imp_obs(apt)
  
  # Merge the climatic observations with the aircraft data
  
  # Define a list to hold the data in each iteration of the loop
  tko_list <- list()
  
  # For each observation
  for (i in 1:nrow(dt)) {
    
    ############################################################################
    # Simulate the takeoff from first principles                               #
    # The 'repeat' loop serves to increase engine thrust and decrease payload  #
    #  until the takeoff distance required fits within the distance available  #
    ############################################################################
    
    # Start a timer for the current climate observation simulation
    i_time <- Sys.time()
    
    # Initialize a counter to track the number of simulation iterations
    j <- 0
    
    # Initialize the variables to solve for iteratively
    rto <- const$reg_rto # Takeoff thrust reduction
    m   <- dt_act[, m]   # Takeoff mass in kg
    
    repeat {
      
      ##########################################################################
      # Increment the iteration counter                                        #
      ##########################################################################
      
      j <- j + 1
      
      ##########################################################################
      # Calculate the weight force in N                                        #
      ##########################################################################
      
      W <- fn_sim_wgt(m)
      
      ##########################################################################
      # Calculate the drag and lift coefficients cD and cL (dimensionless)     #
      ##########################################################################
      
      # c <- fn_sim_cff(dt_act[, cD0], dt_act[, k], dt_act[, lambda_f], dt_act[, cfc], dt_act[, SfS], dt_act[, flap_angle], m, dt_act[, S], dt_act[, span])
      
      ##########################################################################
      # Calculate the speed in m/s at which lift L equals weight W             #
      ##########################################################################
      
      Vlof <- fn_sim_vlo(W, dt_act[, S], dt_act[, cL], dt[i, rho])
      
      ##########################################################################
      # Calculate the takeoff speeds V in m/s                                  #
      ##########################################################################
      
      V <- fn_sim_spd(W, dt_act[, S], dt_act[, cL], dt[i, wnd_hdw])
      
      ##########################################################################
      # Calculate the dynamic pressure q in Pa                                 #
      ##########################################################################
      
      q <- fn_sim_dyn(dt[i, rho], V$tas)
      
      ##########################################################################
      # Calculate the lift force in N                                          #
      ##########################################################################
      
      L <- fn_sim_lft(q, dt_act[, S], dt_act[, cL])
      
      ##########################################################################
      # Calculate the drag force in N                                          #
      ##########################################################################
      
      D <- fn_sim_drg(q, dt_act[, S], dt_act[, cD])
      
      ##########################################################################
      # Calculate the propulsive force F in N                                  #
      ##########################################################################
      
      F <- fn_sim_thr(dt_act[, n], dt_act[, bpr], dt_act[, slst], dt[i, ps], dt[i, tas], V$tas, rto)
      
      ##########################################################################
      # Calculate the acceleration a in m/s²                                   #
      ##########################################################################
      
      a <- fn_sim_acc(W, F$rto, dt_act[, cD], dt_act[, cL], q, dt_act[, S])
      
      ##########################################################################
      # Calculate the horizontal takeoff distance in m                         #
      ##########################################################################
      
      dis <- fn_sim_dis(a, V$gnd)
      
      ##########################################################################
      # Check if the takeoff distance required fits within that available      #
      # If not, decrease the thrust reduction percentage by 1% until max thrust#
      # If still not enough, decrease mass by 1 kg at a time                   #
      # Once TODR <= TODA, break out of the repeat loop                        #
      ##########################################################################
      
      if (max(dis$todr) > dt[i, toda]) {
        
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
      i        = i,                                     # Simulated takeoff number
      obs      = dt[i, obs],                        # Date and time of the simulated takeoff
      # exp      = toupper(dt[i, exp]),               # Climate experiment
      icao     = apt,                                   # Airport ICAO code
      act      = toupper(dt_act[, type]),               # Aircraft type
      # rwy      = dt[i, rwy],                        # Active runway
      hdw      = round(dt[i, wnd_hdw], digits = 1), # Headwind in m/s
      tas      = round(dt[i, tas], digits = 1),     # Outside air temperature in K
      ps       = round(dt[i, ps], digits = 1),      # Barometric pressure in Pa
      Vgnd     = round(V$gnd, digits = 1),              # Groundspeed in m/s
      Vtas     = round(V$tas, digits = 1),              # Airspeed in m/s
      Fmax     = round(F$max, digits = 1),              # Max propulsive force in N
      Frto     = round(F$rto, digits = 1),              # Reduced propulsive force in N
      rto      = rto,                                   # Percentage of takeoff thrust reduction
      MTOM     = round(dt_act[, m], digits = 1),        # Maximum takeoff mass in kg
      m        = round(m, digits = 1),                  # Actual takeoff mass in kg
      rem      = round(dt_act[, m] - m, digits = 1),    # Mass in kg of payload that had to be removed from the aircraft
      W        = round(W, digits = 1),                  # Takeoff weight in N
      L        = round(L, digits = 1),                  # Lift in N
      D        = round(D, digits = 1),                  # Drag in N
      q        = round(q, digits = 1),                  # Dynamic pressure
      a        = round(a, digits = 1),                  # Acceleration in m/s²
      inc      = round(dis$inc, digits = 1),            # Incremental takeoff distance in m
      cum      = round(dis$cum, digits = 1),            # Cumulative takeoff distance in m
      todr     = round(dis$todr, digits = 1),           # Regulatory takeoff distance required in m
      toda     = dt[i, toda]                        # Takeoff distance available in m
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
      obs      = dt[i, obs],                 # Date and time of the simulated takeoff
      icao     = apt,                            # Airport ICAO code
      exp      = dt[i, exp],                 # Climate experiment
      act      = dt_act[, type],                 # Aircraft type
      rto      = rto,                            # Percentage of takeoff thrust reduction
      rem      = dt_act[, m] - m,                # Mass in kg of payload that had to be removed from the aircraft
      j        = j,                              # Number of simulation iterations
      time     = as.numeric(Sys.time() - i_time) # Execution time for the current climate observation simulation
    )
    
    print(dt_res)
    
    tko_list[[i]] <- dt_res
    
  } # End of the per-climate-observation loop
  
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
  
} # End of the fn_takeoff function

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################
  
  ##############################################################################
  # Prepare the cluster                                                        #
  ##############################################################################
  
  # Set the number of workers to use in the cluster
  cores <- 6
  
  # Set the log file for the cluster defined in 0_common.R
  outfile <- log_6
  
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
  clusterExport(cl, c("db_cnf", "db_grp", "db_imp", "db_cli", "db_tko", "db_tst", "dt_act", "fn_imp_obs", "fn_sim_wgt", "fn_sim_cff", "fn_sim_spd", "fn_sim_lft", "fn_sim_drg", "fn_sim_thr", "fn_sim_dyn", "fn_sim_acc", "fn_sim_dis"))
  
# LIMIT NUMBER OF AIRPORTS FOR TESTING ONLY - REMOVE BEFORE GO-LIVE
dt_apt <- head(dt_apt, 1)

# Distribute the unique airports across the workers
parLapply(cl, dt_apt, fn_takeoff)

# Terminate the cluster once finished
stopCluster(cl)

################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF