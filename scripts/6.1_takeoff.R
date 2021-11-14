################################################################################
#    NAME: scripts/6_takeoff.R                                                 #
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

# Import the common settings and helper functions
source("scripts/0_common.R")
source("scripts/6.0_functions.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

################################################################################
# Prepare the simulation                                                       #
################################################################################
  
  # Set up the database table to store the takeoff calculations 
  fn_set_tbl(db_tko)
  
  # Import the simulation airports from the database
  dt_apt <- fn_imp_apt(pop_thr)
  
  # Import the simulation aircraft from the database
  # act <- c("a20n", "b38m", "a359", "b789")
  act <- c("a20n")
  
  # Return the aircraft characteristics
  dt_act <- fn_imp_act(act)
  
  # Select which runway surface friction coefficient to simulate
  mu <- fn_imp_rwy("dca_esdu")
  
################################################################################
# Define a function to simulate takeoffs at each airport                       #
################################################################################

# For a given airport apt
fn_takeoff <- function(apt) {
  
  ##############################################################################
  # Import the simulation conditions from the database                         #
  ##############################################################################
  
  # Retrieve the observations. The second argument is either "live" for actual data, or "test" for calibration data
  dt_smp <- fn_imp_obs(apt, "live")
  
  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), " Worker ", Sys.getpid(), " started simulating ", nrow(dt_smp)," takeoffs at airport ", apt, "...", sep = ""))
  
  # Define a list to hold the data in each iteration of the loop
  tko_list <- list()
  
  ##############################################################################
  # For each climate observation at the current airport                        #
  ##############################################################################
  
  for (i in 1:nrow(dt_smp)) {
    
    ############################################################################
    # Define simulation inputs                                                 #
    ############################################################################
    
    # Aircraft characteristics
    # n    <- dt_act[type == act, n]    # Old format
    n    <- dt_act[, n]       # Engine count in units
    bpr  <- dt_act[, bpr]     # Engine bypass ratio
    slst <- dt_act[, slst]    # Engine sea-level static maximum thrust in N (per engine)
    m    <- dt_act[, m]       # Maximum aircraft takeoff mass in kg
    S    <- dt_act[, S]       # Total wing surface area (incl. flaps/slats in takeoff configuration) in m²
CL <- .5
    CD   <- dt_act[, cd0]     # Dimensionless coefficient of drag in clean configuration
    
    # Climate inputs
    exp <- dt_smp[i, exp]     # Climate experiment (SSP)
    hdw <- dt_smp[i, wnd_hdw] # Headwind in meters per second at the active runway
    tas <- dt_smp[i, tas]     # Outside air temperature in K
    obs <- dt_smp[i, obs]     # Date and time
    rho <- dt_smp[i, rho]     # Air density in kg/m³
    ps  <- dt_smp[i, ps]      # Air pressure in Pa
    
    # Runway inputs
    rwy   <- dt_smp[i, rwy]   # Active runway (based on the prevailing wind at the time of the observation)
    toda  <- dt_smp[i, toda]  # Takeoff distance available in m
    theta <- 0                # Runway slope in °
    
    
    # Simulation settings
    int <- 10                 # Simulation resolution / number of integration steps
    j   <- 0                  # Initialize a counter for simulation iterations
    
    # Log file display options
    width <- 150
    options(width = width)    # Column width in the log file before word wrap
    
    ############################################################################
    # Simulate the takeoff from first principles                               #
    # The 'repeat' loop serves to increase engine thrust and decrease payload  #
    #  until the takeoff distance required fits within the distance available  #
    ############################################################################
    
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
      # Calculate the minimum takeoff airspeed in m/s                          #
      ##########################################################################
      
      V <- fn_sim_spd(W, S, CL, rho, hdw, int)
      
      ##########################################################################
      # Calculate the lift force in N                                          #
      ##########################################################################
      
      L <- fn_sim_lft(rho, V$tas, S, CL)
      
      ##########################################################################
      # Calculate the drag force in N                                          #
      ##########################################################################
      
      D <- fn_sim_drg(rho, V$tas, S, CD)
      
      ##########################################################################
      # Calculate the propulsive force F in N                                  #
      ##########################################################################
      
      F <- fn_sim_thr(n, bpr, slst, ps, tas, V$tas)

      ##########################################################################
      # Calculate the dynamic pressure q in Pa                                 #
      ##########################################################################
      
      q <- fn_sim_dyn(rho, V$tas)
      
      ##########################################################################
      # Calculate the acceleration a in m/s²                                   #
      ##########################################################################
      
      a <- fn_sim_acc(W, F$rto, mu, CD, CL, q, S, theta)
      
      ##########################################################################
      # Calculate the horizontal takeoff distance in m                         #
      ##########################################################################
      
      dis <- fn_sim_dis(a, V$gnd)
      
      ##########################################################################
      # Check if the takeoff distance required fits within that available      #
      # If not, increase the thrust by 1% at a time until maximum thrust       #
      # If still not enough, decrease mass by 1 kg at a time                   #
      # Once TODR <= TODA, break out of the repeat loop                        #
      ##########################################################################
      
      if (max(dis$todr) > toda) {
        
        ifelse(const$reg_rto < 1, const$reg_rto <- const$reg_rto + .01, m <- m - 1)
          
      } else {
        
        break;
        
      } # End of the if loop
    
    } # End of the repeat loop
    
    ############################################################################
    # Return results to the log file (for calibration, testing, and debugging) #
    # This section can safely be commented out to speed up script execution    #
    ############################################################################
    
    # Assemble the simulation steps into a data table
    dt_out <- data.table(
      Vgnd     = round(V$gnd, digits = 1),   # Groundspeed in m/s
      Vtas     = round(V$tas, digits = 1),   # Airspeed in m/s
      W        = round(W, digits = 1),       # Weight in N
      Fmax     = round(F$max, digits = 1),   # Max propulsive force in N
      Frto     = round(F$rto, digits = 1),   # Reduced propulsive force in N
      L        = round(L, digits = 1),       # Lift in N
      D        = round(D, digits = 1),       # Drag in N
      q        = round(q, digits = 1),       # Dynamic pressure
      a        = round(a, digits = 1),       # Acceleration in m/s²
      inc      = round(dis$inc, digits = 1), # Incremental takeoff distance in m
      cum      = round(dis$cum, digits = 1), # Cumulative takeoff distance in m
      todr     = round(dis$todr, digits = 1) # Regulatory takeoff distance required in m
    )
    
    # Display in the log file
    print(paste(rep("#", width), collapse = ""))
    print(paste("Takeoff", i, "-", dt_act[, type], "at", apt, "departing from", rwy, "on", obs, "Z under", toupper(exp), "with", round(hdw, digits = 1), "m/s headwind, an OAT of", tas, "K, and a QNH of", ps, "Pa:", sep = " "))
    print(dt_out)
    print(paste("Thrust increased by ", (const$reg_rto - .75) * 100, "% and mass decreased by ", dt_act[, m] - m, " kg after ", j, " iterations.", sep = ""))
    
    ############################################################################
    # Write the the takeoff performance calculations to the database           #
    ############################################################################
    
  #   # Assemble the results into a data table
  #   dt_res <- data.table(
  #     obs   = obs,                                              # Date and time of the observation
  #     icao  = apt,                                              # Airport ICAO code
  #     exp   = exp,                                              # Climate experiment (SSP)
  #     rwy   = rwy,                                              # Active runway at the time of the observation
  #     act   = aircraft,                                         # Aircraft type
  #     # T_inc = (F$max - F$rto) / F$max,                        # Percentage of thrust reduction from Tmax
  #     # m_dec = round(dt_act[act == aircraft, m] - m, digits = 0) # Kilograms of payload that had to be removed from the aircraft
  #   )
  #   
  #   # Append the results to the list initialized earlier
  #   tko_list[[i]] <- dt_res

  } # End of the loop

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
  cores <- 12
  
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
  clusterExport(cl, c("db_cnf", "db_grp", "db_imp", "db_cli", "db_tko", "dt_act", "mu", "const", "fn_imp_obs", "fn_sim_wgt", "fn_sim_spd", "fn_sim_lft", "fn_sim_drg", "fn_sim_thr", "fn_sim_dyn", "fn_sim_acc", "fn_sim_dis"))
  
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