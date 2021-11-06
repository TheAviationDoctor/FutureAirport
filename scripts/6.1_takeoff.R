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
  
  ##############################################################################
  # Set up the database table to store the takeoff performance calculations    #
  # Unlike in the other scripts, we don't drop the table if it exists already  #
  # This allows the user to break up the script's execution over several runs  #
  ##############################################################################
  
  # Connect to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to create the table
  db_qry <- paste("CREATE TABLE IF NOT EXISTS", tolower(db_tko), "(id INT UNSIGNED NOT NULL AUTO_INCREMENT, obs DATETIME NOT NULL, icao CHAR(4) NOT NULL, exp CHAR(6) NOT NULL, rwy CHAR(5) NOT NULL, act CHAR(4) NOT NULL, T_inc DECIMAL(3,2) NOT NULL, m_dec FLOAT NOT NULL, PRIMARY KEY (id));", sep = " ")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Release the database resource
  dbClearResult(db_res)
  
  ##############################################################################
  # Import the sample of airports to simulate                                  #
  ##############################################################################
  
  # Build a query to retrieve the sample airports
  db_qry <- paste("SELECT DISTINCT icao FROM ", db_pop, " WHERE traffic > ", pop_thr, ";", sep = "")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Return the results to a data frame
  df_apt <- suppressWarnings(dbFetch(db_res, n = Inf))
  
  # Release the database resource
  dbClearResult(db_res)
  
  ##############################################################################
  # Remove the airports that were already processed from the sample            #
  # This allows for incremental adds to the table over several script runs     #
  ##############################################################################
  
  # Build a query to check if the simulation outputs table already exists in the database
  db_qry <- paste("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = '", db_grp,"' AND table_name = '", db_tko,"';", sep = "")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Return the results to a data frame
  df_exi <- suppressWarnings(dbFetch(db_res, n = Inf))
  
  # Release the database resource
  dbClearResult(db_res)
  
  # If the takeoff table already exists in the database
  if(df_exi == 1) {
    
    # Build a query to retrieve the airports that were already processed
    db_qry <- paste("SELECT DISTINCT icao FROM ", db_tko, ";", sep = "")
    
    # Send the query to the database
    db_res <- dbSendQuery(db_con, db_qry)
    
    # Return the results to a data frame
    df_pro <- suppressWarnings(dbFetch(db_res, n = Inf))
    
    # Release the database resource
    dbClearResult(db_res)
    
    # Remove the airports already processed from the sample
    df_apt <- subset(df_apt, !(icao %in% df_pro$icao))
    print(paste(nrow(df_pro), "airports exist in the database table", db_tko,"and will be ommitted from the simulation.", nrow(df_apt), "airports remain.", sep = " "))
    
  }
  
  # Disconnect from the database
  dbDisconnect(db_con)
  
  ##############################################################################
  # Set simulation parameters                                                  #
  ##############################################################################
  
  # Define runway friction coefficients from Filippone (2012), Table 9.3 unless mentioned otherwise
  mu_options <- c(
    "blake" = .0165, # Value used by Blake (2009), p. 18-11
    "dca"   = .02,   # Value for dry concrete/asphalt recommended by ESDU 85029 (p. 32)
    "htg"   = .04,   # Value for hard turf and gravel
    "sdg"   = .05,   # Value for short and dry grass
    "lg"    = .10,   # Value for long grass
    "sg_lo" = .10,   # Value for soft ground (low softness)
    "sg_mi" = .20,   # Value for soft ground (medium softness)
    "sg_hi" = .30    # Value for soft ground (high softness)
  )
  
  # Define the aircraft characteristics from Sun et al. (2018, 2020)
  dt_act <- data.table(
    act  = c("A320", "A359", "B738", "B789"),                             # Aircraft type
    eng  = c("CFM56-5B4", "Trent XWB-84", "CFM56-7B26", "Trent 1000-K2"), # Engine type
    n    = rep(2L, 4),                                                    # Engine count in units
    bpr  = c(5.9, 9.01, 5.1, 9.04),                                       # Engine bypass ratio
    slst = c(117900, 379000, 116990, 350900),                             # Engine sea-level static maximum thrust in newtons (per engine)
    m    = c(78000, 280000, 79000, 254000),                               # Maximum aircraft takeoff mass in kilograms
    S    = c(124, 442, 124.6, 377),                                       # Total wing area (incl. flaps/slats at takeoff) in m²
    CL   = rep(1.6087, 4),                                                # Dimensionless coefficient of lift in takeoff configuration
    CD   = c(.078, .075, .076, .074)                                      # Dimensionless coefficient of drag in takeoff configuration
  )

################################################################################
# Define a function to simulate takeoffs at each airport                       #
################################################################################
  
fn_takeoff <- function(apt) {

  ##############################################################################
  # Import the time series of environmental observations at the current airport#
  ##############################################################################
  
  # Retrieve the observations
  dt_smp <- fn_imp_obs(apt)
  
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
    
    # Natural constants
    g <- 9.806665                         # Gravitational acceleration in m/s², assuming a non-oblate, non-rotating Earth (Blake, 2009; Daidzic, 2016)
    
    # Aircraft characteristics
    aircraft <- "A320"                    # Aircraft type
    n    <- dt_act[act == aircraft, n]    # Engine count in units
    bpr  <- dt_act[act == aircraft, bpr]  # Engine bypass ratio
    slst <- dt_act[act == aircraft, slst] # Engine sea-level static maximum thrust in N (per engine)
    m    <- dt_act[act == aircraft, m]    # Maximum aircraft takeoff mass in kg
    S    <- dt_act[act == aircraft, S]    # Total wing surface area (incl. flaps/slats in takeoff configuration) in m²
    CL   <- dt_act[act == aircraft, CL]   # Dimensionless coefficient of lift in takeoff configuration
    CD   <- dt_act[act == aircraft, CD]   # Dimensionless coefficient of drag in takeoff configuration

    # Climate inputs
    exp <- dt_smp[i, exp]                 # Climate experiment (SSP)
    hdw <- dt_smp[i, wnd_hdw]             # Headwind in meters per second at the active runway
    tas <- dt_smp[i, tas]                 # Outside air temperature in K
    obs <- dt_smp[i, obs]                 # Date and time
    rho <- dt_smp[i, rho]                 # Air density in kg/m³
    ps  <- dt_smp[i, ps]                  # Air pressure in Pa
    
    # Runway inputs
    rwy   <- dt_smp[i, rwy]               # Active runway (based on the prevailing wind at the time of the observation)
    toda  <- dt_smp[i, toda]              # Takeoff distance available in m
    theta <- 0                            # Runway slope in °
    mu    <- mu_options["dca"]            # Type of runway surface
    
    # Operational and regulatory inputs
    spd <- 1.2                            # Safety factor to apply to the speed at which lift equals weight
    dis <- 1.15                           # Percent of the horizontal distance along the takeoff path, with all engines operating, from the start of the takeoff to a point equidistant between the point at which VLOF is reached and the point at which the airplane is 35 feet above the takeoff surface, according to 14 CFR § 25.113 (1998)
    rto <- .75                            # Minimum percentage of takeoff thrust permissible under reduced takeoff thrust operations as per FAA Advisory Circular 25-13 (1988)
    
    # Simulation settings
    int <- 10                             # Simulation resolution / number of integration steps
    j   <- 0                              # Initialize a counter for simulation iterations
    
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
      
      W <- g * m
      
      ##########################################################################
      # Calculate the minimum takeoff airspeed in m/s                          #
      ##########################################################################
      
      Vtko <- fn_tko_speed(W, S, CL, rho, spd)
      
      ##########################################################################
      # Create airspeed intervals up to the minimum takeoff airspeed           #
      ##########################################################################
      
      Vtas <- seq(from = 0 + hdw, to = Vtko, length = int)
      
      ##########################################################################
      # Create groundspeed intervals up to the minimum takeoff airspeed        #
      ##########################################################################
      
      Vgnd <- seq(from = 0, to = Vtko - hdw, length = int)
      
      ##########################################################################
      # Calculate the lift force in N at each airspeed increment               #
      ##########################################################################
      
      L <- fn_tko_lift(rho, Vtas, S, CL)
      
      ##########################################################################
      # Calculate the drag force in N at each airspeed increment               #
      ##########################################################################
      
      D <- fn_tko_drag(rho, Vtas, S, CD)
      
      ##########################################################################
      # Calculate the propulsive force (maximum takeoff thrust) in N           #
      ##########################################################################
      
      Fmax <- fn_tko_thrust(n, bpr, slst, ps, tas, Vtas)
      
      ##########################################################################
      # Calculate the propulsive force (reduced takeoff thrust) in N           #
      ##########################################################################
      
      Frto <- Fmax * rto
      
      ##########################################################################
      # Calculate the dynamic pressure at each airspeed increment              #
      ##########################################################################
      
      q <- fn_tko_dyn_press(rho, Vtas)
      
      ##########################################################################
      # Calculate the aircraft acceleration in m/s² at each airspeed increment #
      ##########################################################################
      
      a <- fn_tko_accel(g, W, Frto, mu, CD, CL, q, S, theta)
      
      ##########################################################################
      # Calculate the cumulative distance in m covered by the aircraft         #
      ##########################################################################
      
      cum <- fn_tko_dist(a, Vgnd)
      
      ##########################################################################
      # Calculate the regulatory takeoff distance required (TODR) in m         #
      ##########################################################################
      
      todr <- max(cum) * dis
      
      ##########################################################################
      # Check if the takeoff distance required fits within that available      #
      # If not, increase the thrust by 1% at a time until maximum thrust       #
      # If still not enough, decrease mass by 1 kg at a time                   #
      # Once TODR <= TODA, break out of the repeat loop                        #
      ##########################################################################
      
      if (todr > toda) {
        
        ifelse(rto < 1, rto <- rto + .01, m <- m - 1)
          
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
      Vgnd     = round(Vgnd, digits = 1), # Groundspeed in m/s
      Vtas     = round(Vtas, digits = 1), # Airspeed in m/s
      W        = round(W, digits = 1),    # Weight in N
      L        = round(L, digits = 1),    # Lift in N
      D        = round(D, digits = 1),    # Drag in N
      q        = round(q, digits = 1),    # Dynamic pressure
      a        = round(a, digits = 1),    # Acceleration in m/s²
      cum      = round(cum, digits = 1)   # Cumulative takeoff distance in m
    )
    
    # Display in the log file
    width <- 150
    options(width = width)
    print(paste(rep("#", width), collapse = ""))
    print(paste("Takeoff", i, "-", aircraft, "at", apt, "departing from", rwy, "on", obs, "Z under", toupper(exp), "with", round(hdw, digits = 1), "m/s headwind, an OAT of", tas, "K, and a QNH of", ps, "Pa:", sep = " "))
    print(dt_out)
    print(paste("Thrust increased by ", (rto - .75) * 100, "% and mass decreased by ", dt_act[act == aircraft, m] - m, " kg after ", j, " iterations.", sep = ""))
    
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
  clusterExport(cl, c("db_cnf", "db_grp", "db_imp", "db_cli", "db_tko", "dt_act", "mu_options", "fn_imp_obs", "fn_tko_speed", "fn_tko_lift", "fn_tko_drag", "fn_tko_thrust", "fn_tko_dyn_press", "fn_tko_accel", "fn_tko_dist"))
  
# LIMIT NUMBER OF AIRPORTS FOR TESTING ONLY - REMOVE BEFORE GO-LIVE
df_apt <- head(df_apt, 1)

# Distribute the unique airports across the workers
parLapply(cl, df_apt$icao, fn_takeoff)

# Terminate the cluster once finished
stopCluster(cl)

################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF