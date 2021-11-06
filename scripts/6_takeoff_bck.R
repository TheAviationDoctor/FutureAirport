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

# Import the common settings
source("scripts/0_common.R")

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
  # Import the sample of airports and runways                                  #
  ##############################################################################
  
  # Build a query to retrieve the sample airports
  db_qry <- paste("SELECT DISTINCT icao FROM ", db_pop, " WHERE traffic > ", pop_thr, ";", sep = "")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Return the results to a data frame
  df_apt <- suppressWarnings(dbFetch(db_res, n = Inf))
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect from the database
  dbDisconnect(db_con)
  
  ##############################################################################
  # Set the aircraft characteristics from Sun et al. (2018, 2020)              #
  ##############################################################################
  
  dt_act <- data.table(
    act  = c("A320", "A359", "B738", "B789"),                             # Aircraft type
    eng  = c("CFM56-5B4", "Trent XWB-84", "CFM56-7B26", "Trent 1000-K2"), # Engine type
    n    = rep(2L, 4),                                                    # Engine count in units
    bpr  = c(5.9, 9.01, 5.1, 9.04),                                       # Engine bypass ratio
    slst = c(117900, 379000, 116990, 350900),                             # Engine sea-level static maximum thrust in newtons (per engine)
    m    = c(78000, 280000, 79000, 254000),                               # Maximum aircraft takeoff mass in kilograms
    S    = c(124, 442, 124.6, 377),                                       # Total wing area (incl. flaps/slats at takeoff) in square meters
    CL   = rep(1.6087, 4),                                                # Dimensionless coefficient of lift in takeoff configuration
    CD   = c(.078, .075, .076, .074)                                      # Dimensionless coefficient of drag in takeoff configuration
  )
  
  ##############################################################################
  # Set the runway's dimensionless coefficient of rolling friction             #
  ##############################################################################
  
  # List all possible values of mu encountered in the literature
  mu <- c(
    "blake" = .0165, # Value used by Blake (2009), p. 18-11
    "dca"   = .02,   # Value for dry concrete/asphalt recommended by ESDU 85029 (p. 32)
    "htg"   = .04,   # Value for hard turf and gravel from Filippone (2012), Table 9.3
    "sdg"   = .05,   # Value for short and dry grass from Filippone (2012), Table 9.3
    "lg"    = .10,   # Value for long grass from Filippone (2012), Table 9.3
    "sg_lo" = .10,   # Value for soft ground (low softness) from Filippone (2012), Table 9.3
    "sg_mi" = .20,   # Value for soft ground (medium softness) from Filippone (2012), Table 9.3
    "sg_hi" = .30    # Value for soft ground (high softness) from Filippone (2012), Table 9.3
  )
  
  # Choose which coefficient to use in the simulation
  mu <- mu["dca"]
  
  ##############################################################################
  # Set the simulation's resolution / number of integration steps              #
  ##############################################################################
  int <- 10
  
################################################################################
# Define a function to simulate takeoffs at each airport                       #
################################################################################
  
fn_takeoff <- function(apt) {
  
  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), " Worker ", Sys.getpid(), " is parsing airport ", apt, "...", sep = ""))
  
  # Import the required functions
  source("scripts/0_functions.R")
  
  ##############################################################################
  # Import the takeoff conditions at the current airport                       #
  ##############################################################################
  
  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to retrieve the takeoff conditions
  db_qry <- paste("SELECT id, obs, exp, tas, ps, rho, wnd_hdw, rwy, toda FROM ", db_cli, " WHERE exp = 'ssp126' AND icao = '", apt, "' LIMIT 5;", sep = "")
  
  # Send the query to the database
  db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))
  
  # Return the results
  dt_smp <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "id", check.names = FALSE))
  
  # Release the database resource
  dbClearResult(db_res)

  ##############################################################################
  # Define airport-level simulation inputs                                     #
  ##############################################################################
  
  # Gravitational acceleration in meters per second squared, assuming a non-oblate, non-rotating Earth (Blake, 2009; Daidzic, 2016)
  g <- 9.806665
  
  # Aircraft type
  aircraft <- "A320"
  
  # Aircraft characteristics
  n    <- dt_act[act == aircraft, n]    # Engine count in units
  bpr  <- dt_act[act == aircraft, bpr]  # Engine bypass ratio
  slst <- dt_act[act == aircraft, slst] # Engine sea-level static maximum thrust in newtons (per engine)
  m    <- dt_act[act == aircraft, m]    # Maximum aircraft takeoff mass in kilograms
  S    <- dt_act[act == aircraft, S]    # Total wing surface area (incl. flaps/slats in takeoff configuration) in m²
  CL   <- dt_act[act == aircraft, CL]   # Dimensionless coefficient of lift in takeoff configuration
  CD   <- dt_act[act == aircraft, CD]   # Dimensionless coefficient of drag in takeoff configuration
  W    <- m * g                         # Aircraft weight in newtons
  
  # Define a list to hold the data in each iteration of the loop
  tko_list <- list()
  
  ##############################################################################
  # For each climate observation at the current airport                        #
  ##############################################################################
  
  for (i in 1:nrow(dt_smp)) {
    
    ############################################################################
    # Define observation-level simulation inputs                               #
    ############################################################################
    
    # Climate inputs
    exp <- dt_smp[i, exp]     # Climate experiment (SSP)
    hdw <- dt_smp[i, wnd_hdw] # Headwind in meters per second at the active runway
    tas <- dt_smp[i, tas]     # Outside air temperature in kelvins
    obs <- dt_smp[i, obs]     # Date and time
    rho <- dt_smp[i, rho]     # Air density in kg/m³
    ps  <- dt_smp[i, ps]      # Air pressure in pascals
    
    # Runway inputs
    rwy   <- dt_smp[i, rwy]   # Active runway (based on the prevailing wind at the time of the observation)
    toda  <- dt_smp[i, toda]  # Takeoff distance available in meters
    theta <- 0                # Runway slope (theta) in degrees
    
    ############################################################################
    # Calculate the takeoff speeds in m/s (groundspeed and true airspeed)      #
    ############################################################################
    
    V <- fn_takeoff_speed(W, S, CL, hdw, rho, int)
    
    ############################################################################
    # Calculate the lift force in N at each airspeed increment                 #
    ############################################################################
    
    L <- fn_takeoff_lift(rho, V$tas, S, CL)
    
    ############################################################################
    # Calculate the lift force in N at each airspeed increment                 #
    ############################################################################
    
    D <- fn_takeoff_drag(rho, V$tas, S, CD)
    
    ############################################################################
    # Calculate the propulsive force in N at each airspeed increment           #
    ############################################################################
    
    F <- fn_takeoff_thrust(n, bpr, slst, ps, tas, V$tas)
    
    ############################################################################
    # Calculate the dynamic pressure at each airspeed increment                #
    ############################################################################
    
    q <- fn_dynamic_pressure(rho, V$tas)
    
    ############################################################################
    # Calculate the acceleration of the aircraft at each airspeed increment    #
    ############################################################################

    # Calculate the takeoff distance required (TODR), incrementing the thrust and decreasing the weight iteratively as needed
    repeat {

      # Calculate the acceleration of the aircraft along the runway in meters per second squared
      a <- g / W * (F$rto * 2 - (mu * W) - (CD - (mu * CL)) * (q * S) - (W * sin(theta)))
      
      # Calculate the average acceleration (i.e., midpoint acceleration) between two groundspeed increments
      a_avg <- frollmean(x = a, n = 2, fill = head(a, 1), align = "right")
      
      # Calculate the average groundspeed between two groundspeed increments
      Vgnd_avg <- frollmean(x = V$gnd, n = 2, fill = 0, align = "right")
      
      # Calculate the groundspeed difference between two groundspeed increments
      Vgnd_inc <- V$gnd - shift(x = V$gnd, n = 1, fill = 0, type = "lag")
      
      # Calculate the distance in meters covered between two groundspeed increments
      dis <- Vgnd_avg * Vgnd_inc / a_avg
      
      # Increment the cumulative (running total) distance accordingly
      cum <- cumsum(x = dis)
      
      # Calculate the regulatory takeoff distance (115% of the running total)
      todr <- max(cum) * 1.15
      
      # For as long as the regulatory takeoff distance required (TODR) exceeds the takeoff distance available (TODA),
      if (todr > toda) {
        
        # For as long as the reduced initial takeoff thrust is below the maximum initial thrust
        if (head(F$rto, 1) < head(F$max, 1)) {
          
          # We increase the takeoff thrust by one percent at a time
          F$rto <- F$rto + F$max * .01
          
        # However, if the takeoff thrust is already at its maximum
        } else {
        
          # We decrease the takeoff mass instead, by one kilogram at a time
          # m <- m - 1
          m <- (240000 / 2.205) - 1
          
          # And calculate the takeoff weight again
          W <- m * g
        }
        
      # However, if the TODR is below or at TODA,
      } else {
        
        # We exit the repeat loop
        break;
        
      }
      
    }
    
    ############################################################################
    # Return results to the log file (for calibration, testing, and debugging) #
    # This section can safely be commented out to speed up script execution    #
    ############################################################################
    
    # Assemble the simulation steps into a data table
    dt_out <- data.table(
      Vgnd     = round(V$gnd, digits = 1),    # Groundspeed in meters per second
      Vtas     = round(V$tas, digits = 1),    # Airspeed in meters per second
      L        = round(L, digits = 1),        # Lift in newtons
      W        = round(W, digits = 1),        # Weight in newtons
      D        = round(D, digits = 1),        # Drag in newtons
      q        = round(q, digits = 1),        # Dynamic pressure
      a        = round(a, digits = 1),        # Acceleration in meters per second squared
      a_avg    = round(a_avg, digits = 1),    # Average acceleration between the two speed increments
      Vgnd_avg = round(Vgnd_avg, digits = 1), # Average speed between the two speed increments in meters per second
      Vgnd_inc = round(Vgnd_inc, digits = 1), # Speed increment in meters per second
      dis      = round(dis, digits = 1),      # Distance in meters covered between the two speed increments
      cum      = round(cum, digits = 1)       # Cumulative distance in meters covered up to this increment
    )
    
    # Display in the log file
    width <- 150
    options(width = width)
    print(paste(rep("#", width), collapse = ""))
    print(paste(aircraft, "at", apt, "departing from", rwy, "on", obs, "under", exp, "with", round(hdw, digits = 1), "m/s headwind, an OAT of", tas, "kelvins, and a QNH of", ps / 100, "hPa:", sep = " "))
    print(dt_out)
    
    ############################################################################
    # Write the the takeoff performance calculations to the database           #
    ############################################################################
    
    # Assemble the results into a data table
    dt_res <- data.table(
      obs   = obs,                                              # Date and time of the observation
      icao  = apt,                                              # Airport ICAO code
      exp   = exp,                                              # Climate experiment (SSP)
      rwy   = rwy,                                              # Active runway at the time of the observation
      act   = aircraft,                                         # Aircraft type
      # T_inc = (F$max - F$rto) / F$max,                        # Percentage of thrust reduction from Tmax
      m_dec = round(dt_act[act == aircraft, m] - m, digits = 0) # Kilograms of payload that had to be removed from the aircraft
    )
    
    # Append the results to the list initialized earlier
    tko_list[[i]] <- dt_res

  } # End of the loop
  
  # Combine the outputs of all takeoff simulations at the current airport into a data table
  dt_tko <- rbindlist(tko_list)
  
  # Write the data
  # Here we use the deprecated RMySQL::MySQL() driver instead of the newer RMariaDB::MariaDB()) driver because it was found to be ~2.8 times faster here
  # dbWriteTable(conn = db_con, name = tolower(db_tko), value = dt_tko, append = TRUE, row.names = FALSE)
  
  # Disconnect the worker from the database
  dbDisconnect(db_con)
    
} # End of the fn_takeoff function

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################
  
  ##############################################################################
  # Prepare the cluster                                                        #
  ##############################################################################
  
  # Set the number of workers to use in the cluster
  cores <- 12
  
  # Set the log file for the cluster
  outfile <- log_6
  
  # Clear the log file
  close(file(outfile, open = "w"))
  
  # Build the cluster of workers
  cl <- makeCluster(cores, outfile = outfile)
  
  # Have each worker load the libraries that they need to handle the fn_import function defined above
  clusterEvalQ(cl, {
    library(data.table)
    library(DBI)
  })
  
  # Pass the required variables from the main scope to the workers' scope
  clusterExport(cl, c("db_cnf", "db_grp", "db_imp", "db_cli", "db_tko", "dt_act", "mu", "int"))
  
  ##############################################################################
  # Remove the airports that were already processed from the sample            #
  # This allows for incremental adds to the table over several script runs     #
  ##############################################################################
  
  # Connect to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to check if the takeoff table already exists in the database
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