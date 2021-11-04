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
library(ggplot2)
library(parallel)
library(zoo)

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
  df_smp <- suppressWarnings(dbFetch(db_res, n = Inf))
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect from the database
  dbDisconnect(db_con)
  
  ##############################################################################
  # Import the aeronautical data, including, for all aircraft to be modeled:   #
  #  model: Aircraft model (e.g., A320, B737)                                  #
  # engine: Engine model (e.g., CFM56-5B4, Trent 1000-E2)                      #
  #   Tmax: Maximum static sea-level thrust at takeoff in newtons              #
  #   MTOW: Maximum takeoff mass in kilograms                                  #
  #      S: Total wing area in square meters                                   #
  #     CL: Dimensionless coefficient of lift in takeoff configuration         #
  #     CD: Dimensionless coefficient of drag in takeoff configuration         #
  # Data were sourced from Sun et al. (2018, 2020)                             #
  ##############################################################################
  
  dt_act <- data.table(
    model  = c("A319", "A320", "A321", "A332", "A333", "A343", "A359", "A388", "B734", "B737", "B738", "B739", "B744", "B748", "B772", "B773", "B77w", "B788", "B789"),
    engine = c("CFM56-5B7", "CFM56-5B4", "CFM56-5B1", "Trent 772", "Trent 772", "CFM56-5C3", "Trent XWB-84", "GP7270", "CFM56-3B-2", "CFM56-7B26", "CFM56-7B26", "CFM56-7B26", "RB211-524G", "GEnx-2B67", "PW4090", "Trent 892", "GE90-115B", "Trent 1000-E2", "Trent 1000-K2"),
    Tmax   = c(120100, 117900, 133450, 320300, 320300, 144570, 379000, 332390, 98300, 116990, 116990, 116990, 253000, 299800, 408300, 411480, 513900, 268000, 350900),
    MTOW   = c(75500, 78000, 93500, 230000, 242000, 276000, 280000, 560000, 68000, 70000, 79000, 85100, 396800, 447700, 297000, 299300, 351500, 228000, 254000),
    S      = c(124, 124, 128, 361.6, 361.6, 363.1, 442, 845, 91.4, 124.6, 124.6, 124.6, 525.6, 554, 427.8, 427.8, 436.8, 377, 377),
    CL     = rep(1.6087, 19),
    CD     = c(0.074, 0.078, 0.091, 0.074, 0.075, 0.075, 0.075, 0.059, 0.081, 0.077, 0.076, 0.08, 0.076, 0.079, 0.083, 0, 0.085, 0.071, 0.074)
  )
  
################################################################################
# Define a function to simulate takeoffs at each airport                       #
################################################################################
  
fn_takeoff <- function(apt) {
  
  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), " Worker ", Sys.getpid(), " is parsing airport ", apt, "...", sep = ""))
  
  ##############################################################################
  # Import the takeoff conditions at the current airport                       #
  ##############################################################################
  
  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to retrieve the takeoff conditions
  db_qry <- paste("SELECT id, obs, exp, tas, ps, rho, wnd_hdw, rwy, toda FROM ", db_cli, " WHERE icao = '", apt, "' LIMIT 1;", sep = "")
  
  # Send the query to the database
  db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))
  
  # Return the results
  dt_smp <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "id", check.names = FALSE))
  
  # Release the database resource
  dbClearResult(db_res)

  ##############################################################################
  # Define airport-level simulation inputs                                     #
  ##############################################################################
  
  # Natural and unit conversion inputs
  g <- 9.806665          # Gravitational acceleration in meters per second squared, assuming a non-oblate, non-rotating Earth (Blake, 2009; Daidzic, 2016)
  
  # Function to convert feet to meters
  fn_f_to_m <- function(ft) {
    ft / 3.280839895
  }
  
  # Aircraft inputs
  act <- "A320"                         # Aircraft model
  CL  <- dt_aer[aircraft == act, CL]    # Dimensionless coefficient of lift in takeoff (non-clean) configuration
  CD  <- dt_aer[aircraft == act, CD]    # Dimensionless coefficient of drag in takeoff (non-clean) configuration
  m   <- dt_aer[aircraft == act, MTOW]  # Aircraft mass in kilograms
  m   <- 240000 / 2.205                 # FOR TESTING ONLY
  S   <- dt_aer[aircraft == act, S]     # Wing surface area in square meters
  W   <- m * g                          # Aircraft weight in newtons
  
  # Engine inputs
  Tmax <- dt_aer[aircraft == act, Tmax] # Maximum sea-level static thrust under ISA conditions
  
  # Dimensionless coefficients of rolling friction. Values below are taken from Filippone (2012), Table 9.3 unless mentioned otherwise
  mu_array <- c(
    "blake" = .0165, # Value used by Blake (2009), p. 18-11
    "dca"   = .02,   # Value for dry concrete/asphalt. Recommended as typical by ESDU 85029 (p. 32)
    "htg"   = .04,   # Value for hard turf and gravel
    "sdg"   = .05,   # Value for short and dry grass
    "lg"    = .10,   # Value for long grass
    "sg_lo" = .10,   # Value for soft ground (low softness)
    "sg_mi" = .20,   # Value for soft ground (medium softness)
    "sg_hi" = .30    # Value for soft ground (high softness)
  )
  
  # Choose which coefficient of rolling friction to use in the simulation
  mu <- mu_array["blake"]
  
  # Simulation settings
  int <- 10 # Simulation resolution / number of integration steps (i.e., number of speed increments between zero and minimum takeoff speed)

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
    exp <- dt_smp[i, exp]               # Climate experiment (SSP)
    hdw <- dt_smp[i, wnd_hdw]           # Headwind in meters per second at the active runway
    oat <- dt_smp[i, tas]               # Outside air temperature in kelvins
    obs <- dt_smp[i, obs]               # Date and time
    rho <- dt_smp[i, rho]               # Air density in kilograms per cubic meter
    ps  <- dt_smp[i, ps]                # Air pressure in pascals
    
    # Runway inputs
    rwy   <- dt_smp[i, rwy]             # Active runway (based on the prevailing wind at the time of the observation)
    toda  <- fn_f_to_m(dt_smp[i, toda]) # Takeoff distance available converted from feet to meters
    theta <- 0                          # Runway slope (theta) in degrees
    
    ############################################################################
    # Calculate the speed at which lift L is equal to weight W                 #
    ############################################################################
    
    # Calculate minimum takeoff speed and add a 1.2 safety margin factor
    V2min <- sqrt( W / (.5 * S * rho * CL) ) * 1.2

    # Define groundspeed increments. Headwind decreases the terminal groundspeed
    Vgnd <- seq(from = 0, to = V2min - hdw, length = int)
    
    # Define airspeed increments. Headwind increases the starting airspeed
    Vtas <- seq(from = 0 + hdw, to = V2min, length = int)
    
    ############################################################################
    # Simulate the aerodynamic forces (lift and drag) at every V               #
    ############################################################################
    
    # Calculate the lift force in newtons at each airspeed increment
    L <- .5 * rho * Vtas^2 * S * CL

    # Calculate the drag force in newtons at each airspeed increment
    D <- .5 * rho * Vtas^2 * S * CD
    
    ############################################################################
    # Simulate the propulsive force (thrust) at every V                        #
    ############################################################################
    
    # NEED TO CALCULATE THE AIRPORT'S GEOPOTENTIAL PRESSURE ALTITUDE HERE
    Hp <- 0
    
    # Set reduced takeoff operations (takeoff at no less than 75% of TOGA thrust)
    Trto <- Tmax  * .75
    
    ############################################################################
    # Simulate the acceleration of the aircraft along the runway               #
    ############################################################################
    
    # Print the current aircraft mass
    # print(paste("Starting thrust is", Trto, "newtons.", sep = " "))
    # print(paste("Starting mass is", m, "kilograms.", sep = " "))
    
    # Calculate the dynamic pressure at each airspeed increment
    q <- (rho * Vtas^2) / 2
        
    # Calculate the takeoff distance required (TODR), incrementing the thrust and decreasing the weight iteratively as needed
    repeat {

      # Calculate the acceleration of the aircraft along the runway in meters per second squared
      a <- g / W * (Trto * 2 - (mu * W) - (CD - (mu * CL)) * (q * S) - (W * sin(theta)))
      
      # Calculate the average acceleration (i.e., midpoint acceleration) between two groundspeed increments
      a_avg <- frollmean(x = a, n = 2, fill = head(a, 1), align = "right")
      
      # Calculate the average groundspeed between two groundspeed increments
      Vgnd_avg <- frollmean(x = Vgnd, n = 2, fill = 0, align = "right")
      
      # Calculate the groundspeed difference between two groundspeed increments
      Vgnd_inc <- Vgnd - shift(x = Vgnd, n = 1, fill = 0, type = "lag")
      
      # Calculate the distance in meters covered between two groundspeed increments
      dis <- Vgnd_avg * Vgnd_inc / a_avg
      
      # Increment the cumulative (running total) distance accordingly
      cum <- cumsum(x = dis)
      
      # Calculate the regulatory takeoff distance (115% of the running total)
      todr <- max(cum) * 1.15
      
      # For as long as the regulatory takeoff distance required (TODR) exceeds the takeoff distance available (TODA),
      if (todr > toda) {
        
        # For as long as the current takeoff thrust is below maximum thrust
        if (Trto < Tmax) {
          
          # We increase the takeoff thrust by one percent at a time
          Trto <- Trto + Tmax * .01
          
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
      Vgnd     = Vgnd,     # Groundspeed in meters per second
      Vtas     = Vtas,     # Airspeed in meters per second
      L        = L,        # Lift in newtons
      W        = W,        # Weight in newtons
      D        = D,        # Drag in newtons
      q        = q,        # Dynamic pressure
      a        = a,        # Acceleration in meters per second squared
      a_avg    = a_avg,    # Average acceleration between the two speed increments
      Vgnd_avg = Vgnd_avg, # Average speed between the two speed increments in meters per second
      Vgnd_inc = Vgnd_inc, # Speed increment in meters per second
      dis      = dis,      # Distance in meters covered between the two speed increments
      cum      = cum       # Cumulative distance in meters covered up to this increment
    )
    
    # Display in the log file
    width <- 150
    options(width = width)
    print(paste(rep("#", width), collapse = ""))
    print(paste(act, "at", apt, "departing from", rwy, "on", obs, "under", exp, "with", round(hdw, digits = 1), "m/s headwind, an OAT of", oat - 273.15, "degrees C, and a QNH of", ps / 100, "hPa:", sep = " "))
    print(dt_out)
    
    ############################################################################
    # Write the the takeoff performance calculations to the database           #
    ############################################################################
    
    # Assemble the results into a data table
    dt_res <- data.table(
      obs   = obs,                                                 # Date and time of the observation
      icao  = apt,                                                 # Airport ICAO code
      exp   = exp,                                                 # Climate experiment (SSP)
      rwy   = rwy,                                                 # Active runway at the time of the observation
      act   = act,                                                 # Aircraft type
      T_inc = (Tmax - Trto) / Tmax,                                # Percentage of thrust reduction from Tmax
      m_dec = round(dt_aer[aircraft == act, MTOW] - m, digits = 0) # Kilograms of payload that had to be removed from the aircraft
    )
    
    # Append the results to the list initialized earlier
    tko_list[[i]] <- dt_res

  } # End of the loop
  
  # Combine the outputs of all takeoff simulations at the current airport into a data table
  dt_tko <- rbindlist(tko_list)
  
  # Write the data
  # Here we use the deprecated RMySQL::MySQL() driver instead of the newer RMariaDB::MariaDB()) driver because it was found to be ~2.8 times faster here
  dbWriteTable(conn = db_con, name = tolower(db_tko), value = dt_tko, append = TRUE, row.names = FALSE)
  
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
  clusterExport(cl, c("db_cnf", "db_grp", "db_imp", "db_cli", "db_tko", "dt_act"))
  
  ##############################################################################
  # Import the airports that were already processed so we can skip them        #
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
    df_smp <- subset(df_smp, !(icao %in% df_pro$icao))
    print(paste(nrow(df_pro), "airports exist in the database table", db_tko,"and will be ommitted from the simulation.", nrow(df_smp), "airports remain.", sep = " "))
    
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