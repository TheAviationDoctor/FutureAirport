################################################################################
#    NAME: scripts/6_takeoff.R                                                 #
#   INPUT: 442,769,456 rows from database table cli created in 5_transform.R   #
# ACTIONS: Estimate relationship between thrust and airspeed from Blake (2009) #
#          Calculate the takeoff distance required (TODR)                      #
#          Write the resulting TODR to the database table cli                  #
#  OUTPUT: 442,769,456 rows of database table cli updated with one new column  #
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
# Import the supporting data                                                   #
################################################################################

  ##############################################################################
  # Import the runway data                                                     #
  ##############################################################################
  
  # Connect to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build the query to retrieve the sample airports
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
  #  Max takeoff thrust Tmax in N from Sun et al. (2020)                       #
  #  Max takeoff weight MTOW in kg from Sun et al. (2020)                      #
  #  Wing area S in square meters from Sun et al. (2020)                       #
  #  Dimensionless coefficient of lift CL - not sourced yet                    #
  #  Dimensionless coefficient of drag CD as per Sun et al. (2018)             #
  #  Stall speed Vs in knots in takeoff configuration - not sourced yet        #
  #  1st, 2nd, and 3rd coefficients of maximum thrust at takeoff - not sourced #
  ##############################################################################
  
  # Load the aircraft and engine data from a CSV file
  dt_aer <- fread(file = paste(path_aer, aer_act, sep = "/"), header = TRUE, colClasses = c("character", "factor", "integer", "integer", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

################################################################################
# Define a function to simulate takeoffs at each airport                       #
################################################################################
  
fn_takeoff <- function(apt) {
  
  ##############################################################################
  # Retrieve the takeoff conditions at the current airport                     #
  ##############################################################################
  
  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build the query to retrieve the takeoff conditions
  db_qry <- paste("SELECT id, obs, exp, tas, ps, rho, wnd_hdw, rwy, toda FROM ", db_cli, " WHERE icao = '", apt, "' LIMIT 1;", sep = "")
  
  # Send the query to the database
  db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))
  
  # Return the results
  dt_smp <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "id", check.names = FALSE))
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect the worker from the database
  dbDisconnect(db_con)
  
  ##############################################################################
  # Define airport-level simulation inputs                                     #
  ##############################################################################
  
  # Natural inputs
  g <- 9.806665 # Gravitational acceleration in meters per second squared, assuming a non-oblate, non-rotating Earth (Blake, 2009; Daidzic, 2016)
  
  # Aircraft inputs
  act <- "A320"                         # Aircraft model
  CL  <- dt_aer[aircraft == act, CL]    # Dimensionless coefficient of lift in takeoff (non-clean) configuration
  CD  <- dt_aer[aircraft == act, CD]    # Dimensionless coefficient of drag in takeoff (non-clean) configuration
  # m   <- dt_aer[aircraft == act, MTOW]  # Aircraft mass in kilograms
  m   <- 68000  # Aircraft mass in kilograms
  S   <- dt_aer[aircraft == act, S]     # Wing surface area in square meters
  W   <- m * g                          # Aircraft weight in newtons
  
  # Engine inputs
  Tmax <- dt_aer[aircraft == act, Tmax] # Maximum sea-level static thrust under ISA conditions
  CT1  <- dt_aer[aircraft == act, CT1]  # First coefficient of maximum takeoff thrust
  CT2  <- dt_aer[aircraft == act, CT2]  # Second coefficient of maximum takeoff thrust
  CT3  <- dt_aer[aircraft == act, CT2]  # Third coefficient of maximum takeoff thrust
  
  # Airport inputs
  mu <- mu["blake"]                     # Coefficient of rolling friction
  
  # Simulation settings
  int <- 30                             # Resolution / number of integration steps (i.e., number of speed increments between zero and minimum takeoff speed)
  
  ##############################################################################
  # Define airport-level simulation outputs                                    #
  ##############################################################################
  
  # Initialize a data table to hold the results for the current airport
  dt_apt <- data.table()
  
  ##############################################################################
  # For each climate observation at the current airport                        #
  ##############################################################################
  
  for (i in 1:nrow(dt_smp)) {
    
    ############################################################################
    # Define observation-level simulation inputs                               #
    ############################################################################
      
    # Climate inputs
    exp <- dt_smp[i, exp]          # Climate experiment (SSP)
    hdw <- dt_smp[i, wnd_hdw]      # Headwind in meters per second at the active runway
    oat <- dt_smp[i, tas] - 273.15 # Outside air temperature converted from kelvins to degrees Celsius
    obs <- dt_smp[i, obs]          # Date and time
    rho <- dt_smp[i, rho]          # Air density in kilograms per cubic meter
    ps  <- dt_smp[i, ps]           # Air pressure in pascals
    
    # Runway inputs
    rwy   <- dt_smp[i, rwy]        # Active runway (based on the prevailing wind at the time of the observation)
    theta <- 0                     # Runway slope (theta) in degrees

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
    
    # Calculate the dynamic pressure at each airspeed increment
    q <- (rho * Vtas^2) / 2
    
    # Acceleration in meters per second squared
    a <- g / W * (Trto * 2 - (mu * W) - (CD - (mu * CL)) * (q * S) - (W * sin(theta)))
    
    ############################################################################
    # Step integration of distance as per Table 18-2 of Blake (2019)           #
    ############################################################################
    
    # Calculate the average acceleration (i.e., midpoint acceleration)
    a_avg <- frollmean(x = a, n = 2, fill = head(a, 1), align = "right")
    
    # Calculate the average speed between two speed increments
    Vgnd_avg <- frollmean(x = Vgnd, n = 2, fill = 0, align = "right")
    
    # Calculate the difference between two speed increments
    Vgnd_inc <- Vgnd - shift(x = Vgnd, n = 1, fill = 0, type = "lag")
    
    # Calculate the distance covered in each increment
    dis <- Vgnd_avg * Vgnd_inc / a_avg
    
    # Increment the cumulative (total) distance by that incremental distance
    cum <- cumsum(x = dis)
    
    ############################################################################
    # Return results                                                           #
    ############################################################################
    
    # Assemble the results into a data table
    dt_obs <- data.table(
      icao     = rep(apt, int),
      obs      = rep(obs, int),
      exp      = rep(exp, int),
      rwy      = rep(rwy, int),
      hdw      = rep(hdw, int),
      ps       = rep(ps, int),
      oat      = rep(oat, int),
      act      = rep(act, int),
      Tmax     = rep(Tmax, int),
      Trto     = rep(Trto, int),
      CL       = rep(CL, int),
      CD       = rep(CD, int),
      V2min    = rep(V2min, int),
      Vgnd     = Vgnd,
      Vtas     = Vtas,
      L        = L,
      W        = W,
      D        = D,
      q        = q,
      a        = a,
      a_avg    = a_avg,
      Vgnd_avg = Vgnd_avg,
      Vgnd_inc = Vgnd_inc,
      dis      = dis,
      cum      = cum
    )
    
    # Bind the per-observation results to the per-airport data table
    
    # Display it in the log file
    width <- 240
    options(width = width)
    print(paste(rep("#", width), collapse = ""))
    print(dt_obs)
    print(paste(rep("#", width), collapse = ""))
    
  } # End of the loop
  
} # End of the fn_takeoff function

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################

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
clusterExport(cl, c("db_cnf", "db_grp", "db_imp", "db_cli", "dt_aer", "mu"))

# LIMIT NUMBER OF AIRPORTS FOR TESTING ONLY - REMOVE BEFORE GO-LIVE
df_smp <- head(df_smp, 1)

# Distribute the unique airports across the workers
parLapply(cl, df_smp$icao, fn_takeoff)

# Terminate the cluster once finished
stopCluster(cl)

################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF