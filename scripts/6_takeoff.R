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
# Define simulation functions                                                  #
################################################################################
  
  ##############################################################################
  # Define a function to convert degrees Kelvin to Celsius                     #
  ##############################################################################
  
  fn_k_to_c <- function(k) {
    k - 273.15
  }
  
  ##############################################################################
  # Define a function to convert meters per second to knots (intl. naut. mph.) #
  ##############################################################################
  
  fn_ms_to_kt <- function(ms) {
    ms * 1.9438445
  }
  
  ##############################################################################
  # Define a function to convert knots to meters per second                    #
  ##############################################################################
  
  fn_kt_to_ms <- function(kt) {
    kt * 0.5144442
  }
  
  ##############################################################################
  # Define a function to calculate the maximum takeoff thrust                  #
  # CT1, CT2, and CT3 are coefficients of max thrust as per BADA3              #
  # HP is the geopotential pressure altitude                                   #
  # As per the method used in Gratton et al. (2020) & proposed by Nuic (2013)  #
  ##############################################################################
  
  fn_thrust <- function(act, Hp) {
    dt_aer[aircraft == act, CT1] * ( 1 - ( Hp / dt_aer[aircraft == act, CT2] ) + ( dt_aer[aircraft == act, CT3] * Hp^2 ) )
  }
  
  ##############################################################################
  # Define a function to calculate the lift force in newtons for a given speed #
  ##############################################################################
  
  fn_lift <- function(act, rho, Vtas) {
    .5 * rho * Vtas^2 * dt_aer[aircraft == act, S] * dt_aer[aircraft == act, CL]
  }
  
  ##############################################################################
  # Define a function to calculate the drag force in newtons for a given speed #
  ##############################################################################
  
  fn_drag <- function(act, rho, Vtas) {
    .5 * rho * Vtas^2 * dt_aer[aircraft == act, S] * dt_aer[aircraft == act, CD]
  }
  
  ##############################################################################
  # Define a function to approximate the minimum takeoff speed                 #
  # V2min is the minimum takeoff safety speed in takeoff configuration         #
  # Vs is the stall speed in takeoff configuration                             #
  ##############################################################################
  
  fn_speed <- function(act) {
    dt_aer[aircraft == act, Vs] * 1.2
  }
  
################################################################################
# Define a function to simulate takeoffs                                       #
################################################################################
  
fn_takeoff <- function(apt) {
  
  ##############################################################################
  # Retrieve the takeoff conditions at the current airport                     #
  ##############################################################################
  
  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build the query to retrieve the takeoff conditions
  db_qry <- paste("SELECT id, obs, exp, tas, rho, wnd_hdw, rwy, toda FROM ", db_cli, " WHERE icao = '", apt, "' LIMIT 1;", sep = "")
  
  # Send the query to the database
  db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))
  
  # Return the results
  dt_smp <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "id", check.names = FALSE))
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect the worker from the database
  dbDisconnect(db_con)
  
  ##############################################################################
  # For each climate observation at the airport                                #
  ##############################################################################
  
  for (i in 1:nrow(dt_smp)) {
    
    ############################################################################
    # Define simulation inputs                                                 #
    ############################################################################
    
      ##########################################################################
      # Aircraft inputs                                                        #
      ##########################################################################
      
      act <- "A320"                          # Aircraft model
      int <- 10                              # Set integration steps (i.e., number of speed increments between zero and minimum takeoff speed)
      
      ##########################################################################
      # Climate inputs                                                         #
      ##########################################################################
      
      exp <- dt_smp[i, exp]                  # Climate experiment (SSP)
      hdw <- fn_ms_to_kt(dt_smp[i, wnd_hdw]) # Headwind converted to knots
      oat <- fn_k_to_c(dt_smp[i, tas])       # Outside air temperature converted to degrees Celsius
      obs <- dt_smp[i, obs]                  # Date and time
      rho <- dt_smp[i, rho]                  # Air density in kilograms per cubic meter
      
      ##########################################################################
      # Runway inputs                                                          #
      ##########################################################################
      
      mu  <- mu["blake"]                     # Coefficient of rolling friction
      rwy <- dt_smp[i, rwy]                  # Active runway (based on the prevailing wind at the time of the observation)
      slp <- 0                               # Runway slope (theta) in degrees
      
    ############################################################################
    # Define groundspeed and airspeed increments for the step integration      #
    ############################################################################
    
    # Approximate the minimum takeoff true airspeed in takeoff configuration
    V2min <- fn_speed(act)
    
    # Define groundspeed increments in knots from zero to the minimum takeoff true airspeed in takeoff configuration
    # If there is headwind, the maximum groundspeed we need to achieve is decreased
    Vgnd <- seq(from = 0, to = V2min - hdw, length = int)
    
    # Define airspeed increments in knots from zero to the minimum takeoff true airspeed in takeoff configuration
    # If there is headwind, the minimum airspeed when the aircraft is idle is nonzero
    Vtas <- seq(from = 0 + hdw, to = V2min, length = int)
    
    ############################################################################
    # Simulate the aerodynamic forces (lift and drag)                          #
    ############################################################################
    
    # Calculate the lift force in newtons at each true airspeed increment
    L <- fn_lift(act, rho, fn_kt_to_ms(Vtas))
    
    # Calculate the drag force in newtons at each true airspeed increment
    D <- fn_drag(act, rho, fn_kt_to_ms(Vtas))
    
    # Calculate the dynamic pressure at each true airspeed increment
    q <- (rho * fn_kt_to_ms(Vtas)^2) / 2
    
    ############################################################################
    # Simulate the propulsive force (thrust) at every V                        #
    ############################################################################
    
    # NEED TO CALCULATE THE AIRPORT'S GEOPOTENTIAL PRESSURE ALTITUDE HERE
    Hp <- 0
    
    # Calculate maximum possible thrust (i.e., TOGA thrust) at the start of the takeoff
    Tmax <- fn_thrust(act, Hp)
    
    # Simulate reduced takeoff operations (takeoff at no less than 75% of TOGA thrust)
    Trto <- Tmax * .75
    
    ############################################################################
    # Simulate the gravitational force (weight)                                #
    ############################################################################
    
    # # Gravitational acceleration in meters per second squared, assuming a non-oblate, non-rotating Earth (Blake, 2009; Daidzic, 2016)
    # g <- 9.806665
    # 
    # # Aircraft takeoff weight, expressed in kilograms rather than newtons for convenience
    # weight <- W[act]
    # 
    # # Aircraft takeoff weight in newtons
    # weight <- g * mass
    # 
    # # Mu times weight
    muW <- mu * dt_aer[aircraft == act, MTOW]
    
    ############################################################################
    # Return results                                                           #
    ############################################################################
    
    # Assemble a data frame and display it in the log file
    out <- data.frame("ICAO" = apt, "Observation" = obs, "Experiment" = exp, "Runway" = rwy, "Aircraft" = act, "Groundspeed" = round(Vgnd, digits = 2), "Airspeed" = round(Vtas, digits = 2), "DynamicPressure" = round(q, digits = 2), "Thrust" = round(Trto, digits = 2), "Slope" = slp, "muW" = round(muW, digits = 2), "Lift" = round(L, digits = 2), "Weight" = dt_aer[aircraft == act, MTOW], row.names = NULL)
    width <- 200
    options(width = width)
    print(paste(rep("#", width), collapse = ""))
    print(out)
    print(paste(rep("#", width), collapse = ""))
    
  } # End of the loop
    
    
    
  # # Calculate the thrust value for each V
  # for (j in 2:length(V)){
  #   T[j] <- T[j-1] + (V[j] - V[j-1]) * coef(model)[2]
  # }
  # 
  # # Dynamic pressure. The speed is converted to feet per second
  # q <- (dt_smp$rho[i] * (V * 1.68781)^2) / 2
  # 
  # 
  # g <- 32.174
  # mu <- .0165
  # CL <- 1.9805065
  # S <- 1951
  # 
  # # Acceleration in feet per second per second
  # a <- g / W * (T * 2 - (mu * W) - (CD - (mu * CL)) * (q * S) - (W * sin(slp)))
  # 
  # print(paste("GS:", (GS), sep = " "))
  # print(paste("V:", (V), sep = " "))
  # print(paste("T:", (T), sep = " "))
  # print(paste("q:", (q), sep = " "))
  # print(paste("a:", (a), sep = " "))
  #   
  # # For each groundspeed increment
  # for (j in 1:length(GS)) {
  # 
  #   # Calculate the average speed (i.e., midpoint speed), skipping the zero speed row
  #   avg <- ifelse(GS[j] == 0, GS[j], mean(c(GS[j - 1], GS[i])))
  # 
  #   # Calculate the speed increment (i.e., new speed minus previous speed), skipping the zero speed row
  #   inc <- ifelse(GS[j] == 0, GS[j], GS[j] - GS[j - 1])
  # 
  #   # Calculate the average acceleration (i.e., midpoint acceleration)
  #   acc <- mean(c(a[j - 1], a[j]))
  # 
  #   # Calculate the distance covered in that increment (converting speed from knots to feet/s)
  #   dis <- avg * inc * 1.6878^2 / acc
  # 
  #   # Increment the cumulative (total) distance by that incremental distance
  #   cum <- cum + dis
  # 
  #   # Save incremental results for later display to the console
  #   out1 <- rbind(out1, c(GS[j], V[j], round(a[j], digits = 1), round(dis, digits = 0), round(cum, digits = 0)))
  # 
  # }
  # 
  # # Output results to the console
  # print(out1)
  
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
clusterExport(cl, c("db_cnf", "db_grp", "db_imp", "db_cli", "dt_aer", "fn_drag", "fn_lift", "fn_k_to_c", "fn_kt_to_ms", "fn_ms_to_kt", "fn_speed", "fn_thrust", "mu"))

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