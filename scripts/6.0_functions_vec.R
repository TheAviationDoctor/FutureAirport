################################################################################
#    NAME: scripts/6.0_functions.R                                             #
#   INPUT: None                                                                #
# ACTIONS: Define functions used in the takeoff simulation                     #
#  OUTPUT: Set of global functions loaded into R's environment                 #
# RUNTIME: <1 second on the researcher's config (https://bit.ly/3ChCBAP)       #
################################################################################

################################################################################
# Index of functions in this file                                              #
#  HOUSEKEEPING FUNCTIONS                                                      #
#   - fn_imp_act = Import the aircraft data from the CSV file                  #
#   - fn_imp_cal = Import the calibration data from the CSV file               #
#   - fn_imp_apt = Import the simulation airports                              #
#   - fn_imp_obs = Import the simulation conditions                            #
#   - fn_set_tko = Set up the database table to store the takeoff calculations #
#  SIMULATION FUNCTIONS                                                        #
#   - fn_sim_wgt = Calculate the weight force W in N                           #
#   - fn_sim_vlo = Calculate the speed in m/s at which lift L equals weight W  #
#   - fn_sim_spd = Calculate the takeoff speeds V in m/s                       #
#   - fn_sim_lft = Calculate the lift force L in N                             #
#   - fn_sim_drg = Calculate the drag force D in N                             #
#   - fn_sim_thr = Calculate the propulsive force F in N                       #
#   - fn_sim_dyn = Calculate the dynamic pressure q in Pa                      #
#   - fn_sim_acc = Calculate the acceleration a in m/s²                        #
#   - fn_sim_dis = Calculate the horizontal takeoff distance in m              #
################################################################################

################################################################################
# HOUSEKEEPING FUNCTIONS                                                       #
################################################################################

##############################################################################
# Function fn_imp_act                                                        #
# Import the aircraft data from the CSV file                                 #
# Aircraft data is taken from Sun et al. (2020)                              #
##############################################################################

fn_imp_act <- function() {
  
  # Import the aircraft data from the CSV file into a data table
  dt_act <- fread(file = paste(path_aer, aer_act, sep = "/"), header = TRUE, colClasses = c("character", "character", "factor", "integer", "numeric", "integer", "integer", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
  
  # Filter for selected aircraft
  dt_act <- dt_act[type %in% sim$aircraft]
  
  # Remove rows with missing values, if any
  dt_act <- na.omit(dt_act)
  
  # Return the data table
  return(dt_act)
  
}

##############################################################################
# Function fn_imp_cal                                                        #
# Import the calibration data from the CSV file                              #
# Calibration data is taken from the respective OEM aircraft manuals         #
##############################################################################

fn_imp_cal <- function() {
  
  # Import the takeoff performance calibration data from the CSV file into a data table
  dt_cal <- fread(file = paste(path_aer, aer_cal, sep = "/"), header = TRUE, colClasses = c("factor", "numeric", "numeric"))
  
  # Filter for selected aircraft
  dt_cal <- dt_cal[type %in% sim$aircraft]
  
  # Remove rows that do not have complete corresponding aircraft data
  dt_cal <- dt_cal[type %in% dt_act[, type]]
  
}

##############################################################################
# Function fn_imp_apt                                                        #
# Import the simulation airports from the database                           #
##############################################################################

fn_imp_apt <- function() {
  
  # Connect to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  ############################################################################
  # Retrieve the ICAO codes of airports above the traffic threshold          #
  ############################################################################
  
  # Build a query to retrieve the sample airports above the traffic threshold
  db_qry <- paste("SELECT DISTINCT icao FROM ", db_pop, " WHERE traffic > ", pop_thr, ";", sep = "")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Return the results to a data table
  dt_smp <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "icao", check.names = FALSE))
  
  # Release the database resource
  dbClearResult(db_res)
  
  ##########################################################################
  # Exclude airports that were already processed by earlier simulations    #
  # This allows for incremental simulation runs                            #
  ##########################################################################
  
  # Build a query to check if the simulation outputs table already exists in the database
  db_qry <- paste("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = '", db_grp,"' AND table_name = '", db_tko,"';", sep = "")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Return the results to a data table
  tbl_exists <- suppressWarnings(as.logical(dbFetch(db_res, n = Inf)))
  
  # Release the database resource
  dbClearResult(db_res)
  
  # If the takeoff table already exists in the database
  if (tbl_exists == TRUE) {
    
    # Build a query to retrieve the airports that were already processed
    db_qry <- paste("SELECT DISTINCT icao FROM ", db_tko, ";", sep = "")
    
    # Send the query to the database
    db_res <- dbSendQuery(db_con, db_qry)
    
    # Return the results to a data table
    dt_exc <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "icao", check.names = FALSE))
    
    # Release the database resource
    dbClearResult(db_res)
    
    # Remove the airports already processed from the sample (inverse left outer join)
    dt_out <- dt_smp[!dt_exc]
    
  }
  
  # Disconnect from the database
  dbDisconnect(db_con)
  
  # Return the data table
  return(dt_out)
  
}

##############################################################################
# Function fn_imp_obs                                                        #
# Import the simulation conditions from the database based on:               #
#  apt  = airport's four-letter ICAO code                                    #
##############################################################################

fn_imp_obs <- function(apt, mode) {

  # Connect to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to retrieve the takeoff conditions
  db_qry <- paste("SELECT id, obs, exp, tas, ps, rho, wnd_hdw, rwy, toda FROM ", db_cli, " WHERE icao = '", apt, "' LIMIT 5;", sep = "")
  
  # Send the query to the database
  db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))
  
  # Return the results
  dt_obs <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "id", check.names = FALSE))
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect from the database
  dbDisconnect(db_con)
  
  # Return the results
  return(dt_obs)
  
}

##############################################################################
# Set up the database table to store the takeoff calculations                #
# Unlike in the other scripts, we don't drop the table if it exists already  #
# This allows the user to break up the script's execution over several runs  #
##############################################################################

fn_set_tko <- function() {
  
  # Connect to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to create the table
  db_qry <- paste("CREATE TABLE IF NOT EXISTS", tolower(db_tko), "(id INT UNSIGNED NOT NULL AUTO_INCREMENT, obs DATETIME NOT NULL, icao CHAR(4) NOT NULL, exp CHAR(6) NOT NULL, act CHAR(4) NOT NULL, j SMALLINT UNSIGNED NOT NULL, rto DECIMAL(3,2) NOT NULL, rem FLOAT NOT NULL, PRIMARY KEY (id));", sep = " ")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect from the database
  dbDisconnect(db_con)
  
}

################################################################################
# SIMULATION FUNCTIONS                                                         #
################################################################################

################################################################################
# Calculate the total (lift-independent + lift-induced) drag coefficient cD    #
# Adapted from Sun et al. (2020)                                               #
################################################################################

fn_sim_cd <- function(cL) {
  
  # Calculate the drag coefficient component attributable to flaps/slats in takeoff configuration
  delta_cD_flaps <- dt_act[, lambda_f] * dt_act[, cfc]^1.38 * dt_act[, SfS] * sin(sim$flap_angle * pi / 180)^2
  
  # Calculate the drag coefficient component attributable to the landing gear
  delta_cD_gear <- dt_act[, m] * sim$g / dt_act[, S] * 3.16E-5 * dt_act[, m]^-.215
  
  # Calculate the total drag coefficient in non-clean configuration
  cD0_total <- dt_act[, cD0] + delta_cD_flaps + delta_cD_gear
  
  # Calculate the Oswald efficiency factor for the selected flap deflection (for wing-mounted engines)
  delta_e_flaps <- .0026 * sim$flap_angle
  
  # Calculate the aspect ratio
  ar <- dt_act[, span]^2 / dt_act[, S]
  
  # Calculate the lift-induced coefficient k in non-clean configuration
  k_total <- 1 / (1 / dt_act[, k] + pi * ar * delta_e_flaps)
  
  # Calculate the drag coefficient in non-clean configuration
  cD <- cD0_total + k_total * cL^2
  
  # Return the drag coefficient
  return(cD)
  
}

##############################################################################
# Function fn_sim_wgt                                                        #
# Calculate the weight force W in N based on:                                #
#  m   = aircraft mass in kg                                                 #
##############################################################################

fn_sim_wgt <- function(m) {
  
  # Calculate the weight force in N
  W <- sim$g * m
  
  # Return the result
  return(W)
  
}

##############################################################################
# Function fn_sim_vlo                                                        #
# Calculate the speed in m/s at which lift L equals weight W based on:       #
#  W   = aircraft weight in N                                                #
#  rho = air density in kg/m³                                                #
#  S   = total wing surface area in takeoff configuration in m²              #
#  CL  = dimensionless coefficient of lift in takeoff configuration          #
# Adapted from Blake (2009)                                                  #
##############################################################################

fn_sim_vlo <- function(W, S, cL, rho) {
  
  # Calculate the speed in m/s at which lift L equals weight W
  Vlof <- sqrt( W / (.5 * rho * S * cL) )
  
  # Return the result
  return(Vlof)
  
}

##############################################################################
# Function fn_sim_spd                                                        #
# Calculate the takeoff speeds V in m/s based on:                            #
#  W    = aircraft weight in N                                               #
#  S    = total wing surface area in takeoff configuration in m²             #
#  CL   = dimensionless coefficient of lift in takeoff configuration         #
#  hdw  = headwind in m/s at the active runway                               #
#  Vlof = speed in m/s at which lift L equals weight W                       #
# Adapted from Blake (2009)                                                  #
##############################################################################

fn_sim_spd <- function(hdw, Vlof) {
  
  # Create a function to vectorize the default seq() base function
  seq_vec <- Vectorize(seq.default, vectorize.args = c("from", "to"))
  
  # Create airspeed intervals up to the minimum takeoff airspeed
  # Vtas <- seq(from = 0 + hdw, to = Vlof, length = sim$int)
  dt_out <- data.table(
    Vgnd = "numeric",
    Vtas = "numeric",
    q    = "numeric",
    L    = "numeric",
    D    = "numeric",
    Fmax = "numeric",
    Frto = "numeric",
    a    = "numeric"
  )
  
  dt_out[, Vtas := seq_vec(from = 0 + hdw, to = Vlof, length = sim$int)]
  # Vtas <- seq_vec(from = 0 + hdw, to = Vlof, length = sim$int)
  # print(Vtas)
  
  # Create groundspeed intervals up to the minimum takeoff airspeed
  # Vgnd <- seq(from = 0, to = Vlof - hdw, length = sim$int)
  Vgnd <- seq_vec(from = 0, to = Vlof - hdw, length = sim$int)
  
  V <- list("tas" = Vtas, "gnd" = Vgnd)
  
  # Return the results
  return(V)
  
}

##############################################################################
# Function fn_sim_dyn                                                        #
# Calculate the dynamic pressure q in Pa based on:                           #
#  rho  = air density in kg/m³                                               #
#  Vtas = true airspeed in m/s                                               #
# Adapted from Blake (2009)                                                  #
##############################################################################

fn_sim_dyn <- function(rho, Vtas) {
  
  # Calculate the dynamic pressure
  q <- .5 * rho * Vtas^2

  # Return the results
  return(q)
  
}

##############################################################################
# Function fn_sim_lft                                                        #
# Calculate the lift force L in N based on:                                  #
#  rho  = air density in kg/m³                                               #
#  Vtas = true airspeed in m/s                                               #
#  S    = total wing surface area in takeoff configuration in m²             #
#  CL   = Dimensionless coefficient of lift in takeoff configuration         #
##############################################################################

fn_sim_lft <- function(q, S, CL) {
  
  # Calculate the lift force in N
  L <- q * S * CL
  
  # Return the results
  return(L)
  
}

##############################################################################
# Function fn_sim_drg                                                        #
# Calculate the drag force D in N based on:                                  #
#  rho  = air density in kg/m³                                               #
#  Vtas = true airspeed in m/s                                               #
#  S    = total wing surface area in takeoff configuration in m²             #
#  CD   = Dimensionless coefficient of drag in takeoff configuration         #
##############################################################################

fn_sim_drg <- function(q, S, CD) {
  
  # Calculate the drag force in N
  D <- q * S * CD
  
  # Return the results
  return(D)
  
}

##############################################################################
# Function fn_sim_thr                                                        #
# Calculate the propulsive force F in N based on:                            #
#  n    = engine count in units                                              #
#  bpr  = engine bypass ratio                                                #
#  slst = engine sea-level static maximum thrust in N (per engine)           #
# Adapted from Sun et al. (2020)                                             #
##############################################################################

fn_sim_thr <- function(n, bpr, slst, ps, tas, Vtas) {
  
  # Calculate the speed of sound in m/s for the given temperature
  # TODO: model it for humid air using http://resource.npl.co.uk/acoustics/techguides/speedair/
  Vsnd <- sqrt(sim$gamma * sim$Rd * tas)
  
  # Calculate the mach number
  Vmach <- Vtas / Vsnd
  
  # Calculate the air pressure ratio
  dP = ps / sim$ps_isa
  
  # Calculate the coefficients of thrust
  G0 <- 0.0606 * bpr + 0.6337
  A <- -0.4327 * dP^2 + 1.3855 * dP + 0.0472
  Z <- 0.9106 * dP^3 - 1.7736 * dP^2 + 1.8697 * dP
  X <- 0.1377 * dP^3 - 0.4374 * dP^2 + 1.3003 * dP
  
  # Calculate the thrust ratio
  thrust_ratio <- A - 0.377 * (1 + bpr) / sqrt((1 + 0.82 * bpr) * G0) * Z * Vmach + (0.23 + 0.19 * sqrt(bpr)) * X * Vmach^2
  
  # Calculate the maximum takeoff thrust in N under the given environmental conditions and airspeed
  Fmax <- thrust_ratio * slst * n
  
  # Apply the maximum takeoff thrust reduction permissible
  Frto <- Fmax * (100 - sim$reg_rto) / 100
  
  # Assemble the results into a list
  F <- list("max" = Fmax, "rto" = Frto)
  
  # Return the results
  return(F)
  
}

##############################################################################
# Function fn_sim_acc                                                        #
# Calculate the acceleration a in m/s² based on:                             #
#  W     = aircraft weight in N                                              #
#  F     = propulsive force in N                                             #
#  cD    = dimensionless coefficient of drag                                 #
#  cL    = dimensionless coefficient of lift                                 #
#  q     = dynamic pressure                                                  #
#  S     = total wing area (incl. flaps/slats at takeoff) in m²              #
# Adapted from Blake (2009)                                                  #
##############################################################################

fn_sim_acc <- function(W, F, cD, cL, q, S) {
  
  # Calculate the acceleration of the aircraft in m/s along the runway
  a <- sim$g / W * (F * 2 - (sim$mu * W) - (cD - (sim$mu * cL)) * (q * S) - (W * sin(sim$theta)))
  
  # Return the results
  return(a)
  
}

##############################################################################
# Function fn_sim_dis                                                        #
# Calculate the horizontal takeoff distances in m based on:                  #
#  a    = acceleration in m/s²                                               #
#  Vgnd = groundspeed in m/s                                                 #
# Adapted from Blake (2009)                                                  #
##############################################################################

fn_sim_dis <- function(a, Vgnd) {
  
  # Calculate the average acceleration (i.e., midpoint acceleration) between two groundspeed increments
  # a_avg <- frollmean(x = a, n = 2, fill = head(a, 1), align = "right")
  a_avg <- frollmean(x = a, n = 2, fill = 0, align = "right")
  
  # Calculate the average groundspeed between two groundspeed increments
  Vgnd_avg <- frollmean(x = Vgnd, n = 2, fill = 0, align = "right")
  
  # Calculate the groundspeed difference between two groundspeed increments
  Vgnd_inc <- Vgnd - shift(x = Vgnd, n = 1, fill = 0, type = "lag")
  
  # Calculate the distance in meters covered within each speed increment
  inc <- Vgnd_avg * Vgnd_inc / a_avg
  
  # Increment the cumulative (running total) distance accordingly
  cum <- cumsum(x = inc)
  
  # Calculate the airborne distance from Vlof to screen height based on Gratton et al. (2020)
  ad <- .3048 * (35 / cos(7.7))
  
  # Add the regulatory safety margin of 15% as per 14 CFR § 25.113 (1998)
  # 15% applied to the takeoff run distance (TOR) which is from brake release to Vlof + up to the middle point between Vlof and reaching screen height
  todr <- (max(cum) + ad / 2) * (sim$reg_dis / 100)
  
  # Return the results
  return(todr)
  
}

# ##############################################################################
# # Function fn_sim_cff                                                        #
# # Calculate various drag parameters                                          #
# # Adapted from Sun et al. (2020)                                             #
# ##############################################################################
# 
# fn_sim_cff <- function(cD0, k, lambda_f, cfc, SfS, flap_angle, m, S, span) {
#   
#   # Calculate the drag coefficient component attributable to flaps
#   delta_cD_flaps <- lambda_f * cfc^1.38 * SfS * sin(flap_angle * pi / 180)^2
#   
#   # Calculate the drag coefficient component attributable to the landing gear
#   delta_cD_gear <- m * sim$g * S * 3.16E-5 * m^-.215
#   
#   # Calculate the total drag coefficient in non-clean configuration
#   cD0_total <- cD0 + delta_cD_flaps + delta_cD_gear
#   
#   # Calculate the Oswald efficiency factor for the selected flap deflection (for wing-mounted engines)
#   delta_e_flaps <- .0026 * flap_angle
#   
#   # Calculate the aspect ratio
#   ar <- span^2 / S
#   
#   # Calculate the lift-induced coefficient k in non-clean configuration
#   k_total <- 1 / (1 / k + pi * ar * delta_e_flaps)
#   
#   # Calculate the coefficient of lift in non-clean configuration
#   cL <- m * sim$g / ( q * S )
#   
#   # Calculate the coefficient of drag in non-clean configuration
#   cD <- cD0_total + k_total * cL^2
#   
#   # Assemble the results
#   c <- list("D" = cD, "L" = cL)
#   
#   # Return the results
#   return(c)
#   
# }

##############################################################################
# Function fn_sim_tko                                                        #
# Wrapper function for the takeoff simulation                                #
##############################################################################

fn_sim_tko <- function(m, S, cL, cD, n, bpr, slst, hdw, ps, rho, tas) {
  
  # Calculate the weight force in N
  W <- fn_sim_wgt(m)
  
  # Calculate the speed in m/s at which lift L equals weight W, based on the estimated cL
  Vlof <- fn_sim_vlo(W, S, cL, rho)
  
  dt_out <- data.table(
    Vgnd = "numeric",
    Vtas = "numeric",
    q    = "numeric",
    L    = "numeric",
    D    = "numeric",
    Fmax = "numeric",
    Frto = "numeric",
    a    = "numeric"
  )
  
  # Calculate the takeoff speeds V in m/s
  V <- fn_sim_spd(hdw, Vlof)
  
  # Calculate the dynamic pressure q in Pa
  q <- fn_sim_dyn(rho, V$tas)
  
  # Calculate the lift force in N
  L <- fn_sim_lft(q, S, cL)
  
  # Calculate the drag force in N
  D <- fn_sim_drg(q, S, cD)
  
  # Calculate the propulsive force F in N
  F <- fn_sim_thr(n, bpr, slst, ps, tas, V$tas)
  
  # Calculate the acceleration a in m/s² up to liftoff
  a <- fn_sim_acc(W, F$rto, cD, cL, q, S)
  
  # Calculate the horizontal takeoff distance in m
  todr <- fn_sim_dis(a, V$gnd)
  
}

# EOF