################################################################################
#    NAME: scripts/6.0_functions.R                                             #
#   INPUT: None                                                                #
# ACTIONS: Define functions used in the takeoff simulation                     #
#  OUTPUT: Set of global functions loaded into R's environment                 #
# RUNTIME: <1 second on the researcher's config (https://bit.ly/3ChCBAP)       #
################################################################################

################################################################################
# Index of functions in this file                                              #
#  Housekeeping functions                                                      #
#   - fn_set_tbl = Set up the database table to store the takeoff calculations #
#  Data handling functions                                                     #
#   - fn_imp_apt = Import the simulation airports                              #
#   - fn_imp_act = Import the simulation aircraft                              #
#   - fn_imp_rwy = Import the runway surface friction coefficient              #
#   - fn_imp_obs = Import the simulation conditions                            #
#  Simulation functions                                                        #
#   - fn_sim_wgt = Calculate the weight force W in N                           #
#   - fn_sim_spd = Calculate the takeoff speed Vtko in m/s                     #
#   - fn_sim_lft = Calculate the lift force L in N                             #
#   - fn_sim_drg = Calculate the drag force D in N                             #
#   - fn_sim_thr = Calculate the propulsive force F in N                       #
#   - fn_sim_dyn = Calculate the dynamic pressure q in Pa                      #
#   - fn_sim_acc = Calculate the acceleration a in m/s²                        #
#   - fn_sim_tdr = Calculate the regulatory takeoff distance required in m     #
################################################################################

################################################################################
# Set constants used by the functions                                          #
################################################################################

# Gravitational acceleration constant in m/s², assuming a non-oblate, non-rotating Earth (Blake, 2009; Daidzic, 2016)
g <- 9.806665

# Factor to convert speed from m/s to kt
ms_to_kt <- 1.9438445

# Factor to convert distance from m to ft
m_to_ft <- 3.280839895

# Percentage of the speed at which lift equals weight to consider as the minimum takeoff speed
spd <- 1.2

# Percent of the horizontal distance along the takeoff path, with all engines operating, from the start of the takeoff to a point equidistant between the point at which VLOF is reached and the point at which the airplane is 35 feet above the takeoff surface, according to 14 CFR § 25.113 (1998)
dis <- 1.15

# Minimum percentage of takeoff thrust permissible under reduced takeoff thrust operations as per FAA Advisory Circular 25-13 (1988)
rto <- .75

################################################################################
# Set up the database table to store the takeoff calculations based on:        #
#  table = name of the database table to be set up                             #
# Unlike in the other scripts, we don't drop the table if it exists already    #
# This allows the user to break up the script's execution over several runs    #
################################################################################

fn_set_tbl <- function(table) {
  
  # Connect to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to create the table
  db_qry <- paste("CREATE TABLE IF NOT EXISTS", tolower(table), "(id INT UNSIGNED NOT NULL AUTO_INCREMENT, obs DATETIME NOT NULL, icao CHAR(4) NOT NULL, exp CHAR(6) NOT NULL, rwy CHAR(5) NOT NULL, act CHAR(4) NOT NULL, T_inc DECIMAL(3,2) NOT NULL, m_dec FLOAT NOT NULL, PRIMARY KEY (id));", sep = " ")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect from the database
  dbDisconnect(db_con)
  
}

################################################################################
# Import the simulation airports from the database based on:                   #
#  thr = traffic threshold above which to select airports                      #
################################################################################

fn_imp_apt <- function(thr) {
  
  ##############################################################################
  # Retrieve the ICAO codes of airports above the traffic threshold            #
  ##############################################################################
  
  # Connect to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to retrieve the sample airports above the traffic threshold
  db_qry <- paste("SELECT DISTINCT icao FROM ", db_pop, " WHERE traffic > ", pop_thr, ";", sep = "")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Return the results to a data table
  dt_smp <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "icao", check.names = FALSE))
  
  # Release the database resource
  dbClearResult(db_res)
  
  ##############################################################################
  # Exclude airports that were already processed by earlier simulations        #
  # This allows for incremental simulation runs                                #
  ##############################################################################
  
  # Build a query to check if the simulation outputs table already exists in the database
  db_qry <- paste("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = '", db_grp,"' AND table_name = '", db_tko,"';", sep = "")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Return the results to a data table
  tbl_exists <- suppressWarnings(as.logical(dbFetch(db_res, n = Inf)))
  
  # Release the database resource
  dbClearResult(db_res)
  
  # If the takeoff table already exists in the database
  if(tbl_exists == TRUE) {
    
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
  
  return(dt_out)
  
}

################################################################################
# Import the simulation aircraft from a CSV file based on:                     #
#  act = aircraft type                                                         #
################################################################################

fn_imp_act <- function(act) {
  
  # Import aircraft data from the CSV file into a data table
  dt_act <- fread(file = paste(path_aer, aer_act, sep = "/"), header = TRUE, colClasses = c("character", "character", "factor", "integer", "numeric", "integer", "integer", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
  
  # Return the data table filtered for the selected aircraft
  return(dt_act[type %in% act])
  
}

################################################################################
# Import the runway surface friction coefficient from a CSV file based on:     #
#  type = runway surface type                                                  #
################################################################################

fn_imp_rwy <- function(rwy) {
  
  # Import runway surface data from the CSV file into a data table
  dt_mu <- fread(file = paste(path_aer, aer_mu, sep = "/"), header = TRUE, colClasses = c("character", "character", "numeric", "character"))
  
  # Extract the friction coefficient for the selected runway surface type
  mu <- as.numeric(dt_mu[type == rwy, "mu"])
  
  # Return the data table filtered for the selected runway surface
  return(mu)
  
}

################################################################################
# Import the simulation conditions from the database based on:                 #
#  apt  = airport's four-letter ICAO code                                      #
#  mode = "live" for actual data, "test" for calibration data                  #
################################################################################

fn_imp_obs <- function(apt, mode) {
  
  if (mode == "live") {
    
    # Connect to the database
    db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
    
    # Build a query to retrieve the takeoff conditions
    db_qry <- paste("SELECT id, obs, exp, tas, ps, rho, wnd_hdw, rwy, toda FROM ", db_cli, " WHERE exp = 'ssp126' AND icao = '", apt, "' LIMIT 5;", sep = "")
    
    # Send the query to the database
    db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))
    
    # Return the results
    dt_obs <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "id", check.names = FALSE))
    
    # Release the database resource
    dbClearResult(db_res)
    
    # Disconnect from the database
    dbDisconnect(db_con)
    
  } else if (mode == "test") {
    
    # Import a test CSV
    
  }
  return(dt_obs)
  
}


################################################################################
# Calculate the weight force W in N based on:                                  #
#  g   = Gravitational acceleration constant in m/s²                           #
#  m   = aircraft mass in kg                                                   #
################################################################################

fn_sim_wgt <- function(m) {
  
  # Calculate the weight force in N
  W <- g * m
  
  # Return the result
  return(W)
  
}

################################################################################
# Calculate the takeoff speed in m/s based on:                                 #
#  W   = aircraft weight in N                                                  #
#  S   = aircraft wing area in m²                                              #
#  CL  = dimensionless coefficient of lift in takeoff configuration            #
#  hdw = headwind in m/s at the active runway                                  #
#  rho = air density in kg/m³                                                  #
#  int = number of integration steps of the simulation                         #
# Adapted from Blake (2009)                                                    #
################################################################################

fn_sim_spd <- function(W, S, CL, rho, spd) {
  
  # Calculate minimum takeoff speed in m/s and add the safety margin factor
  Vtko <- sqrt( W / (.5 * S * rho * CL) ) * spd
  
  # Return the results
  return(Vtko)
  
}

################################################################################
# Calculate the lift force L in N based on:                                    #
#  rho  = air density in kg/m³                                                 #
#  Vtas = true airspeed in m/s                                                 #
#  S    = total wing surface area in takeoff configuration in m²               #
#  CL   = Dimensionless coefficient of lift in takeoff configuration           #
################################################################################

fn_sim_lft <- function(rho, Vtas, S, CL) {
  
  # Calculate the lift force in N
  L <- .5 * rho * Vtas^2 * S * CL
  
  # Return the results
  return(L)
  
}

################################################################################
# Calculate the drag force D in N based on:                                    #
#  rho  = air density in kg/m³                                                 #
#  Vtas = true airspeed in m/s                                                 #
#  S    = total wing surface area in takeoff configuration in m²               #
#  CD   = Dimensionless coefficient of drag in takeoff configuration           #
################################################################################

fn_sim_drg <- function(rho, Vtas, S, CD) {
  
  # Calculate the drag force in N
  D <- .5 * rho * Vtas^2 * S * CD
  
  # Return the results
  return(D)
  
}

################################################################################
# Calculate the propulsive force F in N based on:                              #
#  n    = engine count in units                                                #
#  bpr  = Engine bypass ratio                                                  #
#  slst = Engine sea-level static maximum thrust in N (per engine)             #
# Adapted from Sun et al. (2020)                                               #
################################################################################

fn_sim_thr <- function(n, bpr, slst, ps, tas, Vtas, rto) {
  
  # Set the adiabatic index (a.k.a., heat capacity ratio) for dry air
  gamma <- 1.401
  
  # Set the specific gas constant for dry air in J/(kg·K)
  Rd <- 287.058
  
  # Calculate the speed of sound in m/s for the given temperature
  # TODO: model it for humid air using http://resource.npl.co.uk/acoustics/techguides/speedair/
  Vsnd <- sqrt(gamma * Rd * tas)
  
  # Calculate the mach number
  Vmach <- Vtas / Vsnd
  
  # Set the air pressure in Pa at sea level under international standard atmospheric conditions
  ps_isa <- 101325L
  
  # Calculate the air pressure ratio
  dP = ps / ps_isa
  
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
  F <- Fmax * rto
  
  # Return the results
  return(F)
  
}

################################################################################
# Calculate the dynamic pressure q in Pa based on:                             #
#  rho  = air density in kg/m³                                                 #
#  Vtas = true airspeed in m/s                                                 #
# Adapted from Blake (2009)                                                    #
################################################################################

fn_sim_dyn <- function(rho, Vtas) {
  
  # Calculate the dynamic pressure
  q <- .5 * rho * Vtas^2
  
  # Return the results
  return(q)
  
}

################################################################################
# Calculate the acceleration a in m/s² based on:                               #
#  g     = gravitational acceleration in m/s²                                  #
#  W     = aircraft weight in N                                                #
#  Trto  = takeoff thrust in N                                                 #
#  mu    = dimensionless runway friction coefficient                           #
#  CD    = dimensionless coefficient of drag                                   #
#  CL    = dimensionless coefficient of lift                                   #
#  q     = dynamic pressure                                                    #
#  S     = total wing area (incl. flaps/slats at takeoff) in m²                #
#  theta = runway slope in °                                                   #
# Adapted from Blake (2009)                                                    #
################################################################################

fn_sim_acc <- function(g, W, Trto, mu, CD, CL, q, S, theta) {
  
  # Calculate the acceleration of the aircraft in m/s along the runway
  a <- g / W * (Trto * 2 - (mu * W) - (CD - (mu * CL)) * (q * S) - (W * sin(theta)))
  
  # Return the results
  return(a)
  
}

################################################################################
# Calculate the regulatory takeoff distance required in m                      #
# Adapted from Blake (2009)                                                    #
################################################################################

fn_sim_tdr <- function(a, Vgnd, dis) {

  # Calculate the average acceleration (i.e., midpoint acceleration) between two groundspeed increments
  a_avg <- frollmean(x = a, n = 2, fill = head(a, 1), align = "right")
  
  # Calculate the average groundspeed between two groundspeed increments
  Vgnd_avg <- frollmean(x = Vgnd, n = 2, fill = 0, align = "right")
  
  # Calculate the groundspeed difference between two groundspeed increments
  Vgnd_inc <- Vgnd - shift(x = Vgnd, n = 1, fill = 0, type = "lag")
  
  # Calculate the distance in meters covered in each segment
  seg <- Vgnd_avg * Vgnd_inc / a_avg
  
  # Increment the cumulative (running total) distance accordingly
  cum <- cumsum(x = seg)
  
  # Add the regulatory safety margin as per 14 CFR § 25.113 (1998)
  todr <- max(cum) * dis
  
  # Return the results
  return(todr)
  
}

################################################################################
# Calculate various drag parameters                                            #
# Adapted from Sun et al. (2020)                                               #
################################################################################

fn_drag <- function(m, V, rho, cd0, k, path_angle) {
  
  # Convert airspeed to knots
  V <- V * ms_to_kt
  
  # Convert altitude to feet
  # alt <- fn_m_to_ft(alt)
  
  
  
  
  
  
  
  
  
  
  
  
  gamma <- path_angle * pi / 180
  
  S = 121
  
  L <- m * g * cos(gamma)
  qS
  cL <- L /
  
  cD <- cd0 + k + cl^2
  
  D <- cd * qS
  
  
  
  
  
  
}

# fn_drag(79000, 150, 1.225, .017, .038, 0)

















































# EOF