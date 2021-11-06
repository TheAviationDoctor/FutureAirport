################################################################################
#    NAME: scripts/6.0_functions.R                                             #
#   INPUT: None                                                                #
# ACTIONS: Define functions used in the takeoff simulation                     #
#  OUTPUT: Set of global functions loaded into R's environment                 #
# RUNTIME: <1 second on the researcher's config (https://bit.ly/3ChCBAP)       #
################################################################################

################################################################################
# Return a time series of each airport's environmental observations based on:  #
#  apt  = airport's four-letter ICAO code                                      #
################################################################################

fn_imp_obs <- function(apt) {
  
  # Connect to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to retrieve the takeoff conditions
  db_qry <- paste("SELECT id, obs, exp, tas, ps, rho, wnd_hdw, rwy, toda FROM ", db_cli, " WHERE exp = 'ssp126' AND icao = '", apt, "' LIMIT 5;", sep = "")
  
  # Send the query to the database
  db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))
  
  # Return the results
  dt_smp <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "id", check.names = FALSE))
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect from the database
  dbDisconnect(db_con)
  
  return(dt_smp)
  
}

################################################################################
# Return the takeoff speeds based on:                                          #
#  W   = aircraft weight in N                                                  #
#  S   = aircraft wing area in m²                                              #
#  CL  = dimensionless coefficient of lift in takeoff configuration            #
#  hdw = headwind in m/s at the active runway                                  #
#  rho = air density in kg/m³                                                  #
#  int = number of integration steps of the simulation                         #
# Adapted from Blake (2009)                                                    #
################################################################################

fn_tko_speed <- function(W, S, CL, rho, spd) {
  
  # Calculate minimum takeoff speed in m/s and add the safety margin factor
  Vtko <- sqrt( W / (.5 * S * rho * CL) ) * spd
  
  # Return the results
  return(Vtko)
  
}

############################################################################
# Return the lift force in N at each airspeed increment based on:          #
#  rho  = air density in kg/m³                                             #
#  Vtas = true airspeed in m/s                                             #
#  S    = total wing surface area in takeoff configuration in m²           #
#  CL   = Dimensionless coefficient of lift in takeoff configuration       #
############################################################################

fn_tko_lift <- function(rho, Vtas, S, CL) {
  
  # Calculate the lift force in N
  L <- .5 * rho * Vtas^2 * S * CL
  
  # Return the results
  return(L)
  
}

############################################################################
# Return the drag force in N at each airspeed increment based on:          #
#  rho  = air density in kg/m³                                             #
#  Vtas = true airspeed in m/s                                             #
#  S    = total wing surface area in takeoff configuration in m²           #
#  CD   = Dimensionless coefficient of drag in takeoff configuration       #
############################################################################

fn_tko_drag <- function(rho, Vtas, S, CD) {
  
  # Calculate the drag force in N
  D <- .5 * rho * Vtas^2 * S * CD
  
  # Return the results
  return(D)
  
}

################################################################################
# Return the takeoff thrust in N at each airspeed increment based on:          #
#  n    = engine count in units                                                #
#  bpr  = Engine bypass ratio                                                  #
#  slst = Engine sea-level static maximum thrust in N (per engine)             #
# Adapted from Sun et al. (2020)                                               #
################################################################################

fn_tko_thrust <- function(n, bpr, slst, ps, tas, Vtas, rto) {
  
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
  
  # Return the results
  return(Fmax)
  
}

################################################################################
# Return the dynamic pressure at each airspeed increment based on:             #
#  rho  = air density in kg/m³                                                 #
#  Vtas = true airspeed in m/s                                                 #
# Adapted from Blake (2009)                                                    #
################################################################################

fn_tko_dyn_press <- function(rho, Vtas) {
  
  # Calculate the dynamic pressure at each airspeed increment
  q <- (rho * Vtas^2) / 2
  
  # Return the results
  return(q)
  
}

################################################################################
# Calculate aircraft acceleration in m/s² at each airspeed increment based on: #
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

fn_tko_accel <- function(g, W, Trto, mu, CD, CL, q, S, theta) {
  
  # Calculate the acceleration of the aircraft in m/s along the runway
  a <- g / W * (Trto * 2 - (mu * W) - (CD - (mu * CL)) * (q * S) - (W * sin(theta)))
  
  # Return the results
  return(a)
  
}

################################################################################
# Calculate the takeoff distance required                                      #
# Adapted from Blake (2009)                                                    #
################################################################################

fn_tko_dist <- function(a, Vgnd, reg) {

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
  
  # Return the results
  return(cum)
  
}

# EOF