################################################################################
#    NAME: scripts/6.0_functions.R                                             #
#   INPUT: None                                                                #
# ACTIONS: Define functions used in the takeoff simulation                     #
#  OUTPUT: Set of global functions loaded into R's environment                 #
# RUNTIME: <1 second on the researcher's config (https://bit.ly/3ChCBAP)       #
################################################################################

################################################################################
# Function fn_todr                                                             #
# Calculate the regulatory takeoff distance required                           #
################################################################################

fn_todr <- function(dt) {
  
  ##############################################################################
  # Calculate the weight force W in N based on:                                #
  #  g = Gravitational acceleration constant in m/s²                           #
  #  m = aircraft mass in kg                                                   #
  ##############################################################################
  
  # W <- sim$g * dt[, m]
  
  ##############################################################################
  # Calculate the speed in m/s at which lift L equals weight W based on:       #
  #  W   = aircraft weight in N                                                #
  #  S   = wing surface area in m²                                             #
  #  cL  = dimensionless lift coefficient                                      #
  #  rho = air density in kg/m³                                                #
  # Adapted from Blake (2009)                                                  #
  ##############################################################################
  
  Vlof <- sqrt(dt[, W] / (.5 * dt[, rho] * dt[, S] * dt[, cL]))
  
  # Create airspeed intervals up to the minimum takeoff airspeed
  Vtas <- seq(from = 0 + dt[, hdw], to = Vlof, length = sim$int)
  
  # Create groundspeed intervals up to the minimum takeoff airspeed
  Vgnd <- seq(from = 0, to = Vlof - dt[, hdw], length = sim$int)
  
  # Assemble the results into a list
  # V <- list("tas" = Vtas, "gnd" = Vgnd)
  
  ##############################################################################
  # Calculate the dynamic pressure q in Pa for each airspeed interval based on:#
  #  rho  = air density in kg/m³                                               #
  #  Vtas = airspeed intervals in m/s                                          #
  # Adapted from Blake (2009)                                                  #
  ##############################################################################
  
  q <- .5 * dt[, rho] * Vtas^2
  
  ##############################################################################
  # Calculate the propulsive force F in N for each airspeed interval based on: #
  #  bpr  = engine bypass ratio                                                #
  #  n    = engine count in units                                              #
  #  ps   = air pressure in Pa                                                 #
  #  rto  = percentage of reduction from the maximum takeoff thrust            #
  #  slst = engine sea-level static maximum thrust in N (per engine)           #
  #  tas  = air temperature in K                                               #
  #  Vtas = airspeed intervals in m/s                                          #
  # Adapted from Sun et al. (2020)                                             #
  ##############################################################################
  
  # Calculate the speed of sound in m/s for the given temperature in dry air
  Vsnd <- sqrt(sim$gamma * sim$Rd * dt[, tas])
  
  # Calculate the Mach number for each airspeed interval 
  Vmach <- Vtas / Vsnd
  
  # Calculate the air pressure ratio
  dP = dt[, ps] / sim$ps_isa
  
  # Calculate the coefficients of thrust at the current air pressure ratio
  G0 <- 0.0606 * dt[, bpr] + 0.6337
  A <- -0.4327 * dP^2 + 1.3855 * dP + 0.0472
  Z <- 0.9106 * dP^3 - 1.7736 * dP^2 + 1.8697 * dP
  X <- 0.1377 * dP^3 - 0.4374 * dP^2 + 1.3003 * dP
  
  # Calculate the thrust ratio for each Mach number
  thrust_ratio <- A - 0.377 * (1 + dt[, bpr]) / sqrt((1 + 0.82 * dt[, bpr]) * G0) * Z *
    Vmach + (0.23 + 0.19 * sqrt(dt[, bpr])) * X * Vmach^2
  
  # Calculate the maximum takeoff thrust in N for each Mach number
  Fmax <- thrust_ratio * dt[, slst] * dt[, n]
  
  # Apply the maximum takeoff thrust reduction permissible
  Frto <- Fmax * (100 - dt[, rto]) / 100
  
  # Assemble the results into a list
  # F <- list("max" = Fmax, "rto" = Frto)
  
  ##############################################################################
  # Calculate the acceleration a in m/s² based on:                             #
  #  W  = aircraft weight in N                                                 #
  #  F  = propulsive force in N                                                #
  #  cD = dimensionless drag coefficient                                       #
  #  cL = dimensionless lift coefficient                                       #
  #  q  = dynamic pressure in Pa                                               #
  #  S  = wing surface area in m²                                              #
  # Adapted from Blake (2009)                                                  #
  ##############################################################################

  # Calculate the acceleration a in m/s² up to liftoff
  a <- sim$g / dt[, W] * (Frto * 2 - (sim$mu * dt[, W]) -
    (dt[, cD] - (sim$mu * dt[, cL])) * (q * dt[, S]) - (dt[, W] * sin(sim$theta)))
  
  ##############################################################################
  # Calculate the horizontal takeoff distances in m based on:                  #
  #  a    = acceleration in m/s²                                               #
  #  Vgnd = groundspeed intervals in m/s                                       #
  # Adapted from Blake (2009)                                                  #
  ##############################################################################
  
  # Calculate the average acceleration between two groundspeed increments
  a_avg <- frollmean(x = a, n = 2, fill = head(a, 1), align = "right")
  
  # Calculate the average groundspeed between two groundspeed increments
  Vgnd_avg <- frollmean(x = Vgnd, n = 2, fill = 0, align = "right")
  
  # Calculate the groundspeed difference between two groundspeed increments
  Vgnd_inc <- Vgnd - shift(x = Vgnd, n = 1, fill = 0, type = "lag")
  
  # Calculate the distance in meters covered within each speed increment
  inc <- Vgnd_avg * Vgnd_inc / a_avg
  
  # Increment the cumulative (running total) distance accordingly
  cum <- cumsum(x = inc)
  
  # Calculate the airborne distance from Vlof to screen height
  # Based on Gratton et al. (2020).
  ad <- .3048 * (35 / cos(7.7))
  
  # Add the regulatory safety margin of 15% as per 14 CFR § 25.113 (1998)
  # Applied to the takeoff run distance, which is from brake release to Vlof
  # + up to the middle point between Vlof and reaching screen height.
  todr <- (max(cum) + ad / 2) * (sim$reg_dis / 100)
  
  return(todr)

}

# EOF