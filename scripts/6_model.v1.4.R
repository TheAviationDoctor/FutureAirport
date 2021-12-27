#===============================================================================
#    NAME: scripts/6_model.R
#   INPUT: A data table of aircraft characteristics and climatic observations
# ACTIONS: Perform a simulated takeoff
#  OUTPUT: A takeoff distance required for each climatic observation
# RUNTIME: Variable based on input data table size
#===============================================================================

#===============================================================================
# Function fn_todr
# Calculate the regulatory takeoff distance required (TODR) based on:
# A        = thrust coefficient (scalar)
# a        = acceleration in m/s² (vector)
# a_avg    = average acceleration between two groundspeed increments (vector)
# bpr      = engine bypass ratio (scalar)
# cD       = dimensionless drag coefficient (scalar)
# cL       = dimensionless lift coefficient (scalar)
# cum      = cumulative (running total) takeoff distance in m (vector)
# dP       = air pressure ratio (scalar)
# F        = propulsive force in N (vector)
# Fmax     = maximum takeoff thrust in N for each Mach number (vector)
# Frto     = reduced/flex takeoff thrust in N for each Mach number (vector)
# G0       = thrust coefficient (scalar)
# hdw      = headwind in m/s (scalar)
# inc      = incremental distance in meters within each speed increment (vector)
# m        = aircraft mass in kg (scalar)
# n        = engine count in units (scalar)
# ps       = sea-level air pressure in Pa (scalar)
# q        = dynamic pressure in Pa (vector)
# rho      = air density in kg/m³ (scalar)
# reg_dis  = regulatory safety margin to be added to the TODR (scalar)
# reg_rto  = percentage of takeoff thrust reduction (scalar)
# S        = wing surface area in m² (scalar)
# sim$g    = gravitational acceleration constant in m/s² (scalar)
# sim$gamma = adiabatic index (a.k.a., heat capacity ratio) for dry air (scalar)
# sim$int  = simulation resolution / number of integration steps (scalar)
# sim$mu   = dimensionless coefficient of friction (scalar)
# sim$ps_isa = Air pressure in Pa at sea level under ISA conditions (scalar)
# sim$Rd   = specific gas constant for dry air in J/(kg·K) (scalar)
# sim$theta = runway slope in ° (scalar)
# slst     = engine sea-level static maximum thrust in N (per engine) (scalar)
# tas      = sea-level air temperature in K (scalar)
# todr_sim = takeoff distance required in m (vector)
# tr       = thrust ration (vector)
# Vgnd     = groundspeed intervals in m/s (vector)
# Vgnd_avg = average groundspeed between two groundspeed increments (vector)
# Vlof     = speed in m/s at which lift L equals weight W (scalar)
# Vmach    = Mach number for each airspeed interval (vector)
# Vsnd     = speed of sound in m/s for the given temperature in dry air (scalar)
# Vtas     = airspeed intervals in m/s (vector)
# W        = aircraft weight in N (scalar)
# X        = thrust coefficient (scalar)
# Z        = thrust coefficient (scalar)
#===============================================================================

fn_todr <- function(DT) {
  
  # Inform the log file
  print(
    paste(
      Sys.time(), "pid",
      stringr::str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      DT[1, icao], "i",
      stringr::str_pad(mean(DT[, i]),  width = 3, side = "left", pad = "0"),
      "t/o", format(nrow(DT), big.mark = ","),
      "rto", scales::percent(mean(DT[, rto]) / 100),
      "Starting simulation...",
      sep = " "
    )
  )
  
  #=============================================================================
  # Calculate the weight force W in N
  #=============================================================================
  
  # Inform the log file
  print(
    paste(
      Sys.time(), "pid",
      stringr::str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      DT[1, icao], "i",
      stringr::str_pad(mean(DT[, i]),  width = 3, side = "left", pad = "0"),
      "t/o", format(nrow(DT), big.mark = ","),
      "rto", scales::percent(mean(DT[, rto]) / 100),
      "Calculating weight",
      sep = " "
    )
  )
  
  DT[, W := sim$g * m]
  
  #=============================================================================
  # Calculate the speed in m/s at which lift equals weight, from Blake (2009)
  #=============================================================================
  
  # Inform the log file
  print(
    paste(
      Sys.time(), "pid",
      stringr::str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      DT[1, icao], "i",
      stringr::str_pad(mean(DT[, i]),  width = 3, side = "left", pad = "0"),
      "t/o", format(nrow(DT), big.mark = ","),
      "rto", scales::percent(mean(DT[, rto]) / 100),
      "Calculating speeds",
      sep = " "
    )
  )
  DT[, Vlof := sqrt(W / (.5 * rho * S * cL))]
  
  # Create airspeed intervals up to the minimum takeoff airspeed
  DT[, Vtas := Map(seq, from = hdw, to = Vlof, length.out = sim$int)]
  
  # Create groundspeed intervals up to the minimum takeoff airspeed
  DT[, Vgnd := Map(seq, from = 0, to = Vlof - hdw, length.out = sim$int)]
  
  #=============================================================================
  # Calculate the dynamic pressure q in Pa, adapted from Blake (2009)
  #=============================================================================
  
  # Inform the log file
  print(
    paste(
      Sys.time(), "pid",
      stringr::str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      DT[1, icao], "i",
      stringr::str_pad(mean(DT[, i]),  width = 3, side = "left", pad = "0"),
      "t/o", format(nrow(DT), big.mark = ","),
      "rto", scales::percent(mean(DT[, rto]) / 100),
      "Calculating dynamic pressure",
      sep = " "
    )
  )
  
  DT[, q := Map("*", Vtas, (.5 * rho))]
  
  #=============================================================================
  # Calculate the propulsive force F in N, adapted from Sun et al. (2020)
  #=============================================================================
  
  # Inform the log file
  print(
    paste(
      Sys.time(), "pid",
      stringr::str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      DT[1, icao], "i",
      stringr::str_pad(mean(DT[, i]),  width = 3, side = "left", pad = "0"),
      "t/o", format(nrow(DT), big.mark = ","),
      "rto", scales::percent(mean(DT[, rto]) / 100),
      "Calculating thrust",
      sep = " "
    )
  )
  
  # Calculate the speed of sound in m/s for the given temperature in dry air
  DT[, Vsnd := sqrt(sim$gamma * sim$Rd * tas)]
  
  # Calculate the Mach number for each airspeed interval
  DT[, Vmach := Map("/", Vtas, Vsnd)]
  
  # Calculate the air pressure ratio
  DT[, dP := ps / sim$ps_isa]
  
  # Calculate the coefficients of thrust
  DT[, G0 :=  .0606 * bpr  +  .6337]
  DT[, A  := -.4327 * dP^2 + 1.3855 * dP   +  .0472]
  DT[, Z  :=  .9106 * dP^3 - 1.7736 * dP^2 + 1.8697 * dP]
  DT[, X  :=  .1377 * dP^3 -  .4374 * dP^2 + 1.3003 * dP]
  
  # Calculate the thrust ratio
  DT[, tr := Map(
    function(A, bpr, G0, Z, X, Vmach) {
      A - 0.377 * (1 + bpr) / sqrt((1 + 0.82 * bpr) * G0) * Z * Vmach +
        (0.23 + 0.19 * sqrt(bpr)) * X * Vmach^2
    },
    A = A, bpr = bpr, G0 = G0, Z = Z, X = X, Vmach = Vmach
  )]
  
  # Calculate the maximum takeoff thrust in N for each Mach number
  DT[, Fmax := Map("*", tr, slst * n)]
  
  # Apply the maximum takeoff thrust reduction permissible
  DT[, Frto := Map("*", Fmax, (100 - rto) / 100)]
  
  #=============================================================================
  # Calculate the acceleration in m/s² up to liftoff, adapted from Blake (2009)
  #=============================================================================
  
  # Inform the log file
  print(
    paste(
      Sys.time(), "pid",
      stringr::str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      DT[1, icao], "i",
      stringr::str_pad(mean(DT[, i]),  width = 3, side = "left", pad = "0"),
      "t/o", format(nrow(DT), big.mark = ","),
      "rto", scales::percent(mean(DT[, rto]) / 100),
      "Calculating acceleration",
      sep = " "
    )
  )
  
  DT[, a := Map(
    function(W, Frto, cD, cL, q, S) {
      sim$g / W * (Frto * 2 - (sim$mu * W) -
                     (cD - (sim$mu * cL)) * (q * S) - (W * sin(sim$theta)))
    },
    W = W, Frto = Frto, cD = cD, cL = cL, q = q, S = S
  )]
  
  #=============================================================================
  # Calculate the horizontal takeoff distances in m, adapted from Blake (2009)
  #=============================================================================
  
  # Inform the log file
  print(
    paste(
      Sys.time(), "pid",
      stringr::str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      DT[1, icao], "i",
      stringr::str_pad(mean(DT[, i]),  width = 3, side = "left", pad = "0"),
      "t/o", format(nrow(DT), big.mark = ","),
      "rto", scales::percent(mean(DT[, rto]) / 100),
      "Calculating takeoff distance",
      sep = " "
    )
  )
  
  # Calculate the average acceleration between two groundspeed increments
  DT[, a_avg := frollmean(x = a, n = 2, fill = a[[1]][1], align = "right")]
  
  # Calculate the average groundspeed between two groundspeed increments
  DT[, Vgnd_avg := frollmean(x = Vgnd, n = 2, fill = 0, align = "right")]
  
  # Extract the size of the groundspeed interval
  DT[, Vgnd_inc := sapply(DT[, Vgnd], "[[", 2)]
  
  # Calculate the distance in meters covered within each speed increment
  DT[, inc := Map("/", Map("*", Vgnd_avg, Vgnd_inc), a_avg)]
  
  # Increment the cumulative (running total) distance accordingly
  DT[, cum := Map(cumsum, inc)]
  
  # Add the airborne distance from Vlof to screen height (Gratton et al, 2020)
  # and add the regulatory safety margin of 15% as per 14 CFR § 25.113 (1998)
  # applied to the takeoff run distance, which is from brake release to Vlof
  # + up to the middle point between Vlof and reaching screen height.
  DT[, todr := unlist(Map(
    function(cum) { ceiling((max(cum) + .3048 * 35 / cos(7.7) / 2) *
        (reg_dis / 100)) },
    cum = cum
  ))]
  
  return(DT[, todr])
  
} # End of fn_todr function

# EOF