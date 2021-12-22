################################################################################
#    NAME: scripts/6_model.R                                                   #
#   INPUT: None                                                                #
# ACTIONS: Define functions used in the takeoff simulation                     #
#  OUTPUT: Set of global functions loaded into R's environment                 #
# RUNTIME: <1 second on the researcher's config (https://bit.ly/3ChCBAP)       #
################################################################################

################################################################################
# Function fn_todr                                                             #
# Calculate the regulatory takeoff distance required (TODR) based on:          #
#  a    = acceleration in m/s²                                                 #
#  bpr  = engine bypass ratio                                                  #
#  cD   = dimensionless drag coefficient                                       #
#  cL   = dimensionless lift coefficient                                       #
#  F    = propulsive force in N                                                #
#  n    = engine count in units                                                #
#  ps   = air pressure in Pa                                                   #
#  q    = dynamic pressure in Pa                                               #
#  rho  = air density in kg/m³                                                 #
#  rto  = percentage of reduction from the maximum takeoff thrust              #
#  S    = wing surface area in m²                                              #
#  slst = engine sea-level static maximum thrust in N (per engine)             #
#  tas  = air temperature in K                                                 #
#  Vgnd = groundspeed intervals in m/s                                         #
#  Vtas = airspeed intervals in m/s                                            #
#  W    = aircraft weight in N                                                 #
################################################################################

fn_todr <- function(dt) {
  
  # ##############################################################################
  # # Calculate the speed in m/s at which lift L equals weight W                 #
  # # Adapted from Blake (2009)                                                  #
  # ##############################################################################
  # 
  # dt[, Vlof := sqrt(W / (.5 * rho * S * cL))]
  # 
  # # Create airspeed intervals up to the minimum takeoff airspeed
  # dt[, Vtas := Map(seq, from = hdw, to = Vlof, length.out = sim$int)]
  # 
  # # Create groundspeed intervals up to the minimum takeoff airspeed
  # dt[, Vgnd := Map(seq, from = 0, to = Vlof - hdw, length.out = sim$int)]
  # 
  # ##############################################################################
  # # Calculate the dynamic pressure q in Pa for each airspeed interval          #
  # # Adapted from Blake (2009)                                                  #
  # ##############################################################################
  # 
  # dt[, q := Map("*", Vtas, (.5 * rho))]
  # 
  # ##############################################################################
  # # Calculate the propulsive force F in N for each airspeed interval           #
  # # Adapted from Sun et al. (2020)                                             #
  # ##############################################################################
  # 
  # # Calculate the speed of sound in m/s for the given temperature in dry air
  # dt[, Vsnd := sqrt(sim$gamma * sim$Rd * tas)]
  # 
  # # Calculate the Mach number for each airspeed interval 
  # dt[, Vmach := Map("/", Vtas, Vsnd)]
  # 
  # # Calculate the air pressure ratio
  # dt[, dP := ps / sim$ps_isa]
  # 
  # # Calculate the coefficients of thrust at the current air pressure ratio
  # dt[, G0 :=  .0606 * bpr  +  .6337]
  # dt[, A  := -.4327 * dP^2 + 1.3855 * dP   +  .0472]
  # dt[, Z  :=  .9106 * dP^3 - 1.7736 * dP^2 + 1.8697 * dP]
  # dt[, X  :=  .1377 * dP^3 -  .4374 * dP^2 + 1.3003 * dP]
  # 
  # # Calculate the thrust ratio for each speed interval
  # dt[, thrust_ratio := Map(
  #   function(A, bpr, G0, Z, X, Vmach) {
  #     A - 0.377 * (1 + bpr) / sqrt((1 + 0.82 * bpr) * G0) * Z * Vmach +
  #       (0.23 + 0.19 * sqrt(bpr)) * X * Vmach^2},
  #   A = A, bpr = bpr, G0 = G0, Z = Z, X = X, Vmach = Vmach
  # )]
  # 
  # # Calculate the maximum takeoff thrust in N for each Mach number
  # dt[, Fmax := Map("*", thrust_ratio, slst * n)]
  # 
  # # Apply the maximum takeoff thrust reduction permissible
  # dt[, Frto := Map("*", Fmax, (100 - rto) / 100)]
  # 
  # ##############################################################################
  # # Calculate the acceleration in m/s² up to liftoff, adapted from Blake (2009)
  # ##############################################################################
  # 
  # dt[, a := Map(
  #   function(W, Frto, cD, cL, q, S) {
  #     sim$g / W * (Frto * 2 - (sim$mu * W) -
  #       (cD - (sim$mu * cL)) * (q * S) - (W * sin(sim$theta)))
  #   },
  #   W = W, Frto = Frto, cD = cD, cL = cL, q = q, S = S
  # )]
  # 
  # ##############################################################################
  # # Calculate the horizontal takeoff distances in m                            #
  # # Adapted from Blake (2009)                                                  #
  # ##############################################################################
  # 
  # # Calculate the average acceleration between two groundspeed increments
  # dt[, a_avg := frollmean(x = a, n = 2, fill = a[[1]][1], align = "right")]
  # 
  # # Calculate the average groundspeed between two groundspeed increments
  # dt[, Vgnd_avg := frollmean(x = Vgnd, n = 2, fill = 0, align = "right")]
  # 
  # # Extract the size of the groundspeed interval
  # # dt[, Vgnd_inc := Map("-", Vgnd, lapply(Vgnd, shift, n = 1, fill = 0, type = "lag"))]
  # dt[, Vgnd_inc := sapply(dt[, Vgnd], "[[", 2)]
  # 
  # # Calculate the distance in meters covered within each speed increment
  # dt[, inc := Map("/", Map("*", Vgnd_avg, Vgnd_inc), a_avg)]
  # 
  # # Increment the cumulative (running total) distance accordingly
  # dt[, cum := Map(cumsum, inc)]
  # 
  # # Calculate the airborne distance from Vlof to screen height
  # # Based on Gratton et al. (2020).
  # dt[, ad := .3048 * (35 / cos(7.7))]
  # 
  # # Add the airborne distance from Vlof to screen height (Gratton et al, 2020)
  # # and add the regulatory safety margin of 15% as per 14 CFR § 25.113 (1998)
  # # applied to the takeoff run distance, which is from brake release to Vlof
  # # + up to the middle point between Vlof and reaching screen height.
  # dt[, todr := unlist(Map(
  #   function(cum) { (max(cum) + .3048 * 35 / cos(7.7) / 2) *
  #       (sim$reg_dis / 100) },
  #   cum = cum
  # )
  
  dt[, Vlof := sqrt(W / (.5 * rho * S * cL))]
  
  # Create airspeed intervals up to the minimum takeoff airspeed
  dt[, Vtas := Map(seq, from = hdw, to = Vlof, length.out = sim$int)]
  
  # Create groundspeed intervals up to the minimum takeoff airspeed
  print("Calculating groundspeed intervals...")
  dt[, Vgnd := Map(seq, from = 0, to = Vlof - hdw, length.out = sim$int)]
  
  dt[, q := Map("*", Vtas, (.5 * rho))]
  
  # Calculate the speed of sound in m/s for the given temperature in dry air
  dt[, Vsnd := sqrt(sim$gamma * sim$Rd * tas)]
  
  # Calculate the Mach number for each airspeed interval
  dt[, Vmach := Map("/", Vtas, Vsnd)]
  
  # Calculate the air pressure ratio
  dt[, dP := ps / sim$ps_isa]
  
  # Calculate the coefficients of thrust
  dt[, G0 :=  .0606 * bpr  +  .6337]
  dt[, A  := -.4327 * dP^2 + 1.3855 * dP   +  .0472]
  dt[, Z  :=  .9106 * dP^3 - 1.7736 * dP^2 + 1.8697 * dP]
  dt[, X  :=  .1377 * dP^3 -  .4374 * dP^2 + 1.3003 * dP]
  
  # Calculate the thrust ratio
  dt[, thrust_ratio := Map(
    function(A, bpr, G0, Z, X, Vmach) {
      A - 0.377 * (1 + bpr) / sqrt((1 + 0.82 * bpr) * G0) * Z * Vmach +
        (0.23 + 0.19 * sqrt(bpr)) * X * Vmach^2
    },
    A = A, bpr = bpr, G0 = G0, Z = Z, X = X, Vmach = Vmach
  )]
  
  # Calculate the maximum takeoff thrust in N for each Mach number
  dt[, Fmax := Map("*", thrust_ratio, slst * n)]
  
  # Apply the maximum takeoff thrust reduction permissible
  dt[, Frto := Map("*", Fmax, (100 - rto) / 100)]
  
  # Calculate the acceleration a in m/s² up to liftoff
  dt[, a := Map(
    function(W, Frto, cD, cL, q, S) {
      sim$g / W * (Frto * 2 - (sim$mu * W) -
                     (cD - (sim$mu * cL)) * (q * S) - (W * sin(sim$theta)))
    },
    W = W, Frto = Frto, cD = cD, cL = cL, q = q, S = S
  )]
  
  # Calculate the average acceleration between two groundspeed increments
  dt[, a_avg := frollmean(x = a, n = 2, fill = a[[1]][1], align = "right")]
  
  # Calculate the average groundspeed between two groundspeed increments
  dt[, Vgnd_avg := frollmean(x = Vgnd, n = 2, fill = 0, align = "right")]
  
  # Extract the size of the groundspeed interval
  # dt[, Vgnd_inc := Map("-", Vgnd, lapply(Vgnd, shift, n = 1, fill = 0, type = "lag"))]
  dt[, Vgnd_inc := sapply(dt[, Vgnd], "[[", 2)]
  
  # Calculate the distance in meters covered within each speed increment
  dt[, inc := Map("/", Map("*", Vgnd_avg, Vgnd_inc), a_avg)]
  
  # Increment the cumulative (running total) distance accordingly
  dt[, cum := Map(cumsum, inc)]
  
  # Add the airborne distance from Vlof to screen height (Gratton et al, 2020)
  # and add the regulatory safety margin of 15% as per 14 CFR § 25.113 (1998)
  # applied to the takeoff run distance, which is from brake release to Vlof
  # + up to the middle point between Vlof and reaching screen height.
  dt[, todr_sim := unlist(Map(
    function(cum) { (max(cum) + .3048 * 35 / cos(7.7) / 2) *
        (sim$reg_dis / 100) },
    cum = cum
  ))]
  
  return(dt[, todr_sim])

}

# EOF