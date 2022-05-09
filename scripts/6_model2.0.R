# ==============================================================================
#    NAME: scripts/6_model.R
#   INPUT: A data table of aircraft characteristics and climatic observations
# ACTIONS: Perform a simulated takeoff and calculate the takeoff distance in m
#  OUTPUT: A vector of the ground component of the takeoff distance required
# RUNTIME: Variable based on input data table size
# ==============================================================================

# ==============================================================================
# Define a function to calculate the liftoff speed based on:
# cL         = dimensionless lift coefficient
# g          = gravitational acceleration constant in m/s²
# m          = aircraft mass in kg
# rho        = air density in kg/m³
# S          = wing surface area in m²
# vs_to_vlof = safety factor from stall speed to liftoff speed
# W          = aircraft weight in N
# Adapted from Blake (2009).
# ==============================================================================

fn_Vlof <- function(DT) {

  Vlof <- sqrt(DT[, m] * sim$g / (.5 * DT[, rho] * DT[, S] * DT[, cL])) *
    sim$vs_to_vlof

}

# ==============================================================================
# Define a function to calculate the ground distance required based on:
# A        = thrust coefficient
# bpr      = engine bypass ratio
# cD       = dimensionless drag coefficient
# cL       = dimensionless lift coefficient
# G0       = thrust coefficient
# hdw      = headwind in m/s
# n        = engine count in units
# rho      = air density in kg/m³
# S        = wing surface area in m²
# slst     = engine sea-level static maximum thrust in N (per engine)
# Vlof     = speed in m/s at which lift L equals weight W, plus a safety margin
# Vsnd     = speed of sound in m/s for the given temperature in dry air
# W        = aircraft weight in N
# X        = thrust coefficient
# Z        = thrust coefficient
# ==============================================================================

fn_todr_gnd <- function(DT) {

  # ============================================================================
  # 1 Calculate the airspeed and groundspeed intervals Vtas and Vgnd in m/s 
  # Groundspeed is airspeed plus headwind.
  # ============================================================================

  # Calculate airspeed Vtas in m/s for each simulation interval up to liftoff
  Vtas <- as.vector(
    mapply(
      FUN = seq,
      from = DT[, hdw],
      to = DT[, Vlof],
      length.out = sim$int
    )
  )

  # Calculate groundspeed Vgnd in m/s for each simulation interval up to liftoff
  Vgnd <- as.vector(
    mapply(
      FUN = seq,
      from = 0L,
      to = DT[, Vlof] - DT[, hdw],
      length.out = sim$int
    )
  )

  # ============================================================================
  # 2 Calculate the dynamic pressure q in Pa
  # Adapted from Blake (2009).
  # ============================================================================

  # Air density rho in kg/m³
  rho <- rep(DT[, rho], each = sim$int)

  # Dynamic pressure q in N/m²
  q <- .5 * rho * Vtas^2L

  # ============================================================================
  # 3 Calculate the propulsive force F in N
  # Adapted from Sun et al. (2020).
  # ============================================================================

  # Vectorize the speed of sound
  # Vsnd <- sqrt(sim$gamma * sim$Rd * DT[, tas]) # NOT VECTORIZED!!
  Vsnd <- rep(sqrt(sim$gamma * sim$Rd * DT[, tas]), each = sim$int)

  # Calculate the dimensionless Mach number for each airspeed interval
  Vmach <- Vtas / Vsnd

  # Calculate the air pressure ratio
  dP <- rep(DT[, ps] / sim$ps_isa, each = sim$int)

  # Vectorize the engine bypass ratio
  bpr <- rep(DT[, bpr], each = sim$int)

  # Calculate the thrust coefficients
  G0 <-  .0606 * bpr  + .6337
  A  <- -.4327 * dP^2L + 1.3855 * dP    + .0472
  X  <-  .1377 * dP^3L - .4374  * dP^2L + 1.3003 * dP
  Z  <-  .9106 * dP^3L - 1.7736 * dP^2L + 1.8697 * dP

  # Calculate the thrust ratio for each Mach number interval
  tr <- A - .377 * (1L + bpr) / sqrt((1 + .82 * bpr) * G0) * Z * Vmach +
    (.23 + .19 * sqrt(bpr)) * X * Vmach^2L

  # Vectorize the sea-level static thrust in N
  slst <- rep(DT[, slst], each = sim$int)

  # Vectorize the engine count
  n <- rep(DT[, n], each = sim$int)

  # Calculate the maximum takeoff thrust in N for each airspeed interval
  Fmax <- tr * slst * n

  # Vectorize the thrust reduction percentage
  rto <- rep(DT[, rto], each = sim$int)

  # Apply the maximum takeoff thrust reduction permissible
  Frto <- Fmax * (100L - rto) / 100L

  # ============================================================================
  # 4 Calculate the acceleration in m/s² up to liftoff
  # Adapted from Blake (2009).
  # ============================================================================

  # Vectorize the aircraft mass in kg
  m <- rep(DT[, m], each = sim$int)

  # Calculate the aircraft weight in N
  W <- m * sim$g

  # Vectorize the wing surface area in m²
  S <- rep(DT[, S], each = sim$int)

  # Vectorize the lift coefficient
  cL <- rep(DT[, cL], each = sim$int)

  # Vectorize the drag coefficient
  cD <- rep(DT[, cD], each = sim$int)

  # Calculate the acceleration in m/s²
  a <- (sim$g / W) *
    (Frto - (sim$mu * W) - (cD - sim$mu * cL) * q * S - (W * sin(sim$theta))) # BLAKE
    # (Frto - (sim$mu * W) - .08 * q * S - (W * sin(sim$theta))) # FOR TESTING ONLY

  # ============================================================================
  # 5 Increment the horizontal takeoff distances in m
  # Adapted from Blake (2009).
  # ============================================================================

  # Set the rolling window width
  bar_width <- rep(
    x = c(seq.int(2L), rep(x = 2L, each = sim$int - 2L)),
    times = nrow(DT)
  )

  # Calculate mean acceleration between two groundspeed increments
  a_bar <- frollmean(x = a, n = bar_width, adaptive = TRUE)

  # Calculate mean groundspeed between two groundspeed increments
  Vgnd_bar <- frollmean(x = Vgnd, n = bar_width, adaptive = TRUE)

  # Vectorize the liftoff speed
  Vlof <- rep(DT[, Vlof], each = sim$int)

  # Vectorize the headwind speed
  hdw <- rep(DT[, hdw], each = sim$int)

  # Calculate the size of each groundspeed interval
  # Vgnd_int <- (DT[, Vlof] - DT[, hdw]) / (sim$int - 1) # NOT VECTORIZED!!
  Vgnd_int <- (Vlof - hdw) / (sim$int - 1L)

  # Calculate the incremental distance in m covered in each groundspeed interval
  inc <- Vgnd_bar * Vgnd_int / a_bar

  # Calculate the cumulative distance in m up to liftoff
  cum <- frollsum(
    x = inc,
    n = rep(x = seq(1L:sim$int), times = nrow(DT)),
    adaptive = TRUE
  )

  # ============================================================================
  # 6 Assemble the takeoff distance required in m
  # Adapted from Blake (2009) and Gratton et al (2020)
  # ============================================================================

  # Set the horizontal ground distance up to liftoff
  todr_gnd <- cum[seq(sim$int, length(cum), sim$int)]

  # FOR TESTING ONLY
  # out <- cbind(
  #   "type"        = rep(DT[, type], each = sim$int),
  #   "Vtas (m/s)"  = Vtas,
  #   "Vgnd (m/s)"  = Vgnd,
  #   "Vsnd (m/s)"  = Vsnd,
  #   "Vmach (m/s)" = Vmach,
  #   "rho (kg/m3)" = rho,
  #   "dP"          = dP,
  #   "q (N/m2)"    = q,
  #   "bpr"         = bpr,
  #   "n"           = n,
  #   "A"           = A,
  #   "G0"          = G0,
  #   "X"           = X,
  #   "Z"           = Z,
  #   "tr"          = tr,
  #   "slst (N)"    = slst,
  #   "Fmax (N)"    = Fmax,
  #   "rto (%)"     = rto,
  #   "Frto (N)"    = Frto,
  #   "m (kg)"      = m,
  #   "W (N)"       = W,
  #   "S (m2)"      = S,
  #   "cL"          = cL,
  #   "cD"          = cD,
  #   "mu"          = sim$mu,
  #   "mu*W"        = sim$mu * W,
  #   "(cD-mu*cL)"  = cD - sim$mu * cL,
  #   "(cD-mu*cL)qS"= (cD - sim$mu * cL) * q * S,
  #   "a (m/s2)"    = a,
  #   "a_bar"       = a_bar,
  #   "Vgnd_bar"    = Vgnd_bar,
  #   "Vgnd_int"    = Vgnd_int,
  #   "inc"         = inc,
  #   "cum"         = cum
  # )
  # print(out)

  # Return the ground portion of the takeoff distance required in m
  return(todr_gnd)

} # End of fn_todr_gnd function

# EOF
