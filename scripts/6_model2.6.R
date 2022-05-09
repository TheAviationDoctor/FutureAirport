# ==============================================================================
#    NAME: scripts/6_model.R
#   INPUT: A data table of aircraft characteristics and climatic observations
# ACTIONS: Perform a simulated takeoff and calculate the takeoff distance in m
#  OUTPUT: A vector of the ground component of the takeoff distance required
# RUNTIME: Variable based on input data table size
# ==============================================================================

# ==============================================================================
# 1 Define a function to calculate the liftoff speed based on:
# cllof = dimensionless lift coefficient at liftoff
# g     = gravitational acceleration constant in m/s²
# m     = aircraft mass in kg
# rho   = air density in kg/m³
# s     = wing surface area (incl. extended flaps) in m²
# Adapted from Blake (2009).
# ==============================================================================

fn_vlof <- function(DT) {

  vlof <- sqrt(
    (2 * DT[, tom] * sim$g) /
    (DT[, rho] * DT[, s] * DT[, cllof])
  )

} # End of fn_vlof function

# ==============================================================================
# 2 Define a function to calculate the horizontal dist. covered by the aircraft
# during the first-segment climb, in m. Adapted from Filippone (2012, p. 258).
# ==============================================================================

fn_dis_air <- function() {
  
  dis_air <- sim$scr_hgt / tan(sim$clb_ang * pi / 180) * sim$ft_to_m
  
} # End of fn_dis_air function

# ==============================================================================
# 3 Define a function to calculate the ground distance required based on:
# bpr   = engine bypass ratio
# cd    = dimensionless drag coefficient
# cllof = dimensionless lift coefficient at liftoff
# g0    = thrust coefficient
# hdw   = headwind in m/s
# n     = engine count in units
# rho   = air density in kg/m³
# s     = wing surface area in m²
# slst  = engine sea-level static maximum thrust in N (per engine)
# vlof  = speed in m/s at which lift L equals weight W, plus a safety margin
# vsnd  = speed of sound in m/s for the given temperature in dry air
# w     = aircraft weight in N
# x     = thrust coefficient
# y     = thrust coefficient
# z     = thrust coefficient
# ==============================================================================

fn_dis_gnd <- function(DT) {

  # ============================================================================
  # 3.1 Calculate the airspeed and groundspeed intervals in m/s 
  # Groundspeed is airspeed plus headwind.
  # ============================================================================

  # Calculate the airspeed in m/s for each simulation interval up to liftoff
  vtas <- as.vector(
    mapply(
      FUN = seq,
      from = DT[, hdw],
      to = DT[, vlof],
      length.out = sim$int_stp
    )
  )

  # Calculate the groundspeed in m/s for each simulation interval up to liftoff
  vgnd <- as.vector(
    mapply(
      FUN = seq,
      from = 0L,
      to = DT[, vlof] - DT[, hdw],
      length.out = sim$int_stp
    )
  )

  # ============================================================================
  # 3.2 Calculate the propulsive force in N
  # Adapted from Sun et al. (2020).
  # ============================================================================

  # Vectorize the speed of sound
  vsnd <- rep(sqrt(sim$adb_idx * sim$rsp_dry * DT[, tas]), each = sim$int_stp)

  # Calculate the dimensionless Mach number for each airspeed interval
  vmach <- vtas / vsnd

  # Calculate the air pressure ratio
  dp <- rep(DT[, ps] / sim$ps_isa, each = sim$int_stp)

  # Vectorize the engine bypass ratio
  bpr <- rep(DT[, bpr], each = sim$int_stp)

  # Calculate the thrust coefficients
  g0 <-  .0606 * bpr  + .6337
  y  <- -.4327 * dp^2L + 1.3855 * dp    + .0472
  x  <-  .1377 * dp^3L - .4374  * dp^2L + 1.3003 * dp
  z  <-  .9106 * dp^3L - 1.7736 * dp^2L + 1.8697 * dp

  # Calculate the thrust ratio for each Mach number interval
  tr <- y - .377 * (1L + bpr) / sqrt((1 + .82 * bpr) * g0) * z * vmach +
    (.23 + .19 * sqrt(bpr)) * x * vmach^2L

  # Vectorize the sea-level static thrust in N
  slst <- rep(DT[, slst], each = sim$int_stp)

  # Vectorize the engine count
  n <- rep(DT[, n], each = sim$int_stp)

  # Calculate the maximum takeoff thrust in N for each airspeed interval
  fmax <- tr * slst * n

  # Vectorize the thrust reduction percentage
  thr_red <- rep(DT[, thr_red], each = sim$int_stp)

  # Apply the maximum takeoff thrust reduction permissible
  frto <- fmax * (100L - thr_red) / 100L

  # ============================================================================
  # 3.3 Calculate the acceleration in m/s² up to liftoff
  # Adapted from Blake (2009).
  # ============================================================================

  # Vectorize the takeoff mass in kg
  tom <- rep(DT[, tom], each = sim$int_stp)

  # Calculate the aircraft weight in N
  w <- tom * sim$g

  # Vectorize the wing surface area in m²
  s <- rep(DT[, s], each = sim$int_stp)

  # Vectorize the lift coefficient
  cllof <- rep(DT[, cllof], each = sim$int_stp)

  # Vectorize the drag coefficient
  cd <- rep(DT[, cd], each = sim$int_stp)

  # Vectorize the air density in kg/m³
  rho <- rep(DT[, rho], each = sim$int_stp)
  
  # Calculate the dynamic pressure in N/m²
  q <- .5 * rho * vtas^2L

  # Calculate the acceleration in m/s²
  acc <- (sim$g / w) * (frto - (sim$rwy_frc * w) - (cd - sim$rwy_frc * cllof) *
    q * s - (w * sin(sim$rwy_slp)))

  # Check for negative accelerations (extreme cases of elevated temperature, low
  # air density, low air pressure, max takeoff mass, and max reduced thrust).
  acc_neg <- sum(acc < 0)

  # Zero out negative accelerations in extreme cases environmental cases
  if(acc_neg > 0) {
    print(paste(acc_neg, "negative accelerations were zeroed out."), sep = " ")
    acc[acc < 0] <- .10^-3
  }

  # ============================================================================
  # 3.4 Increment the horizontal takeoff distances in m
  # Adapted from Blake (2009).
  # ============================================================================

  # Set the rolling window width
  bar_width <- rep(
    x = c(seq.int(2L), rep(x = 2L, each = sim$int_stp - 2L)),
    times = nrow(DT)
  )

  # Calculate mean acceleration between two groundspeed increments
  acc_bar <- frollmean(x = acc, n = bar_width, adaptive = TRUE)

  # Calculate mean groundspeed between two groundspeed increments
  vgnd_bar <- frollmean(x = vgnd, n = bar_width, adaptive = TRUE)

  # Vectorize the liftoff speed
  vlof <- rep(DT[, vlof], each = sim$int_stp)

  # Vectorize the headwind speed
  hdw <- rep(DT[, hdw], each = sim$int_stp)

  # Calculate the size of each groundspeed interval
  vgnd_int <- (vlof - hdw) / (sim$int_stp - 1L)

  # Calculate the incremental distance in m covered in each groundspeed interval
  inc <- vgnd_bar * vgnd_int / acc_bar

  # Calculate the cumulative distance in m up to liftoff
  cum <- frollsum(
    x = inc,
    n = rep(x = seq(1L:sim$int_stp), times = nrow(DT)),
    adaptive = TRUE
  )

  # ============================================================================
  # 3.5 Assemble the takeoff distance required in m
  # Adapted from Blake (2009) and Gratton et al (2020)
  # ============================================================================

  # Set the horizontal ground distance up to liftoff
  dis_gnd <- cum[seq(sim$int_stp, length(cum), sim$int_stp)]

} # End of fn_dis_gnd function

# EOF
