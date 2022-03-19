# ==============================================================================
#    NAME: scripts/6_model.R
#   INPUT: A data table of aircraft characteristics and climatic observations
# ACTIONS: Perform a simulated takeoff
#  OUTPUT: A vector of takeoff distance required (TODR)
# RUNTIME: Variable based on input data table size
# ==============================================================================

# ==============================================================================
# Define a function to calculate the regulatory takeoff distance required, from:
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
# Vlof     = speed in m/s at which lift L equals weight W
# Vsnd     = speed of sound in m/s for the given temperature in dry air
# W        = aircraft weight in N
# X        = thrust coefficient
# Z        = thrust coefficient
# ==============================================================================

fn_todr <- function(DT) {

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
print(Vtas)
  # ============================================================================
  # 2 Calculate the dynamic pressure q in Pa
  # Adapted from Blake (2009).
  # ============================================================================

  # Air density rho in kg/m³
  rho <- rep(DT[, rho], each = sim$int)

  # Dynamic pressure q in Pa
  q <- .5 * rho * Vtas^2

  # ============================================================================
  # 3 Calculate the propulsive force F in N
  # Adapted from Sun et al. (2020).
  # ============================================================================

  # Vectorize the speed of sound
  Vsnd <- rep(DT[, Vsnd], each = sim$int)

  # Calculate the dimensionless Mach number for each airspeed interval
  Vmach <- Vtas / Vsnd

  # Vectorize the engine bypass ratio
  bpr <- rep(DT[, bpr], each = sim$int)

  # Vectorize the thrust coefficients
  G0 <- rep(DT[, G0], each = sim$int)
  A  <- rep(DT[, A],  each = sim$int)
  X  <- rep(DT[, X],  each = sim$int)
  Z  <- rep(DT[, Z],  each = sim$int)
  
  # Calculate the thrust ratio for each Mach number interval
  tr <- A - .377 * (1 + bpr) / sqrt((1 + .82 * bpr) * G0) * Z * Vmach +
    (.23 + .19 * sqrt(bpr)) * X * Vmach^2

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

  # Vectorize the mass in kg
  m <- rep(DT[, m], each = sim$int)

  # Vectorize the weight in N
  W <- rep(DT[, W], each = sim$int)

  # Vectorize the wing surface area in m²
  S <- rep(DT[, S], each = sim$int)

  # Vectorize the lift coefficient
  cL <- rep(DT[, cL], each = sim$int)

  # Vectorize the drag coefficient
  cD <- rep(DT[, cD], each = sim$int)

  # Calculate the acceleration in m/s²
  a <- sim$g / W *
    (Frto - (sim$mu * W) - (cD - sim$mu * cL) * (q * S) - (W * sin(sim$theta)))

  # ============================================================================
  # 5 Increment the horizontal takeoff distances in m
  # Adapted from Blake (2009).
  # ============================================================================

  # Set the rolling window width
  bar_width <- rep(x = c(seq.int(2), rep(2, sim$int - 2)), times = nrow(DT))
  
  # Calculate mean acceleration between two groundspeed increments
  a_bar <- frollmean(x = a, n = bar_width, adaptive = TRUE)
  
  # Calculate mean groundspeed between two groundspeed increments
  Vgnd_bar <- frollmean(x = Vgnd, n = bar_width, adaptive = TRUE)

  # Calculate the size of each groundspeed interval
  Vgnd_int <- (DT[, Vlof] - DT[, hdw]) / (sim$int - 1)

  # Calculate the incremental distance in m covered in each groundspeed interval
  inc <- Vgnd_bar * Vgnd_int / a_bar

  # Calculate the cumulative distance in m up to liftoff
  cum <- frollsum(
    x = inc,
    n = rep(x = seq(1:sim$int), times = nrow(DT)),
    adaptive = TRUE
  )

  # ============================================================================
  # 6 Assemble the takeoff distance required in m
  # Adapted from Blake (2009) and Gratton et al (2020)
  # ============================================================================

  # Set the horizontal ground distance up to liftoff
  # The 15% regulatory factor is not applied because the cL already reflects it
  dis_gnd <- cum[seq(sim$int, length(cum), sim$int)]

  # Calculate the horizontal airborne distance after liftoff up to screen height
  # Here the 15% regulatory factor is applied
  # Adapted from Gratton et al, 2020
  dis_air <- sim$ft_to_m * sim$scrn_hght / cos(sim$climb_angle) * sim$tod_mul

  # Calculate the takeoff distance required
  todr <- dis_gnd + dis_air
  
  # Return the TODR
  return(todr)

} # End of fn_todr function

# EOF
