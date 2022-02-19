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

fn_todr <- function(A, bpr, cD, cL, G0, hdw, n, rho, S, slst, Vlof, Vsnd, W, X, Z) {

  len <- length(A)


  # ============================================================================
  # 1 Calculate the airspeed and groundspeed intervals Vtas and Vgnd in m/s 
  # Groundspeed is airspeed plus headwind.
  # ============================================================================

  # Calculate airspeed Vtas in m/s for each simulation interval up to liftoff
  Vtas <- as.vector(
    mapply(
      FUN = seq,
      from = hdw,
      to = Vlof,
      length.out = sim$int
    )
  )

  # Calculate groundspeed Vgnd in m/s for each simulation interval up to liftoff
  Vgnd <- as.vector(
    mapply(
      FUN = seq,
      from = 0L,
      to = Vlof - hdw,
      length.out = sim$int
    )
  )

  # ============================================================================
  # 2 Calculate the dynamic pressure q in Pa
  # Adapted from Blake (2009).
  # ============================================================================

  q <- Vtas * .5 * rep(rho, each = sim$int)

  # ============================================================================
  # 3 Calculate the propulsive force F in N
  # Adapted from Sun et al. (2020).
  # ============================================================================

  # Calculate the dimensionless Mach number for each airspeed interval
  Vmach <- Vtas / rep(Vsnd, each = sim$int)

  # Calculate the thrust ratio for each Mach number interval
  bpr_vec <- rep(bpr, each = sim$int)
  tr <- rep(A, each = sim$int) - 0.377 * (1 + bpr_vec) /
    sqrt((1 + 0.82 * bpr_vec) * rep(G0, each = sim$int)) *
    rep(Z, each = sim$int) * Vmach +
    (0.23 + 0.19 * sqrt(bpr_vec)) * rep(X, each = sim$int) *
      Vmach^2

  # Calculate the maximum takeoff thrust in N for each Mach number
  Fmax <- tr * rep(slst, each = sim$int) * rep(n, each = sim$int)

  # Apply the maximum takeoff thrust reduction permissible
  Frto <- Fmax * (100 - rep(rto, each = sim$int)) / 100

  # ============================================================================
  # 4 Calculate the acceleration in m/s² up to liftoff
  # Adapted from Blake (2009).
  # ============================================================================

  W_vec <- rep(W, each = sim$int)
  a <- sim$g / W_vec * (Frto * 2 - (sim$mu * W_vec) -
    (rep(cD, each = sim$int) - (sim$mu * rep(cL, each = sim$int))) *
      (q * rep(S, each = sim$int)) - (W_vec * sin(sim$theta)))

  # ============================================================================
  # 5 Calculate the horizontal takeoff distances in m
  # Adapted from Blake (2009).
  # ============================================================================

  # Set the rolling window width
  width <- rep(x = c(seq.int(2), rep(2, sim$int - 2)), times = len)

  # Calculate mean acceleration between two groundspeed increments
  a_bar <- frollmean(x = a, n = width, adaptive = TRUE)

  # Calculate mean groundspeed between two groundspeed increments
  Vgnd_bar <- frollmean(x = Vgnd, n = width, adaptive = TRUE)

  # Calculate the size of each groundspeed interval
  d <- diff(Vgnd)                 # Difference between consecutive speeds
  d[d < 0] <- NA                  # Replace negative speeds with NAs
  d <- append(x = d, values = NA) # Pad to original length
  d <- zoo::na.locf(d)            # Carry forward the last speed over NAs

  # Calculate the incremental distance in m of each groundspeed interval
  inc <- Vgnd_bar * d / a_bar

  # Calculate the cumulative distance in m across all groundspeed intervals
  cum <- frollsum(
    x = inc,
    n = rep(x = c(rep(x = 0, times = 9), 10), times = len),
    adaptive = TRUE
  )

  # Collapse the vector to only the actual TODR values
  cum <- cum[cum > 0]

  # Add airborne distance from Vlof to screen height (Gratton et al, 2020)
  # and apply the regulatory safety margin of 15% as per 14 CFR § 25.113
  # (1998) and CS-25.113 (2021) applicable to the horizontal distance from
  # the start of the takeoff to reaching the screen height, assuming a dry
  # runway and no clearway. Round up to the nearest integer.
  # todr <- ceiling(
  #   (cum + sim$ft_to_m * sim$scrn_hght / cos(sim$climb_angle)) *
  #     sim$margin_cal
  # )
  todr <- (cum + sim$ft_to_m * sim$scrn_hght / cos(sim$climb_angle)) *
      sim$margin_cal

  return(todr)

} # End of fn_todr function

# EOF