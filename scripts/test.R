# ==============================================================================
#    NAME: scripts/8_simulate.R
#   INPUT: 442,769,456 rows of climatic observations from the database
# ACTIONS: Assemble the aircraft, calibration, and climatic data
#          Perform simulated takeoffs for each aircraft type and climate obs.
#          Write the resulting takeoff distance required to the database
#          Index the database table
#  OUTPUT: 442,769,456 rows of takeoff data written to the database
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load required libraries
library(data.table)
library(DBI)
library(parallel)
library(stringr)

# Import the common settings
source("scripts/0_common.R")
source("scripts/6_model1.4b.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

icao <- "KILM"

# ==============================================================================
# 1 Prepare the simulation data
# ==============================================================================

# ==============================================================================
# 1.2 Import the simulation data
# ==============================================================================

# ==============================================================================
# 1.2.1 Import the aircraft characteristics (from Sun et al., 2020)
# ==============================================================================

dt_act <- fread(
  file = f$act,
  header = TRUE,
  colClasses = c(rep("factor", 3), rep("integer", 3), rep("numeric", 8)),
  drop = c("name", "eng", "span", "cD0", "k", "lambda_f", "cfc", "SfS"),
  key = "type"
)

# ==============================================================================
# 1.2.2 Import the takeoff performance calibration data (from 7_calibrate.R)
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build a query to retrieve the calibration data within the acceptable range
db_qry <- paste("SELECT type, m, cl, cd, todr_sim, todr_cal FROM ",
                tolower(db$cal), ";",
                sep = ""
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a data table
dt_cal <- suppressWarnings(
  setDT(
    dbFetch(db_res, n = Inf),
    key = c("type", "m")
  )
)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# Convert the type column to factor
dt_cal[, type := as.factor(type)]

# Restore proper capitalization
setnames(dt_cal, c("cl", "cd"), c("cL", "cD"))

# Order by type and descending mass
dt_cal <- dt_cal[order(type, -rank(m))]

# Calculate the percentage of difference between calibrated and simulated TODR
dt_cal[, diff := abs(dt_cal[, todr_sim] - dt_cal[, todr_cal]) /
         dt_cal[, todr_cal] * 100]

# Set the minimum mass for which there is a calibrated TODR
dt_cal[, mintom := min(m), by = type]

# ==============================================================================
# 2 Define a function to simulate takeoffs at each airport
# ==============================================================================

# Inform the log file
print(
  paste(
    Sys.time(),
    "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
    "icao", icao,
    "Loading simulation data",
    sep = " "
  )
)

# ============================================================================
# 2.1 Import the climatic observations for the current airport
# ============================================================================

# Connect the worker to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build a query to retrieve the takeoff conditions
db_qry <- paste(
  "SELECT obs, exp, icao, hurs, ps, tas, rho, hdw, rwy, toda FROM ", db$cli,
  " WHERE icao = '", icao, "';",
  sep = ""
)

# Send the query to the database
db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))

# Return the results to a data table
dt_cli <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "toda"))

# Release the database resource
dbClearResult(db_res)

# Disconnect the worker from the database
dbDisconnect(db_con)

# Coerce columns into their correct class
set(x = dt_cli, j = "obs",  value = as.POSIXct(dt_cli[, obs]))
set(x = dt_cli, j = "exp",  value = as.factor(dt_cli[, exp]))
set(x = dt_cli, j = "icao", value = as.factor(dt_cli[, icao]))
set(x = dt_cli, j = "rwy",  value = as.factor(dt_cli[, rwy]))

# ============================================================================
# 2.2 Combine the airport, aircraft, calibration, and climatic data
# ============================================================================

# Combine climatic observations with aircraft data (Cartesian product)
dt_tko <- dt_cli[, as.list(dt_act), by = dt_cli]

# Remove the climatic observations from the environment to free up memory
rm(dt_cli)

# Convert the airport code to a factor
dt_tko[, icao := as.factor(icao)]

# Initialize the starting mass in kg to the maximum takeoff mass
set(x = dt_tko, j = "m", value = dt_tko[, maxtom])

# Combine climatic observations with calibration data using the starting mass
dt_tko <- dt_cal[dt_tko, on = c("type", "m")]

# Add the airport's latitude and longitude for later analysis
dt_tko <- dt_apt[dt_tko, on = c("icao")]

# ============================================================================
# 2.3 Calculate static takeoff parameters specific to the climatic conditions
# ============================================================================

# Calculate the speed of sound in m/s for the given temperature in dry air
set(x = dt_tko, j = "Vsnd", value = sqrt(sim$gamma * sim$Rd * dt_tko[, tas]))

# Calculate the air pressure ratio dP
set(x = dt_tko, j = "dP", value = dt_tko[, ps] / sim$ps_isa)

# Calculate the coefficients of thrust for the given air pressure ratio
set(x = dt_tko, j = "G0", value = .0606 * dt_tko[, bpr] + .6337)
set(
  x = dt_tko,
  j = "A",
  value = -.4327 * dt_tko[, dP]^2 + 1.3855 * dt_tko[, dP] + .0472
)
set(
  x = dt_tko,
  j = "Z",
  value = .9106 * dt_tko[, dP]^3 - 1.7736 * dt_tko[, dP]^2 + 1.8697 *
    dt_tko[, dP]
)
set(
  x = dt_tko,
  j = "X",
  value = .1377 * dt_tko[, dP]^3 - .4374 * dt_tko[, dP]^2 + 1.3003 *
    dt_tko[, dP]
)

# ============================================================================
# 2.4 Initialize variable takeoff parameters for first simulation iteration
# ============================================================================

# Initialize the thrust reduction to the maximum permissible
set(x = dt_tko, j = "rto", value = sim$thrst_start + sim$thrst_incr)

# Initialize the starting TODR to an arbitrary value greater than the max TODA
set(x = dt_tko, j = "todr", value = 9999L)

# Initialize a counter to track the number of iterations of each takeoff
set(x = dt_tko, j = "itr", value = 0L)

# ============================================================================
# 2.5 Perform vectorized takeoff simulations iteratively until TODR < TODA
# ============================================================================

# FOR TESTING ONLY
# i <- dt_tko[, .I[todr > toda & m >= (mintom + sim$pax_mass)]]
# i <- dt_tko[, .I[todr > toda]]
# i <- dt_tko[, .I[m >= (mintom + sim$pax_mass)]]
# print(sum(is.na(i)))
# print(length(i))
# print(summary(dt_tko[i, m]))
# print(summary(dt_tko[i, mintom]))
cat("\014")

repeat {
  
  # ==========================================================================
  # 2.5.1 Prepare the data
  # ==========================================================================
  
  # Retrieve indices of observations where TODR > TODA and the current mass is
  # not less than the minimum mass for which there is calibrated data
  i <- dt_tko[, .I[todr > toda & m >= (mintom + sim$pax_mass)]]

print(sum(is.na(i)))
print(length(i))

  # As long as there are takeoffs that meet these conditions
  if (length(i) > 0) {

    # Save the iteration
    set(x = dt_tko, i = i, j = "itr", value = dt_tko[i, itr] + 1L)

    # Inform the log file

    print(
      paste(
        Sys.time(),
        "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
        "icao", icao,
        "itr", str_pad(dt_tko[i, mean(itr)], width = 3, side = "left", pad = " "),
        "t/o =", str_pad(
          format(length(i), big.mark = ","),
          width = 9, side = "left", pad = " "
        ),
        sep = " "
      )
    )

print("#############################")
print("Initial mass")
print(summary(dt_tko[i, m]))
print(summary(dt_tko[i, mintom]))

    # If thrust is already at TOGA, then decrease the mass by 87 kg instead
    # which is the mean summer/winter adult pax mass (Filippone, 2012, p. 52)
    set(
      x = dt_tko,
      i = i,
      j = "m",
      value = ifelse(
        dt_tko[i, rto] == 0L,
        dt_tko[i, m] - sim$pax_mass,
        dt_tko[i, m]
      )
    )

    # Otherwise decrease the thrust reduction by 1 percentage point up to TOGA
    set(
      x = dt_tko,
      i = i,
      j = "rto",
      value = ifelse(
        dt_tko[i, rto] > 0L,
        dt_tko[i, rto] - sim$thrst_incr,
        dt_tko[i, rto]
      )
    )

print("#############################")
print("Decreased thrust")
print(summary(dt_tko[i, rto]))

    # Remove the existing cL and cD values
    set(x = dt_tko, i = i, j = "cD", value = NA)
    set(x = dt_tko, i = i, j = "cL", value = NA)

    # Add the calibration data (cD and cL) again for the new mass
    dt_tko[dt_cal, cD := ifelse(is.na(cD), i.cD, cD), on = c("type", "m")]
    dt_tko[dt_cal, cL := ifelse(is.na(cL), i.cL, cL), on = c("type", "m")]

print("#############################")
print("cD")
print(summary(dt_tko[i, cD]))

print("#############################")
print("cD")
print(summary(dt_tko[i, cL]))

    # Calculate the weight force W in N
    set(x = dt_tko, i = i, j = "W", value = sim$g * dt_tko[i, m])

print("#############################")
print("W")
print(summary(dt_tko[i, W]))

    # Calculate the liftoff speed in m/s
    # Adapted from Blake (2009).
    set(
      x = dt_tko,
      i = i,
      j = "Vlof",
      value = sqrt(
        dt_tko[i, W] / (.5 * dt_tko[i, rho] * dt_tko[i, S] * dt_tko[i, cL])
      # ) * sim$vs_to_vlof
      ) * 1
    )

print("#############################")
print("Vlof")
print(summary(dt_tko[i, Vlof]))

    # ========================================================================
    # 2.5.2 Calculate the takeoff distance required TODR in m
    # ========================================================================
    
    # ============================================================================
    # 1 Calculate the airspeed and groundspeed intervals Vtas and Vgnd in m/s 
    # Groundspeed is airspeed plus headwind.
    # ============================================================================
    
    # Calculate airspeed Vtas in m/s for each simulation interval up to liftoff
    Vtas <- as.vector(
      mapply(
        FUN = seq,
        from = dt_tko[i, hdw],
        to = dt_tko[i, Vlof],
        length.out = sim$int
      )
    )
    
print("#############################")
print("Vtas")
print(summary(Vtas))

    # Calculate groundspeed Vgnd in m/s for each simulation interval up to liftoff
    Vgnd <- as.vector(
      mapply(
        FUN = seq,
        from = 0L,
        to = dt_tko[i, Vlof] - dt_tko[i, hdw],
        length.out = sim$int
      )
    )
    
    print("#############################")
    print("Vgnd")
    print(summary(Vgnd))
    
    # ============================================================================
    # 2 Calculate the dynamic pressure q in Pa
    # Adapted from Blake (2009).
    # ============================================================================
    
    q <- Vtas^2 * .5 * rep(dt_tko[i, rho], each = sim$int)
    
    print("#############################")
    print("q")
    print(summary(q))
    
    # ============================================================================
    # 3 Calculate the propulsive force F in N
    # Adapted from Sun et al. (2020).
    # ============================================================================
    
    # Calculate the dimensionless Mach number for each airspeed interval
    Vmach <- Vtas / rep(dt_tko[i, Vsnd], each = sim$int)
    
    print("#############################")
    print("Vmach")
    print(summary(Vmach))
    
    # Calculate the thrust ratio for each Mach number interval
    bpr_vec <- rep(dt_tko[i, bpr], each = sim$int)
    tr <- rep(dt_tko[i, A], each = sim$int) - .377 * (1 + bpr_vec) /
      sqrt((1 + .82 * bpr_vec) * rep(dt_tko[i, G0], each = sim$int)) *
      rep(dt_tko[i, Z], each = sim$int) * Vmach +
      (.23 + .19 * sqrt(bpr_vec)) * rep(dt_tko[i, X], each = sim$int) *
      Vmach^2
    
    # Calculate the maximum takeoff thrust in N for each airspeed interval
    Fmax <- tr * rep(dt_tko[i, slst], each = sim$int) * rep(dt_tko[i, n], each = sim$int)
    
    print("#############################")
    print("Fmax")
    print(summary(Fmax))
    
    # Apply the maximum takeoff thrust reduction permissible
    Frto <- Fmax * (100L - rep(dt_tko[i, rto], each = sim$int)) / 100L
    
    print("#############################")
    print("Frto")
    print(summary(Frto))
    
    # ============================================================================
    # 4 Calculate the acceleration in m/s² up to liftoff
    # Adapted from Blake (2009).
    # ============================================================================
    
    # Build a vector of the weight in N
    W_vec <- rep(dt_tko[i, W], each = sim$int)
    
    print("#############################")
    print("W_vec")
    print(summary(W_vec))
    
    # Calculate the acceleration in m/s
    a <- sim$g / W_vec * (Frto - (sim$mu * W_vec) -
                            (rep(dt_tko[i, cD], each = sim$int) - sim$mu * rep(dt_tko[i, cL], each = sim$int)) *
                            (q * rep(dt_tko[i, S], each = sim$int)) - (W_vec * sin(sim$theta)))

print(length(a[a < 0]))
stop()

    # Remove any negative acceleration (extreme cases of low thrust, max weight)
    a[a < 0] <- 0
    
    print("#############################")
    print("a")
    print(summary(a))
    
    # ============================================================================
    # 5 Calculate the horizontal takeoff distances in m
    # Adapted from Blake (2009).
    # ============================================================================
    
    # Set the rolling window width
    width <- rep(x = c(seq.int(2), rep(2, sim$int - 2)), times = nrow(dt_tko[i, ]))
    
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
      n = rep(x = c(rep(x = 0, times = 9), 10), times = nrow(dt_tko[i, ])),
      adaptive = TRUE
    )
    
    # Collapse the vector to only the actual TODR values
    cum <- cum[cum > 0]
    
    # Add airborne distance from Vlof to screen height (Gratton et al, 2020) times
    # the regulatory safety factor applicable to the horizontal distance, assuming
    # a dry runway and no clearway. Round up to the nearest integer.
    todr <- ceiling(
      (cum + sim$ft_to_m * sim$scrn_hght / cos(sim$climb_angle)) * sim$margin_sim
    )
    
    print(length(todr))
    
    set(
      x = dt_tko,
      i = i,
      j = "todr",
      value = todr
    )
    
    print("#############################")
    print("todr")
    print(summary(dt_tko[i, todr]))
    print(nrow(dt_tko[i, todr]))
    
  } else { # Once there are no more observations that meet the conditions
    
    # End the repeat loop
    break
    
  } # End if-else
  
} # End repeat

# ========================================================================
# 2.6 Write results to the database
# ========================================================================

# Inform the log file
print(
  paste(
    Sys.time(),
    "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
    "icao", icao,
    "Writing",
    str_pad(
      format(nrow(dt_tko), big.mark = ","),
      width = 9, side = "left", pad = " "
    ),
    "rows to the database",
    sep = " "
  )
)

# Select which columns to write to the database and in which order
cols <- c(
  "obs", "icao", "lat", "lon", "exp", "type", "maxtom", "hurs", "ps",
  "tas", "rho", "hdw", "rwy", "toda", "m", "rto", "todr", "itr"
)

# FOR TESTING ONLY
fwrite(dt_tko[, ..cols])

# Inform the log file
print(
  paste(
    Sys.time(),
    "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
    "icao", icao,
    "Wrote",
    str_pad(
      format(nrow(dt_tko), big.mark = ","),
      width = 9, side = "left", pad = " "
    ),
    "rows to the database",
    sep = " "
  )
)

# ==============================================================================
# 3 Run the simulation across multiple cores
# ==============================================================================

# FOR TESTING ONLY
# fn_simulate("KABQ")
fn_simulate(icao)

# ==============================================================================
# 5 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF
