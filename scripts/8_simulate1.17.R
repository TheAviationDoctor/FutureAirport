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

# ==============================================================================
# 1 Prepare the simulation data
# ==============================================================================

# ==============================================================================
# 1.1 Set up the database table to store the simulation results
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build the query to create the table
# Preserve the table if it exists so as to break up script execution into chunks
db_qry <- paste(
  "CREATE TABLE IF NOT EXISTS",
  tolower(db$tko),
  "(id INT UNSIGNED NOT NULL AUTO_INCREMENT,
    obs DATETIME NOT NULL,
    icao CHAR(4) NOT NULL,
    lat FLOAT NOT NULL,
    lon FLOAT NOT NULL,
    exp CHAR(6) NOT NULL,
    type CHAR(4) NOT NULL,
    maxtom MEDIUMINT NOT NULL,
    hurs FLOAT NOT NULL,
    ps FLOAT NOT NULL,
    tas FLOAT NOT NULL,
    rho FLOAT NOT NULL,
    hdw FLOAT NOT NULL,
    rwy CHAR(5) NOT NULL,
    toda SMALLINT NOT NULL,
    m MEDIUMINT NOT NULL,
    rto SMALLINT NOT NULL,
    todr SMALLINT NOT NULL,
    itr SMALLINT UNSIGNED NOT NULL,
    PRIMARY KEY (id));",
  sep = " "
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

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
# 1.2.3 Import the list of sample airports, excluding those already processed
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build a query to retrieve the sample airports above the traffic threshold
db_qry <- paste(
  "SELECT DISTINCT icao, lat, lon FROM", tolower(db$pop),
  "WHERE traffic >", sim$pop_thr, ";",
  sep = " "
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a data table
dt_apt <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "icao"))

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# ==============================================================================
# 1.2.4 Exclude airports already processed to allow incremental simulation runs
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build a query to check if simulation outputs already exists in the database
db_qry <- paste(
  "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = '",
  tolower(db$grp), "' AND table_name = '", tolower(db$tko), "';",
  sep = ""
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a Boolean
tbl_exists <- suppressWarnings(as.logical(dbFetch(db_res, n = Inf)))

# Release the database resource
dbClearResult(db_res)

# If the takeoff table already exists in the database
if (tbl_exists == TRUE) {

  # Build a query to retrieve the airports that were already processed
  db_qry <- paste("SELECT DISTINCT icao FROM", tolower(db$tko), ";", sep = " ")

  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)

  # Return the results to a data table of airports to exclude
  dt_exc <- suppressWarnings(
    setDT(
      dbFetch(db_res, n = Inf),
      key = "icao",
      check.names = FALSE
    )
  )

  # Release the database resource
  dbClearResult(db_res)

  # Remove the airports already processed
  dt_apt <- dt_apt[!dt_exc]

} # End if

# Disconnect from the database
dbDisconnect(db_con)

# Remove the variables that are no longer needed
rm(d, db_con, db_qry, db_res, dt_exc, dt_thr, f, tbl_exists)

# ==============================================================================
# 2 Define a function to simulate takeoffs at each airport
# ==============================================================================

fn_simulate <- function(icao) {

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

  repeat {

    # ==========================================================================
    # 2.5.1 Prepare the data
    # ==========================================================================

    # Retrieve indices of observations where TODR > TODA and the current mass is
    # not less than the minimum mass for which there is calibrated data
    i <- dt_tko[, .I[todr > toda & m >= (mintom + sim$pax_mass)]]

    # For those observations only
    if (length(i) > 0) {

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

      # Save the iteration
      set(x = dt_tko, i = i, j = "itr", value = dt_tko[i, itr] + 1L)

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

      # Remove the existing cL and cD values
      set(x = dt_tko, i = i, j = "cD", value = NA)
      set(x = dt_tko, i = i, j = "cL", value = NA)

      # Add the calibration data (cD and cL) again for the new mass
      dt_tko[dt_cal, cD := ifelse(is.na(cD), i.cD, cD), on = c("type", "m")]
      dt_tko[dt_cal, cL := ifelse(is.na(cL), i.cL, cL), on = c("type", "m")]

      # Calculate the weight force W in N
      set(x = dt_tko, i = i, j = "W", value = sim$g * dt_tko[i, m])
      
      # Calculate the liftoff speed in m/s
      # Adapted from Blake (2009).
      set(
        x = dt_tko,
        i = i,
        j = "Vlof",
        value = sqrt(
          dt_tko[i, W] / (.5 * dt_tko[i, rho] * dt_tko[i, S] * dt_tko[i, cL])
        ) * sim$vs_to_vlof
      )

      # ========================================================================
      # 2.5.2 Calculate the takeoff distance required TODR in m
      # ========================================================================

      set(
        x = dt_tko,
        i = i,
        j = "todr",
        value = fn_todr(DT = dt_tko[i, ], mode = "sim")
      )

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
# fwrite(dt_tko[, ..cols])

  # Connect the worker to the database
  db_con <- dbConnect(
    RMySQL::MySQL(),
    default.file = db$cnf,
    group = db$grp)

  # Write the data to the database
  dbWriteTable(
    conn = db_con,
    name = tolower(db$tko), value = dt_tko[, ..cols],
    append = TRUE,
    row.names = FALSE
  )

  # Disconnect the worker from the database
  dbDisconnect(db_con)

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

} # End of the fn_simulate function

# ==============================================================================
# 3 Run the simulation across multiple cores
# ==============================================================================

# Distribute the work across the cluster
# fn_par_lapply(
#   crs = 23L,
#   pkg = c("data.table", "DBI", "stringr"),
#   lst = dt_apt[, icao],
#   fun = fn_simulate
# )

# FOR TESTING ONLY
# fn_simulate("KABQ")
fn_simulate("KILM")

# ==============================================================================
# 4 Index the database table
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build the query to create the index
db_qry <- paste(
  "CREATE INDEX idx ON",
  tolower(db$tko),
  "(exp, type, icao);",
  sep = " "
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# ==============================================================================
# 5 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF
