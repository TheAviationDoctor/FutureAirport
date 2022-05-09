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
source("scripts/6_model2.0.R")

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

# # Connect to the database
# db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)
# 
# # FOR TESTING ONLY
# db_qry <- paste("DROP TABLE IF EXISTS", tolower(db$tko), ";", sep = " ")
# db_res <- dbSendQuery(db_con, db_qry)
# dbClearResult(db_res)
# 
# # Build the query to create the table
# # Preserve the table if it exists so as to break up script execution into chunks
# db_qry <- paste(
# "CREATE TABLE IF NOT EXISTS",
#   tolower(db$tko),
#   "(id INT UNSIGNED NOT NULL AUTO_INCREMENT,
#     obs DATETIME NOT NULL,
#     icao CHAR(4) NOT NULL,
#     lat FLOAT NOT NULL,
#     lon FLOAT NOT NULL,
#     exp CHAR(6) NOT NULL,
#     type CHAR(4) NOT NULL,
#     hurs FLOAT NOT NULL,
#     ps FLOAT NOT NULL,
#     tas FLOAT NOT NULL,
#     rho FLOAT NOT NULL,
#     hdw FLOAT NOT NULL,
#     rwy CHAR(5) NOT NULL,
#     toda SMALLINT NOT NULL,
#     todr SMALLINT NOT NULL,
#     vlof SMALLINT NOT NULL,
#     itr SMALLINT UNSIGNED NOT NULL,
#     rto SMALLINT NOT NULL,
#     rm MEDIUMINT NOT NULL,
#     PRIMARY KEY (id));",
#   sep = " "
# )
# 
# # Send the query to the database
# db_res <- dbSendQuery(db_con, db_qry)
# 
# # Release the database resource
# dbClearResult(db_res)
# 
# # Disconnect from the database
# dbDisconnect(db_con)

# ==============================================================================
# 1.2 Import the simulation data
# ==============================================================================

# ==============================================================================
# 1.2.1 Import the aircraft characteristics (from Sun et al., 2020)
# ==============================================================================

# Load the file
dt_act <- fread(
  file = f$act,
  header = TRUE,
  colClasses = c(rep("factor", 3), rep("integer", 3), rep("numeric", 7)),
  key = "type"
)

# Reduce the file to the aircraft selected for simulation
dt_act <- dt_act[type %in% sim$act_sim]

# ==============================================================================
# 1.2.2 Import the takeoff performance calibration data
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build a query to retrieve the calibration data for the aircraft to simulate
# db_qry <- paste("SELECT type, m, cl, cd, todr_sim, todr_cal FROM ",
db_qry <- paste("SELECT type, m, cl, cd, todr_cal FROM ",
  tolower(db$cal),
  " WHERE type IN (", paste("'", sim$act_sim, "'", collapse = ", ", sep = ""),
  ");",
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
  # "WHERE traffic >", sim$pop_thr, ";",
  # FOR TESTING ONLY  
  # "WHERE icao IN ('KIAH', 'KELP', 'KOKC', 'MMCS', 'KMEM', 'MMTJ', 'KAUS', 'MMCU', 'KLIT', 'KRNO', 'KXNA', 'KMDW', 'KHOU', 'KORD', 'KDFW', 'MMHO', 'KLBB', 'SBPA', 'KBHM', 'KMFR', 'KMSY', 'KSMF', 'KHSV', 'SBCG', 'KTUL', 'KPDX', 'KATL', 'MMMY', 'KCHA', 'SARI', 'KECP', 'SBFI', 'KVPS', 'KBUR', 'KSAV', 'KLAX', 'KCAE', 'KLGB', 'KSGF', 'KSNA', 'KMAF', 'MMMZ', 'KCLT', 'SBGL', 'KBNA', 'SBRJ', 'WAQQ', 'SPZO', 'ZMUB', 'WMKL', 'VOPB', 'ZSLQ', 'LEMD', 'URSS', 'SKMD');",
  "WHERE icao IN ('KIAH', 'KELP', 'KOKC', 'MMCS', 'KMEM', 'MMTJ', 'KAUS', 'MMCU', 'KLIT');",
  # "WHERE icao = 'KAUS';",
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

# Remove from memory the variables that are no longer needed
rm(d, db_con, db_qry, db_res, dt_exc, f, tbl_exists)

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
    # " WHERE icao = '", icao, "';",
    # FOR TESTING ONLY
    # " WHERE icao = '", icao, "' AND obs = '2015-01-01 06:00:00' AND exp = 'ssp126';",
    " WHERE obs = '2015-01-01 06:00:00' AND exp = 'ssp126';",
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

  # Remove from memory the climatic observations from the environment
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
  # 2.3 Initialize takeoff parameters for the first simulation iteration
  # ============================================================================
  
  # Initialize the thrust reduction to the maximum permissible
  # set(x = dt_tko, j = "rto", value = sim$thrst_start + sim$thrst_incr)

  # FOR TESTING
  # Initialize the thrust reduction. If the TODA is shorter than the calibrated
  # TODR at MTOM (which was calibrated using TOGA thrust), then it is unlikely
  # that a thrust-reduced takeoff could lead to TODR < TODA. In this case, set
  # the thrust reduction to zero. Otherwise, set it to the maximum permissible.
  set(
    x = dt_tko,
    j = "rto",
    value = ifelse(
      dt_tko[, toda] <= dt_tko[, todr_cal],
      0L + sim$thrst_incr,
      sim$thrst_start + sim$thrst_incr
    )
  )

  # FOR TESTING ONLY
  # dt_tko[, hurs := 0L]         # ISA
  # dt_tko[, ps   := 101325L]    # ISA
  # dt_tko[, tas  := 273.15]     # ISA
  # dt_tko[, rho  := 1.225]      # ISA
  # dt_tko[, hdw  := 0L]         # ISA
  # dt_tko[, rto  := 0L]         # No takeoff thrust reduction
  # dt_tko[, rto  := 25L]        # Max takeoff thrust reduction
  # dt_tko[, m    := maxtom + sim$pax_mass] # Use this if no takeoff thrust reduction
  # dt_tko[, toda := 9998L]      # Artifical TODA

  # Initialize the starting TODR to an arbitrary value greater than the max TODA
  set(x = dt_tko, j = "todr", value = 9999L)

  # Initialize a counter to track the number of iterations of each takeoff
  set(x = dt_tko, j = "itr", value = 0L)

  # Set the horizontal airborne component of the TODR in m (assumed)
  # Adapted from Gratton et al, 2020
  set(
    x = dt_tko,
    j = "todr_air",
    value = sim$ft_to_m * sim$scrn_hght / cos(sim$climb_angle)
  )

  # ============================================================================
  # 2.4 Perform vectorized takeoff simulations iteratively until TODR < TODA
  # ============================================================================

# FOR TESTING ONLY
# dt_tko <- dt_tko[type == "A20n", ]
# View(dt_tko)

  repeat {

    # ==========================================================================
    # 2.4.1 Prepare the data
    # ==========================================================================

    # Retrieve indices of observations where TODR > TODA and the current mass is
    # not less than the minimum mass for which there is calibrated data
    i <- dt_tko[, .I[todr > toda & m >= (mintom + sim$pax_mass)]]

    # As long as there are takeoffs that meet these conditions
    if (length(i) > 0) {

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

      # Inform the log file
      print(
        paste(
          Sys.time(),
          "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
          "icao", icao,
          "itr", str_pad(dt_tko[i, mean(itr)], width = 3, side = "left", pad = " "),
          # "rto", str_pad(dt_tko[i, mean(rto)], width = 2, side = "left", pad = " "),
          # "m", str_pad(dt_tko[i, mean(m)], width = 6, side = "left", pad = " "),
          "t/o =", str_pad(
            format(length(i), big.mark = ","),
            width = 9, side = "left", pad = " "
          ),
          sep = " "
        )
      )

      # Remove the existing cL and cD values
      set(x = dt_tko, i = i, j = "cD", value = NA)
      set(x = dt_tko, i = i, j = "cL", value = NA)

      # Add the calibration data (cD and cL) again for the new mass
      dt_tko[dt_cal, cD := ifelse(is.na(cD), i.cD, cD), on = c("type", "m")]
      dt_tko[dt_cal, cL := ifelse(is.na(cL), i.cL, cL), on = c("type", "m")]

      # Calculate the liftoff speed Vlof in m/s
      set(x = dt_tko, i = i, j = "Vlof", value = fn_Vlof(DT = dt_tko[i, ]))

      # ========================================================================
      # 2.4.2 Calculate the takeoff distance required TODR in m
      # ========================================================================

      # Calculate the ground component of the TODR in m
      set(
        x = dt_tko,
        i = i,
        j = "todr_gnd",
        value = fn_todr_gnd(DT = dt_tko[i, ])
      )

      # Calculate the regulatory component of the TODR in m
      set(
        x = dt_tko,
        i = i,
        j = "todr_reg",
        value = (dt_tko[i, todr_gnd] + dt_tko[i, todr_air]) * (sim$tod_mul - 1)
      )

      # Calculate the total TODR in m, rounded to the nearest higher integer
      set(
        x = dt_tko,
        i = i,
        j = "todr",
        value = ceiling(dt_tko[i, todr_gnd] + dt_tko[i, todr_air] +
          dt_tko[i, todr_reg])
      )

    } else { # Once there are no more observations that meet the conditions

      break # End the repeat loop

    } # End if-else

  } # End repeat

  # ========================================================================
  # 2.5 Write the simulation results to the database
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

  # Set how much mass was removed
  set(x = dt_tko, j = "rm", value = dt_tko[, maxtom] - dt_tko[, m])

  # Select which columns to write to the database and in which order
  cols <- c(
    "obs", "icao", "lat", "lon", "exp", "type", "hurs", "ps", "tas",
    "rho", "hdw", "rwy", "toda", "todr", "Vlof", "itr", "rto", "rm"
  )

# FOR TESTING ONLY
print(dt_tko[, ..cols])

  # # Connect the worker to the database
  # db_con <- dbConnect(
  #   RMySQL::MySQL(),
  #   default.file = db$cnf,
  #   group = db$grp)
  # 
  # # Write the data to the database
  # dbWriteTable(
  #   conn = db_con,
  #   name = tolower(db$tko), value = dt_tko[, ..cols],
  #   append = TRUE,
  #   row.names = FALSE
  # )
  # 
  # # Disconnect the worker from the database
  # dbDisconnect(db_con)

  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      "icao", icao,
      "Wrote  ",
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
fn_par_lapply(
  crs = 23L,
  pkg = c("data.table", "DBI", "stringr"),
  lst = dt_apt[, icao],
  fun = fn_simulate
)

# FOR TESTING ONLY
# fn_simulate("KABQ")
# fn_simulate("KAUS")

# ==============================================================================
# 4 Index the database table
# ==============================================================================

# # Connect to the database
# db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)
# 
# # Build the query to create the index
# db_qry <- paste(
#   "CREATE INDEX idx ON",
#   tolower(db$tko),
#   "(exp, type, icao);",
#   sep = " "
# )
# 
# # Send the query to the database
# db_res <- dbSendQuery(db_con, db_qry)
# 
# # Release the database resource
# dbClearResult(db_res)
# 
# # Disconnect from the database
# dbDisconnect(db_con)

# ==============================================================================
# 5 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF
