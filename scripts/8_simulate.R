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
library(zoo)

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
    mtom MEDIUMINT NOT NULL,
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
    i SMALLINT UNSIGNED NOT NULL,
    PRIMARY KEY (id));",
  sep = " "
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

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
# 1.2.2 Import the takeoff performance calibration data
# ==============================================================================

# Build a query to retrieve the calibration data
db_qry <- paste("SELECT type, m, cl, cd FROM", tolower(db$cal), ";", sep = " ")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a data table
dt_cal <- suppressWarnings(
  setDT(
    dbFetch(db_res, n = Inf),
    key = c("type", "m")
  )
)

# Convert the type column to factor
dt_cal[, type := as.factor(type)]

# Restore proper capitalization given that MySQL stores columns in lowercase
setnames(dt_cal, c("cl", "cd"), c("cL", "cD"))

# Release the database resource
dbClearResult(db_res)

# ==============================================================================
# 1.2.3 Import the list of sample airports, excluding those already processed
# ==============================================================================

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
    " WHERE icao = '", icao, "' LIMIT 1000;",
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
  # 2.2 Combine climatic observations with aircraft and calibration data
  # ============================================================================

  # Combine climatic observations with aircraft data (Cartesian product)
  dt_tko <- dt_cli[, as.list(dt_act), by = dt_cli]

  # Remove the climatic observations from the environment to free up memory
  rm(dt_cli)

  # Convert airport code to a factor
  dt_tko[, icao := as.factor(icao)]

  # Calculate the speed of sound in m/s for the given temperature in dry air
  set(x = dt_tko, j = "Vsnd", value = sqrt(sim$gamma * sim$Rd * dt_tko[, tas]))
  
  # Calculate the air pressure ratio
  set(x = dt_tko, j = "dP", value = dt_tko[, ps] / sim$ps_isa)

  # Calculate the coefficients of thrust for the given air pressure ratio
  set(x = dt_tko, j = "G0", value = .0606 * dt_tko[, bpr] + .6337)
  set(x = dt_tko, j = "A", value = -.4327 * dt_tko[, dP]^2 + 1.3855 * dt_tko[, dP] + .0472)
  set(x = dt_tko, j = "Z", value = .9106 * dt_tko[, dP]^3 - 1.7736 * dt_tko[, dP]^2 + 1.8697 * dt_tko[, dP])
  set(x = dt_tko, j = "X", value = .1377 * dt_tko[, dP]^3 - .4374 * dt_tko[, dP]^2 + 1.3003 * dt_tko[, dP])

  # Initialize the starting mass in kg to the maximum takeoff mass
  set(x = dt_tko, j = "m", value = dt_tko[, mtom])
  
  # Maximum percentage of thrust reduction permissible under FAA AC 25-13 (1988)
  # Start 1 p.p. above max value because loop starts by decrementing rto by 1.
  # dt_tko[, rto := reg_rto + 1L]
  set(x = dt_tko, j = "rto", value = 26L)
  
  # Safety margin in percent applied to the horizontal distance along the takeoff
  # path assuming all engines operating, from the start of the takeoff to a point
  # equidistant between the point at which VLOF is reached and the point at which
  # the airplane is 35 ft above the surface, according to 14 CFR § 25.113 (1998).
  set(x = dt_tko, j = "sft", value = 1.15)

  # Initialize the starting TODR to an arbitrary value greater than the max TODA
  # This is so that the repeat loop captures all observations in its first pass.
  # dt_tko[, todr := max(toda) + 1L]
  set(x = dt_tko, j = "todr", value = 9999L)

  # Initialize a counter to track the number of iterations of each takeoff
  # dt_tko[, itr := 0L]
  set(x = dt_tko, j = "itr", value = 0L)

  # Combine climatic observations with calibration data (cL and cD)
  dt_tko <- dt_cal[dt_tko, on = c("type", "m")]

  # Add the airport's latitude and longitude to the table for later analysis
  # dt_tko <- dt_apt[dt_tko, on = c("icao")]

  # ============================================================================
  # 2.3 Perform vectorized takeoff simulations iteratively until TODR < TODA
  # ============================================================================

  # Initialize a counter only to keep track of the number of iterations
  itr <- 0L
  
  repeat {

    # ==========================================================================
    # 2.3.1 Prepare the data
    # ==========================================================================

    # Retrieve indices of observations where TODR > TODA
    i <- dt_tko[, .I[todr > toda]]

    # For those observations only
    if (length(i) > 0) {

      # Inform the log file
      print(
        paste(
          Sys.time(),
          "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
          "icao", icao,
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
      dt_tko[i, m := ifelse(rto == 0L, m - 87L, m)]

      # Otherwise increase thrust by 1 percentage point (up to TOGA)
      dt_tko[i, rto := ifelse(rto > 0L, rto - 1L, rto)]

      # Remove the existing cL and cD values
      set(x = dt_tko, i = i, j = "cD", value = NA)
      set(x = dt_tko, i = i, j = "cL", value = NA)

      # Add the calibration data (cD and cL) again for the new mass
      dt_tko[dt_cal, cD := ifelse(is.na(cD), i.cD, cD), on = c("type", "m")]
      dt_tko[dt_cal, cL := ifelse(is.na(cL), i.cL, cL), on = c("type", "m")]

      # Calculate the weight force W in N
      set(x = dt_tko, i = i, j = "W", value = sim$g * dt_tko[i, m])
      
      # Calculate the speed Vlof in m/s at which lift L equals weight W
      # Adapted from Blake (2009).
      set(
        x = dt_tko,
        i = i,
        j = "Vlof",
        value = sqrt(
          dt_tko[i, W] / (.5 * dt_tko[i, rho] * dt_tko[i, S] * dt_tko[i, cL])
        )
      )

      # ========================================================================
      # 2.3.2 Calculate the takeoff distance required TODR in m
      # ========================================================================

      # set(x = dt_tko, i = i, j = "todr", value = fn_todr(dt_tko[i, ]))
      set(x = dt_tko, i = i, j = "todr", value = fn_todr(DT = dt_tko[i, ], mode = "sim"))

    } else { # Once there are no more observations where TODR > TODA

      # End the repeat loop
      break

    } # End if-else

  } # End repeat

fwrite(x = dt_tko, file = "test.csv")

  # ========================================================================
  # 2.4 Write results to the database
  # ========================================================================
  
  # Select which columns to write to the database and in which order
  # cols <- c(
  #   "obs", "icao", "lat", "lon", "exp", "type", "mtom", "hurs", "ps",
  #   "tas", "rho", "hdw", "rwy", "toda", "m", "rto", "todr", "i"
  # )
  # 
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
  # # Inform the log file
  # print(
  #   paste(
  #     Sys.time(),
  #     "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
  #     "icao", icao,
  #     "Wrote", nrow(dt_tko), "rows to the database",
  #     sep = " "
  #   )
  # )
  # 
  # # Disconnect the worker from the database
  # dbDisconnect(db_con)

} # End of the fn_simulate function

# ==============================================================================
# 3 Handle the parallel computation across multiple cores
# ==============================================================================

# # Remove unneeded variables from the global environment
# rm(list = c("db_qry", "tbl_exists", "db_con", "db_res", "dt_exc"))
# 
# # Distribute the work across the cluster
# fn_par_lapply(
#   crs = 4L,
#   pkg = c("data.table", "DBI", "stringr"),
#   lst = as.list(dt_apt[, icao]),
#   fun = fn_simulate
# )

fn_simulate("OMDB")

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
