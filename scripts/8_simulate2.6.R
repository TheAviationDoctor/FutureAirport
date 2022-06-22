# ==============================================================================
#    NAME: scripts/8_simulate.R
#   INPUT: 442,769,456 rows of climatic observations from the database
# ACTIONS: Assemble the aircraft, calibration, and climate data
#          Perform simulated takeoffs for each aircraft type and climate obs.
#          Write the resulting takeoff distance required to the database
#          Index the database table
#  OUTPUT: 442,769,456 rows of takeoff data written to the database
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2022
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
source("scripts/6_model2.6.R")

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
db_con <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

# Build the query to create the table
# Preserve the table if it exists so as to break up script execution into chunks
db_qry <- paste(
"CREATE TABLE IF NOT EXISTS",
  tolower(dat$tko),
  "(id INT UNSIGNED NOT NULL AUTO_INCREMENT,
    obs DATETIME NOT NULL,
    icao CHAR(4) NOT NULL,
    exp CHAR(6) NOT NULL,
    type CHAR(4) NOT NULL,
    hurs FLOAT NOT NULL,
    ps FLOAT NOT NULL,
    tas FLOAT NOT NULL,
    rho FLOAT NOT NULL,
    hdw FLOAT NOT NULL,
    rwy CHAR(5) NOT NULL,
    toda SMALLINT NOT NULL,
    todr SMALLINT NOT NULL,
    vlof SMALLINT NOT NULL,
    thr_red SMALLINT NOT NULL,
    tom_red SMALLINT NOT NULL,
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

# Load the file to a data table
dt_act <- fread(
  file = fls$act,
  header = TRUE,
  colClasses = c(rep("factor", 2), rep("integer", 5), rep("numeric", 5)),
  key = "type"
)

# Set the starting takeoff mass as the maximum mass
setnames(x = dt_act, "tom_max", "tom")

# Keep only needed columns
dt_act <- dt_act[, .(type, n, slst, bpr, s, tom)]

# ==============================================================================
# 1.2.2 Import the takeoff performance calibration data
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

# Build a query to retrieve the calibration data for the aircraft to simulate
db_qry <- paste("SELECT type, tom, todr_cal, cllof, cd FROM ",
  tolower(dat$cal),
  " WHERE type IN (", paste("'", act, "'", collapse = ", ", sep = ""),
  ");",
  sep = ""
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a data table
dt_cal <- suppressWarnings(
  setDT(
    dbFetch(db_res, n = Inf),
    key = c("type", "tom")
  )
)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# Convert the type column to factor
dt_cal[, type := as.factor(type)]

# Order by type and descending mass
dt_cal <- dt_cal[order(type, -rank(tom))]

# Set the minimum mass for which there is a calibrated TODR
dt_cal[, tom_belf := min(tom), by = type]

# ==============================================================================
# 1.2.3 Import the list of sample airports, excluding those already processed
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

# Build a query to retrieve the sample airports above the traffic threshold
db_qry <- paste(
  "SELECT DISTINCT icao FROM", tolower(dat$pop),
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
db_con <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

# Build a query to check if simulation outputs already exists in the database
db_qry <- paste(
  "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = '",
  tolower(dat$grp), "' AND table_name = '", tolower(dat$tko), "';",
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
  db_qry <- paste("SELECT DISTINCT icao FROM", tolower(dat$tko), ";", sep = " ")

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

# Unload the obsolete variables from memory
rm(db_con, db_qry, db_res, dt_exc, dir, fls, tbl_exists)

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
  db_con <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

  # Build a query to retrieve the takeoff conditions
  db_qry <- paste(
    "SELECT obs, exp, icao, hurs, ps, tas, rho, hdw, rwy, toda FROM ", dat$cli,
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
  # 2.2 Combine the airport, aircraft, calibration, and climate data
  # ============================================================================

  # Combine climatic observations with aircraft data (Cartesian product)
  dt_tko <- dt_cli[, as.list(dt_act), by = dt_cli]

  # Unload the climatic observations from memory
  rm(dt_cli)

  # Convert the airport code to a factor
  dt_tko[, icao := as.factor(icao)]

  # Combine climatic observations with calibration data using the starting mass
  dt_tko <- dt_cal[dt_tko, on = c("type", "tom")]

  # ============================================================================
  # 2.3 Initialize takeoff parameters for the first simulation iteration
  # ============================================================================
  
  # Initialize the thrust reduction. If the TODA is shorter than the calibrated
  # TODR at MTOM (which was calibrated using TOGA thrust), then it is unlikely
  # that a thrust-reduced takeoff could lead to TODR < TODA (short of a few
  # cases of better-than-ISA takeoff conditions). In this case, set the thrust
  # to TOGA, to save on takeoff iterations. Otherwise, set it to the lowest
  # takeoff thrust permissible by regulations.
  set(
    x = dt_tko,
    j = "thr_red",
    value = ifelse(
      dt_tko[, toda] <= dt_tko[, todr_cal],
      0L + sim$thr_inc,
      sim$thr_ini + sim$thr_inc
    )
  )

  # Initialize a column to track how much takeoff mass was removed
  set(x = dt_tko, j = "tom_red", value = 0L)

  # Initialize the starting TODR to a value greater than the max TODA
  set(x = dt_tko, j = "todr", value = dt_tko[, toda] + 1L)

  # Initialize a counter to track the number of iterations of each takeoff
  set(x = dt_tko, j = "itr", value = 0L)

  # Set the horizontal airborne component of the TODR in m
  # Adapted from Gratton et al, 2020
  set(x = dt_tko, j = "dis_air_sim", value = fn_dis_air())

  # ============================================================================
  # 2.4 Perform vectorized takeoff simulations iteratively until TODR < TODA
  # ============================================================================

  repeat {

    # ==========================================================================
    # 2.4.1 Prepare the data
    # ==========================================================================

    # Retrieve indices of observations where TODR > TODA and the current mass is
    # not less than the minimum mass for which there is calibrated data
    i <- dt_tko[, .I[todr > toda & tom >= (tom_belf + sim$pax_avg)]]

    # As long as there are takeoffs that meet these conditions
    if (length(i) > 0L) {

      # Save the iteration
      set(x = dt_tko, i = i, j = "itr", value = dt_tko[i, itr] + 1L)

      # If thrust is already at TOGA, then decrease the mass by the standard
      set(
        x = dt_tko,
        i = i,
        j = "tom",
        value = ifelse(
          dt_tko[i, thr_red] == 0L,
          dt_tko[i, tom] - sim$pax_avg,
          dt_tko[i, tom]
        )
      )

      # And also keep track of how much mass was removed
      set(
        x = dt_tko,
        i = i,
        j = "tom_red",
        value = ifelse(
          dt_tko[i, thr_red] == 0L,
          dt_tko[i, tom_red] + sim$pax_avg,
          dt_tko[i, tom_red]
        )
      )

      # Otherwise decrease the thrust reduction incrementally up to TOGA
      set(
        x = dt_tko,
        i = i,
        j = "thr_red",
        value = ifelse(
          dt_tko[i, thr_red] > 0L,
          dt_tko[i, thr_red] - sim$thr_inc,
          dt_tko[i, thr_red]
        )
      )

      # Inform the log file
      print(
        paste(
          Sys.time(),
          "pid", str_pad(Sys.getpid(), width = 5L, side = "left", pad = " "),
          "icao", icao,
          "itr", str_pad(
            dt_tko[i, mean(itr)], width = 3L, side = "left", pad = " "
          ),
          "t/o =", str_pad(
            format(length(i), big.mark = ","),
            width = 9, side = "left", pad = " "
          ),
          sep = " "
        )
      )

      # Remove the existing cL and cD values
      set(x = dt_tko, i = i, j = "cd", value = NA)
      set(x = dt_tko, i = i, j = "cllof", value = NA)

      # Add the calibration data (cD and cL) again for the new mass
      dt_tko[dt_cal, cd := ifelse(is.na(cd), i.cd, cd), on = c("type", "tom")]
      dt_tko[
        dt_cal,
        cllof := ifelse(
          is.na(cllof),
          i.cllof, cllof
        ),
        on = c("type", "tom")
      ]

      # Calculate the liftoff speed in m/s
      set(x = dt_tko, i = i, j = "vlof", value = fn_vlof(DT = dt_tko[i, ]))

      # ========================================================================
      # 2.4.2 Calculate the takeoff distance required TODR in m
      # ========================================================================

      # Calculate the ground component of the TODR in m
      set(
        x = dt_tko,
        i = i,
        j = "dis_gnd_sim",
        value = fn_dis_gnd(DT = dt_tko[i, ])
      )

      # Calculate the regulatory component of the TODR in m
      set(
        x = dt_tko,
        i = i,
        j = "dis_reg_sim",
        value = (dt_tko[i, dis_gnd_sim] + dt_tko[i, dis_air_sim]) *
          (sim$tod_mul - 1L)
      )

      # Calculate the total TODR in m, rounded to the nearest higher integer
      set(
        x = dt_tko,
        i = i,
        j = "todr",
        value = ceiling(dt_tko[i, dis_gnd_sim] + dt_tko[i, dis_air_sim] +
          dt_tko[i, dis_reg_sim])
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
        width = 9L, side = "left", pad = " "
      ),
      "rows to the database",
      sep = " "
    )
  )

  # Select which columns to write to the database and in which order
  cols <- c(
    "obs", "icao", "exp", "type", "hurs", "ps", "tas", "rho", "hdw", "rwy",
    "toda", "todr", "vlof", "thr_red", "tom_red", "itr"
  )

  # Connect the worker to the database
  db_con <- dbConnect(
    RMySQL::MySQL(),
    default.file = dat$cnf,
    group = dat$grp)

  # Write the data to the database
  dbWriteTable(
    conn = db_con,
    name = tolower(dat$tko), value = dt_tko[, ..cols],
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
  crs = sim$cpu_crs,
  pkg = c("data.table", "DBI", "stringr"),
  lst = dt_apt[, icao],
  fun = fn_simulate
)

# ==============================================================================
# 4 Index the database table
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

# Build the query to create the index
db_qry <- paste(
  "CREATE INDEX idx ON",
  tolower(dat$tko),
  "(obs, exp, type, icao);",
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
