# ==============================================================================
#    NAME: scripts/5_transform.R
#   INPUT: 2,213,829,660 long climate observations read from the dat$imp table
# ACTIONS: Pivot the data
#          Calculate the air density, wind vector, and active runway
#  OUTPUT: 442,765,932 wide climate observations written to the dat$cli table
# RUNTIME: ~10 hours
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2022
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load the required libraries
library(data.table)
library(DBI)
library(parallel)
library(stringr)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# Set the number of CPU cores for parallel processing
crs <- 12L

# ==============================================================================
# 1 Fetch the data that we need
# ==============================================================================

# Fetch the list of airports and runways in the sample
dt_smp <- fn_sql_qry(
  statement = paste(
    "SELECT
      icao,
      lat,
      lon,
      zone,
      rwy,
      toda
    FROM ",
    tolower(dat$pop),
    "WHERE traffic >",
    sim$pop_thr,
    ";",
    sep = " "
  )
)

# Recast column types
set(x = dt_smp, j = "icao", value = as.factor(dt_smp[, icao]))
set(x = dt_smp, j = "zone", value = as.factor(dt_smp[, zone]))

# Convert the runway's name (e.g., RW26R) to its magnetic heading in degrees
# (e.g., 260) for later headwind calculation
dt_smp[, hdg := as.numeric(substr(rwy, 3L, 4L)) * 10L]

# For two runways with the same magnetic heading at a given airport (e.g. RWY26R
# and RWY26L), keep the one with the longest TODA (i.e. the most favorable case)
dt_smp <- dt_smp[, .SD[which.max(toda)], by = .(icao, hdg)]

# Return the resulting count of runways to the console
nrow(dt_smp)

# ==============================================================================
# 2 Set up the database table to store the results in wide format
# ==============================================================================

# Drop the table if it exists
fn_sql_qry(
  statement = paste("DROP TABLE IF EXISTS ", tolower(dat$cli), ";", sep = "")
)

# Create the table
fn_sql_qry(
  statement = paste(
    "CREATE TABLE",
    tolower(dat$cli),
    "(id   INT UNSIGNED NOT NULL AUTO_INCREMENT,
    year YEAR NOT NULL,
    obs  DATETIME NOT NULL,
    icao CHAR(4) NOT NULL,
    lat  FLOAT NOT NULL,
    lon  FLOAT NOT NULL,
    zone CHAR(11) NOT NULL,
    ssp  CHAR(6) NOT NULL,
    hurs FLOAT NOT NULL,
    ps   FLOAT NOT NULL,
    tas  FLOAT NOT NULL,
    rho  FLOAT NOT NULL,
    hdw  FLOAT NOT NULL,
    rwy  CHAR(5) NOT NULL,
    toda SMALLINT NOT NULL,
    PRIMARY KEY (id));",
    sep = " "
  )
)

# ==============================================================================
# 3 Transform the climate data for each airport
# ==============================================================================

fn_transform <- function(apt) {

  # Offset the start of each worker by a random duration to spread disk I/O load
  Sys.sleep(time = sample(x = 1L:(crs * 10L), size = 1L))

  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid",
      stringr::str_pad(
        Sys.getpid(),
        width = 5L,
        side  = "left",
        pad   = " "
      ),
      apt,
      "(1/6) Fetching climate observations...",
      sep = " "
    )
  )

  # ============================================================================
  # 3.1 Fetch and prepare the climate data for the current airport
  # ============================================================================

  # Fetch the climate data for the current airport
  dt_nc <- fn_sql_qry(
    statement = paste(
      "SELECT
        obs,
        icao,
        ssp,
        var,
        val
      FROM ",
      tolower(dat$imp),
      " WHERE
        icao = '",
        apt,
      "';",
      # "' LIMIT 100000;",
      sep = ""
    )
  )

  # Recast column types
  set(x = dt_nc, j = "icao", value = as.factor(dt_nc[, icao]))
  set(x = dt_nc, j = "ssp",  value = as.factor(dt_nc[, ssp]))
  set(x = dt_nc, j = "var",  value = as.factor(dt_nc[, var]))

  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid",
      stringr::str_pad(
        Sys.getpid(),
        width = 5L,
        side  = "left",
        pad   = " "
      ),
      apt,
      "(2/6) Pivoting",
      format(x = nrow(dt_nc), big.mark = ","),
      "observations...",
      sep = " "
    )
  )
  
  # Pivot the dataset from long to wide format
  dt_nc <- dcast.data.table(
    data      = dt_nc,
    formula   = obs + icao + ssp ~ var,
    value.var = "val"
  )

  # ============================================================================
  # 3.2 Calculate the air density of moist air at the current airport
  # ============================================================================

  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid",
      stringr::str_pad(
        Sys.getpid(),
        width = 5L,
        side  = "left",
        pad   = " "
      ),
      apt,
      "(3/6) Calculating air density for",
      format(x = nrow(dt_nc), big.mark = ","),
      "observations...",
      sep = " "
    )
  )

  # Polynomial approximation for the saturation vapor pressure at 0°C in mbar
  # Based on the ESW(T) function at https://icoads.noaa.gov/software/other/profs
  pol <- 0.99999683 +
    (dt_nc[, tas] - sim$k_to_c) * (-0.90826951E-02 +
      (dt_nc[, tas] - sim$k_to_c) * (0.78736169E-04 +
        (dt_nc[, tas] - sim$k_to_c) * (-0.61117958E-06 +
          (dt_nc[, tas] - sim$k_to_c) * (0.43884187E-08 +
            (dt_nc[, tas] - sim$k_to_c) * (-0.29883885E-10 +
              (dt_nc[, tas] - sim$k_to_c) * (0.21874425E-12 +
                (dt_nc[, tas] - sim$k_to_c) * (-0.17892321E-14 +
                  (dt_nc[, tas] - sim$k_to_c) * (0.11112018E-16 +
                    (dt_nc[, tas] - sim$k_to_c) * (-0.30994571E-19)))))))))

  # Calculate the partial pressure of water vapor in mbar
  pv <- (sim$sat_ref / pol^8L) * (dt_nc[, hurs] / 100L)

  # Calculate the partial pressure of dry air in mbar
  pd <- dt_nc[, ps] / 100L - pv

  # Calculate the air density in kg/m3
  set(
    x = dt_nc,
    j = "rho",
    value = (
      (pd / (sim$rsp_air * dt_nc[, tas])) + (pv / (sim$rsp_h2o * dt_nc[, tas]))
    ) * 100L
  )

  # ============================================================================
  # 3.3 Merge with the list of runways
  # ============================================================================

  # Extract the list of runways for the current airport
  dt_rwys <- dt_smp[icao == apt, ]

  # Return the Cartesian product of observations times runway headings
  dt_nc <- merge(x = dt_nc, y = dt_rwys, by = "icao", allow.cartesian = TRUE)

  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid",
      stringr::str_pad(
        Sys.getpid(),
        width = 5L,
        side  = "left",
        pad   = " "
      ),
      apt,
      "(4/6) Calculating headwinds for",
      format(x = nrow(dt_nc), big.mark = ","),
      "observations...",
      sep = " "
    )
  )

  # ============================================================================
  # 3.4 Calculate the wind vector for each runway
  # ============================================================================

  # Calculate the airport's wind speed in m/s
  set(x = dt_nc, j = "wnd_spd", value = sqrt(dt_nc[, uas]^2L + dt_nc[, vas]^2L))

  # Calculate the airport's wind direction in °
  set(
    x = dt_nc,
    j = "wnd_dir",
    value = (180L + (180L / pi) * atan2(dt_nc[, uas], dt_nc[, vas])) %% 360L
  )
  
  # Calculate each runway's headwind speed in m/s
  set(
    x = dt_nc,
    j = "hdw",
    value = dt_nc[, wnd_spd] * cos(abs(dt_nc[, hdg] - dt_nc[, wnd_dir])
      * pi / 180L)
  )

  # Keep only the runway with the maximum headwind speed (presumed to be
  #  the active runway) for each observation and experiment (SSP)
  dt_nc <- dt_nc[, .SD[which.max(hdw)], by = .(obs, ssp)]

  # ============================================================================
  # 3.4 Write the data in wide format to the database
  # ============================================================================

  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid",
      stringr::str_pad(
        Sys.getpid(),
        width = 5L,
        side  = "left",
        pad   = " "
      ),
      apt,
      "(5/6) Writing",
      format(x = nrow(dt_nc), big.mark = ","),
      "observations to the database...",
      sep = " "
    )
  )

  # Create the year column
  set(
    x = dt_nc,
    j = "year",
    value = format.Date(x = dt_nc[, obs], format = "%Y")
  )

  # Select which columns to write to the database and in which order
  cols <- c(
    "year",
    "obs",
    "icao",
    "lat",
    "lon",
    "zone",
    "ssp",
    "hurs",
    "ps",
    "tas",
    "rho",
    "hdw",
    "rwy",
    "toda"
  )

  # Connect the worker to the database
  conn <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

  # Write the data
  dbWriteTable(
    conn      = conn,
    name      = tolower(dat$cli),
    value     = dt_nc[, ..cols],
    append    = TRUE,
    row.names = FALSE
  )

  # Disconnect the worker from the database
  dbDisconnect(conn)

  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid",
      stringr::str_pad(
        Sys.getpid(),
        width = 5L,
        side  = "left",
        pad   = " "
      ),
      apt,
      "(6/6) Written",
      format(x = nrow(dt_nc), big.mark = ","),
      "observations to the database.",
      sep = " "
    )
  )

} # End of the fn_transform function

# ==============================================================================
# 4 Handle the parallel computation
# ==============================================================================

# Distribute the sample airports across the CPU cores
fn_par_lapply(
  crs = crs,
  pkg = c("data.table", "DBI", "stringr"),
  lst = unique(dt_smp[, icao], by = "icao"),
  fun = fn_transform
)

# ==============================================================================
# 5 Index the database table
# ==============================================================================

# Create the index
fn_sql_qry(
  statement = paste(
    "CREATE INDEX idx ON",
    tolower(dat$cli),
    "(year, icao, zone, ssp);", sep = " "
  )
)

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Stop the script timer
Sys.time() - start_time

# EOF
