# ==============================================================================
#    NAME: scripts/5_transform.R
#   INPUT: 2,213,847,280 rows of of climate data from the database
# ACTIONS: Pivot the climate data from long to wide
#          Calculate the air density, wind vector, and active runway
#          Write the resulting dataset to the database
#          Index the database table
#  OUTPUT: 442,769,456 rows of runway conditions written to the database
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

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Import the sample airports and runways
# ==============================================================================

# Fetch the sample airports and runways
dt_smp <- fn_sql_qry(
  statement = paste(
    "SELECT icao, rwy, toda, zone FROM ", dat$pop,
    " WHERE traffic > ", sim$pop_thr, ";",
    sep = ""
  )
)

# Convert the runway's name (e.g., RW26R) to its magnetic heading in degrees
# (e.g., 260) for later headwind calculation
dt_smp[, hdg := as.numeric(substr(rwy, 3L, 4L)) * 10L]

# For two runways with the same magnetic heading at a given airport (e.g. RWY26R
# and RWY26L), keep the one with the longest TODA (i.e. the most favorable case)
dt_smp <- dt_smp[, .SD[which.max(toda)], by = .(icao, hdg)]

# Count the resulting number of runways
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
    "(id INT UNSIGNED NOT NULL AUTO_INCREMENT,
    obs DATETIME NOT NULL,
    icao CHAR(4) NOT NULL,
    zone CHAR(10) NOT NULL,
    exp CHAR(6) NOT NULL,
    hurs FLOAT NOT NULL,
    ps FLOAT NOT NULL,
    tas FLOAT NOT NULL,
    rho FLOAT NOT NULL,
    hdw FLOAT NOT NULL,
    rwy CHAR(5) NOT NULL,
    toda SMALLINT NOT NULL,
    PRIMARY KEY (id));",
    sep = " "
  )
)

# ==============================================================================
# 3 Define a function to transform the climate data for each airport
# ==============================================================================

fn_transform <- function(apt) {

  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      "Parsing airport", apt,
      sep = " "
    )
  )

  # ============================================================================
  # 3.1 Retrieve the climate data at the current airport
  # ============================================================================

  # Fetch the sample airports
  fn_sql_qry(
    statement = paste(
      "SELECT obs, icao, exp, var, val FROM ", dat$imp,
      " WHERE icao = '", apt, "';",
      sep = ""
    )
  )
  
  # Create keys on the data table
  setkey(x = dt_nc, obs, exp)

  # Pivot the dataset from long to wide/tidy format
  dt_nc <- dcast.data.table(dt_nc, obs + icao + exp ~ var, value.var = "val")

  # Extract the runways for the current airport
  dt_rwys <- dt_smp[icao == apt, ]

  # Return the cartesian product of observations times runway headings
  dt_nc <- merge(x = dt_nc, y = dt_rwys, by = "icao", allow.cartesian = TRUE)

  # ============================================================================
  # 3.2 Calculate the air density of moist air at the current airport
  # Replicate in R the FORTRAN polynomial approximation function ESW(T)
  #  for the saturation vapor pressure in mb over liquid water at 0°C
  #  found at https://icoads.noaa.gov/software/other/profs by Herman Wobus
  # ============================================================================

  # Inputs
  hurs <- dt_nc[, hurs] / 100 # Near-surface relative humidity in %
  ps <- dt_nc[, ps] / 100 # Near-surface air pressure in mbar
  tas <- dt_nc[, tas] # Near-surface air temperature in K
  es0 <- 6.1078 # Ref. saturation vapor pressure at 0°C in mbar
  Rd <- 287.058 # Specif. gas constant for dry air, in J/(kg·K)
  Rv <- 461.495 # Specif. gas constant for water vapor in J/(kg·K)
  pol <- 0.99999683 + # Requires tas in °C
    (tas - 273.15) * (-0.90826951E-02 +
      (tas - 273.15) * (0.78736169E-04 +
        (tas - 273.15) * (-0.61117958E-06 +
          (tas - 273.15) * (0.43884187E-08 +
            (tas - 273.15) * (-0.29883885E-10 +
              (tas - 273.15) * (0.21874425E-12 +
                (tas - 273.15) * (-0.17892321E-14 +
                  (tas - 273.15) * (0.11112018E-16 +
                    (tas - 273.15) * (-0.30994571E-19)))))))))

  # Outputs
  esw <- es0 / pol^8 # Saturation vapor pressure at tas, in mbar
  pv <- esw * hurs # Partial pressure of water vapor, in mbar
  pd <- ps - pv # Partial pressure of dry air, in mbar
  rho <- ((pd / (Rd * tas)) + (pv / (Rv * tas))) * 100 # Air density in kg/m3

  # Save the air density to a new column
  dt_nc[, rho := rho]

  # ============================================================================
  # 3.3 Calculate the wind vector at the current airport
  # ============================================================================

  # Calculate and write the airport's wind speed to a new column
  dt_nc[, wnd_spd := sqrt(uas^2 + vas^2)]

  # Calculate and write the airport's wind direction to a new column
  dt_nc[, wnd_dir := (180 + (180 / pi) * atan2(uas, vas)) %% 360]

  # Calculate and write each runway's headwind speed (in m/s) to a new column
  dt_nc[, hdw := wnd_spd * cos(abs(hdg - wnd_dir) * pi / 180)]

  # Keep only the runway with the maximum headwind speed (presumed to be
  # the active runway) for each observation and experiment.
  dt_nc <- dt_nc[, .SD[which.max(hdw)], by = .(obs, exp)]

  # ============================================================================
  # 3.4 Write the data in tidy format to the database
  # ============================================================================

  # Select which columns to write to the database and in which order
  cols <- c(
    "obs", "icao", "exp", "hurs", "ps", "tas", "uas", "vas", "rho", "wnd_dir",
    "wnd_spd", "hdw", "rwy", "toda"
  )

  # Write the data
  dbWriteTable(
    conn = db_con,
    name = tolower(dat$cli),
    value = dt_nc[, ..cols],
    append = TRUE,
    row.names = FALSE
  )

  # Disconnect the worker from the database
  dbDisconnect(db_con)
  
} # End of the fn_transform function

# ==============================================================================
# 4 Handle the parallel computation across multiple cores
# ==============================================================================

# Set the number of workers to use in the cluster
cores <- 12L

# Clear the log file
close(file(description = f$out, open = "w"))

# Build the cluster of workers
cl <- makeCluster(spec = cores, outfile = f$out)

# Have each worker load the libraries that they need
clusterEvalQ(cl, {
  library(data.table)
  library(DBI)
  library(ncdf4)
  library(ncdf4.helpers)
  library(PCICt)
})

# Pass the required variables from the main scope to the workers' scope
clusterExport(cl = cl, varlist = c("db", "dt_smp"))

# Distribute the unique airports across the workers
parLapply(cl = cl, X = unique(dt_smp[, icao], by = "icao"), fun = fn_transform)

# Terminate the cluster once finished
stopCluster(cl)

# ==============================================================================
# 5 Add an index to the database table
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

# Build the query to create the index
db_qry <- paste("CREATE INDEX idx ON ", tolower(dat$cli), " (icao);", sep = "")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF
