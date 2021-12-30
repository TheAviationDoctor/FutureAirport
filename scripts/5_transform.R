################################################################################
#    NAME: scripts/5_transform.R                                               #
#   INPUT: 2,213,847,280 rows from database table tmp created in 4_import.R    #
# ACTIONS: Pivot the database table imp from long to wide/tidy                 #
#          Calculate the air density, wind vector, and active runway           #
#          Write the resulting dataset to the database table nc                #
#          Index the database table nc to speed up subsequent queries          #
#  OUTPUT: 442,769,456 rows of airport data written to the database table nc   #
# RUNTIME: ~3.8 hours on the researcher's config (https://bit.ly/3ChCBAP)      #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

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

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

################################################################################
# Import the sample airports and runways                                       #
################################################################################

# Build the query to retrieve the sample airports
db_qry <- paste("SELECT icao, rwy, toda FROM ", db_pop, " WHERE traffic > ", pop_thr, ";", sep = "")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results
dt_smp <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "icao", check.names = FALSE))

# Release the database resource
dbClearResult(db_res)

# Convert the runway's name (e.g., RW26R) to its magnetic heading in degrees (e.g., 260) for later headwind calculation
dt_smp[, hdg := as.numeric(substr(rwy, 3, 4)) * 10]

# For two runways with the same magnetic heading at a given airport (e.g., RWY26R and RWY26L), keep only the one with the longest TODA (i.e., the most favorable case)
dt_smp <- dt_smp[, .SD[which.max(toda)], by = .(icao, hdg)]

# Count the resulting number of runways
nrow(dt_smp)

################################################################################
# Set up the database table to store the results in wide/tidy format           #
################################################################################

# Build the query to drop the table, if it exists
db_qry <- paste("DROP TABLE IF EXISTS ", tolower(db_cli), ";", sep = "")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Build the query to create the table
db_qry <- paste("CREATE TABLE", tolower(db_cli), "(id INT UNSIGNED NOT NULL AUTO_INCREMENT, obs DATETIME NOT NULL, icao CHAR(4) NOT NULL, exp CHAR(6) NOT NULL, hurs FLOAT NOT NULL, ps FLOAT NOT NULL, tas FLOAT NOT NULL, uas FLOAT NOT NULL, vas FLOAT NOT NULL, rho FLOAT NOT NULL, wnd_dir FLOAT NOT NULL, wnd_spd FLOAT NOT NULL, hdw FLOAT NOT NULL, rwy CHAR(5) NOT NULL, toda SMALLINT NOT NULL, PRIMARY KEY (id));", sep = " ")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

################################################################################
# Define the function to transform the climate data for each airport           #
################################################################################

fn_transform <- function(apt) {
  
  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), " Worker ", Sys.getpid(), " is parsing airport ", apt, "...", sep = ""))
  
  ##############################################################################
  # Retrieve the climate data at the current airport                           #
  ##############################################################################
  
  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build the query to retrieve the sample airports
  db_qry <- paste("SELECT obs, icao, exp, var, val FROM ", db_imp, " WHERE icao = '", apt, "';", sep = "")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Return the results
  dt_nc <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = c("obs", "exp"), check.names = TRUE))
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Pivot the dataset from long to wide/tidy format
  dt_nc <- dcast.data.table(dt_nc, obs + icao + exp ~ var, value.var = "val")
  
  # Extract the runways for the current airport
  dt_rwys <- dt_smp[icao == apt, ]
  
  # Return the cartesian product of observations times runway headings
  dt_nc <- merge(x = dt_nc, y = dt_rwys, by = "icao", allow.cartesian = TRUE)

  ##############################################################################
  # Calculate the air density of moist air at the current airport              #
  # Replicate in R the FORTRAN polynomial approximation function ESW(T)        #
  #  for the saturation vapor pressure in mb over liquid water at 0°C          #
  #  found at https://icoads.noaa.gov/software/other/profs by Herman Wobus     #
  ##############################################################################
  
  # Inputs
  hurs <- dt_nc[, hurs] / 100    # Near-surface relative humidity in %
  ps   <- dt_nc[, ps]   / 100    # Near-surface air pressure in mbar
  tas  <- dt_nc[, tas]           # Near-surface air temperature in K
  es0  <- 6.1078                 # Reference saturation vapor pressure at 0°C in mbar
  Rd   <- 287.058                # Specific gas constant for dry air, in J/(kg·K)
  Rv   <- 461.495                # Specific gas constant for water vapor, in J/(kg·K)
  pol  <- 0.99999683 +           # Requires tas in °C
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
  pv  <- esw * hurs  # Partial pressure of water vapor, in mbar
  pd  <- ps - pv     # Partial pressure of dry air, in mbar
  rho <- ((pd / (Rd * tas)) + (pv / (Rv * tas))) * 100 # Total air density in kg/m3. Requires tas in K
  
  # Save the air density to a new column
  dt_nc[, rho := rho]
  
  ##############################################################################
  # Calculate the wind vector at the current airport                           #
  ##############################################################################
  
  # Calculate and write the airport's wind speed to a new column
  dt_nc[, wnd_spd := sqrt(uas^2 + vas^2)]
  
  # Calculate and write the airport's wind direction to a new column
  dt_nc[, wnd_dir := (180 + (180 / pi) * atan2(uas, vas)) %% 360]
  
  # Calculate and write each runway's headwind speed (in m/s) to a new column
  dt_nc[, hdw := wnd_spd * cos(abs(hdg - wnd_dir) * pi / 180)]
  
  # Keep only the runway with the maximum headwind speed (presumed to be the active runway) for each observation and experiment
  dt_nc <- dt_nc[, .SD[which.max(hdw)], by = .(obs, exp)]
  
  ##############################################################################
  # Write the data in tidy format to the database                              #
  ##############################################################################
  
  # Select which columns to write to the database and in which order
  cols <- c("obs", "icao", "exp", "hurs", "ps", "tas", "uas", "vas", "rho", "wnd_dir", "wnd_spd", "hdw", "rwy", "toda")
    
  # Write the data
  # Here we use the deprecated RMySQL::MySQL() driver instead of the newer RMariaDB::MariaDB()) driver because it was found to be ~2.8 times faster here
  dbWriteTable(conn = db_con, name = tolower(db_cli), value = dt_nc[, ..cols], append = TRUE, row.names = FALSE)
  
  # Disconnect the worker from the database
  dbDisconnect(db_con)

} # End of the fn_transform function

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################

# Set the number of workers to use in the cluster
cores <- 12

# Set the log file for the cluster defined in 0_common.R
outfile <- "logs/5_transform.log"

# Clear the log file
close(file(outfile, open = "w"))

# Build the cluster of workers
cl <- makeCluster(cores, outfile = outfile)

# Have each worker load the libraries that they need to handle the fn_import function defined above
clusterEvalQ(cl, {
  library(data.table)
  library(DBI)
  library(ncdf4)
  library(ncdf4.helpers)
  library(PCICt)
})

# Pass the required variables from the main scope to the workers' scope
clusterExport(cl, c("db_cnf", "db_grp", "db_imp", "db_cli", "dt_smp"))

# Build a list of unique airports to assign to the workers
apts <- unique(dt_smp[, icao], by = "icao")

# Distribute the unique airports across the workers
parLapply(cl, apts, fn_transform)

# Terminate the cluster once finished
stopCluster(cl)

################################################################################
# Add an index to the database table                                           #
################################################################################

# Set the index name
db_idx <- "idx"

# Build the query to create the index
db_qry <- paste("CREATE INDEX ", tolower(db_idx), " ON ", tolower(db_cli), " (icao);", sep = "")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

################################################################################
# Housekeeping                                                                 #
################################################################################

# Disconnect from the database
dbDisconnect(db_con)

# Display the script execution time
Sys.time() - start_time

# EOF