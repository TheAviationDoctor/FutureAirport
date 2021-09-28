################################################################################
# scripts/5_rho.R                                                              #
# Calculates the air density variable from hurs, ps, and tas                   #
#  Took ~2 hours to run on the researcher's config (https://bit.ly/3ChCBAP)    #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(parallel)

# Import the constants
source("scripts/0_constants.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

################################################################################
# Function to calculate the air density rho from hurs, ps, and tas             #
# Each worker is a CPU node that gets assigned one climate experiment (SSP)    #
################################################################################

# Declare the function with the experiment (SSP) as input parameter
fn_rho <- function(nc_exp) {

  ##############################################################################
  # Set up the database table to store this worker's outputs                   #
  ##############################################################################
  
  # Import the constants
  source("scripts/0_constants.R")
  
  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
    
  # Drop the table corresponding to the air density variable and current experiment (SSP), if it exists
  db_qry <- paste("DROP TABLE IF EXISTS ", tolower(db_rho), "_", tolower(nc_exp), ";", sep = "")
  db_res <- dbSendQuery(db_con, db_qry)
  dbClearResult(db_res)

  # Create the table corresponding to the air density variable and current experiment (SSP)
  db_qry <- paste("CREATE TABLE ", tolower(db_rho), "_", tolower(nc_exp), " (id INT NOT NULL AUTO_INCREMENT, obs DATETIME NOT NULL, icao CHAR(4) NOT NULL, val FLOAT NOT NULL, PRIMARY KEY (id));", sep = "")
  db_res <- dbSendQuery(db_con, db_qry)
  dbClearResult(db_res)

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), "Worker", Sys.getpid(), "is parsing the climatic variables for experiment", nc_exp, "from the database...", sep = " "))
  
  # Retrieve the hurs, ps, and tas values across their respective tables for the current experiment (SSP)
  db_qry <- paste("SELECT hurs_", nc_exp, ".id, hurs_", nc_exp, ".obs, hurs_", nc_exp, ".icao, hurs_", nc_exp, ".val AS hurs, ps_", nc_exp, ".val AS ps, tas_", nc_exp, ".val AS tas FROM hurs_", nc_exp, " LEFT JOIN ps_", nc_exp, " ON hurs_", nc_exp, ".obs = ps_", nc_exp, ".obs AND hurs_", nc_exp, ".icao = ps_", nc_exp, ".icao LEFT JOIN tas_", nc_exp, " ON hurs_", nc_exp, ".obs = tas_", nc_exp, ".obs AND hurs_", nc_exp, ".icao = tas_", nc_exp, ".icao;", sep = "")
  db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))                       # Suppress warnings about NAs being introduced by coercion (which there aren't)
  db_out <- setDT(dbFetch(db_res, n = Inf), key = "id", check.names = TRUE)     # Convert the query output from data frame to data table by reference (using setDT rather than as.data.table) to avoid duplication in memory
  dbClearResult(db_res)

  ##############################################################################
  # Calculate the air density of moist air                                     #
  # Replicates in R the FORTRAN polynomial approximation function ESW(T)       #
  # for the saturation vapor pressure in mb over liquid water at 0°C           #
  # found at https://icoads.noaa.gov/software/other/profs by Herman Wobus      #
  ##############################################################################
  
  # Inputs
  hurs <- db_out[, hurs] / 100    # Near-surface relative humidity in %
  ps   <- db_out[, ps]   / 100    # Near-surface air pressure in hPa
  tas  <- db_out[, tas]  - 273.15 # Near-surface air temperature in °C
  es0  <- 6.1078                  # Saturation vapor pressure at 0°C (constant)
  Rd   <- 287.058                 # Specific gas constant for dry air, in J/(kg·K)
  Rv   <- 461.495                 # Specific gas constant for water vapor, in J/(kg·K)
  pol  <- 0.99999683 +            # Requires tas in °C
    tas * (-0.90826951E-02 +
             tas * (0.78736169E-04 +
                      tas * (-0.61117958E-06 +
                               tas * (0.43884187E-08 +
                                        tas * (-0.29883885E-10 +
                                                 tas * (0.21874425E-12 +
                                                          tas * (-0.17892321E-14 +
                                                                   tas * (0.11112018E-16 +
                                                                            tas * (-0.30994571E-19)))))))))

  # Outputs
  esw <- es0 / pol^8              # Saturation vapor pressure at tas, in Pa
  pv  <- esw * hurs               # Partial pressure of water vapor, in Pa
  pd  <- ps - pv                  # Partial pressure of dry air, in Pa
  rho <- ((pd / (Rd * (tas + 273.15))) + (pv / (Rv * (tas + 273.15)))) * 100    # Total air density. Requires tas in K

  # Save the air density to a new column
  db_out[, val := rho]

  # Choose which data table columns to write to the database table
  db_cols <- c("obs", "icao", "val")

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), " Worker ", Sys.getpid(), " is writing ", tolower(db_rho), "_", tolower(nc_exp), " to the database...", sep = ""))

  # Write the data to the table corresponding air density and the current experiment (SSP)
  # Here we use the deprecated RMySQL::MySQL() driver instead of the newer RMariaDB::MariaDB()) driver because it was found to be ~2.8 times faster here
  dbWriteTable(conn = db_con, name = paste(tolower(db_rho), "_", tolower(nc_exp), sep = ""), value = db_out[, ..db_cols], append = TRUE, row.names = FALSE)

  ##############################################################################
  # Add indices to the experiment table                                        #
  ##############################################################################

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), " Worker ", Sys.getpid(), " is indexing table ", tolower(db_rho), "_", tolower(nc_exp), "...", sep = ""))

  # Create a composite index on the icao and obs columns (after the bulk insert above, not before for performance reasons) to speed up subsequent searches
  db_idx <- "idx" # Set index name
  db_qry <- paste("CREATE INDEX ", tolower(db_idx), " ON ", tolower(db_rho), "_", tolower(nc_exp), " (icao, obs);", sep = "")
  db_res <- dbSendQuery(db_con, db_qry)
  dbClearResult(db_res)
  
  # Disconnect the worker from the database
  dbDisconnect(db_con)

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), "Worker", Sys.getpid(), "in charge of experiment", nc_exp, "has completed its work.", sep = " "))
  
} # End of the fn_rho function definition

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################

# Set the number of cores/workers to use in the cluster. Here we set as many workers as there are experiments (SSPs) in the NetCDF files
cores <- length(nc_exps)

# Set and clear the output file for cluster logging
close(file(log_rho, open = "w"))

# Build the cluster of workers and select a file in which to log progress (which can't be printed to the console on the Windows version of RStudio)
cl <- makeCluster(cores, outfile = log_rho)

# Have each worker load the libraries that they need to handle the fn_rho function defined above
clusterEvalQ(cl, {
  library(data.table)
  library(DBI)
})

# Distribute the parallel parsing of NetCDF files across the workers, with the climate variables as input parameter
parLapply(cl, nc_exps, fn_rho)

# Terminate the cluster once finished
stopCluster(cl)

################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF