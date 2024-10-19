# ==============================================================================
#    NAME: scripts/4_import.R
#   INPUT: NetCDF files downloaded from the Earth System Grid Federation (ESGF)
# ACTIONS: Extract time series of climate variables for each airport coordinates
#  OUTPUT: 2,213,829,660 rows of climate data written to the database
# RUNTIME: ~7.2 hours (3.8 GHz CPU / 128 GB DDR4 RAM / SSD)
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2023
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Clear the environment
rm(list = ls())

# Load the required libraries
library(data.table)
library(DBI)
library(ncdf4)
library(ncdf4.helpers)
library(tidyverse)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# Set a time horizon for the climatic data
horizon <- as.POSIXct(
  x      = "2101-01-01 00:00:00",
  tz     = "GMT",
  format = "%Y-%m-%d %H:%M:%S"
)

# ==============================================================================
# 1 Set up the database table
# ==============================================================================

# Drop the table if it exists
fn_sql_qry(
  statement = paste("DROP TABLE IF EXISTS ", tolower(dat$tas), ";", sep = "")
)

# Create the table
fn_sql_qry(
  statement = paste(
    "CREATE TABLE", tolower(dat$tas),
    "(
    id   INT UNSIGNED NOT NULL AUTO_INCREMENT,
    icao CHAR(4) NOT NULL,
    obs  DATETIME NOT NULL,
    ssp  CHAR(6) NOT NULL,
    tas  FLOAT NOT NULL,
    PRIMARY KEY (id)
    );",
    sep = " "
  )
)

# ==============================================================================
# 2 Fetch the data that we need
# ==============================================================================

# Fetch the list of unique airports above the traffic threshold
dt_smp <- fn_sql_qry(
  statement = paste(
    "SELECT icao, MAX(lat) AS lat, MAX(lon) AS lon",
    "FROM", dat$apt,
    "WHERE traffic >", sim$pop_thr,
    "GROUP BY icao;",
    sep = " "
  )
)

# Index the data table to speed up subsequent lookups
setkey(x = dt_smp, cols = icao, verbose = TRUE)

# List the NetCDF files from which to extract the airports' climatic conditions
nc_files <- list.files(path = dir$cli, pattern = "tas_6hrPlevPt_MPI-ESM1-2-HR", full.names = TRUE)

# ==============================================================================
# 3 Define a function to parse the NetCDF files
# ==============================================================================

fn_import <- function(nc_file) {

  # ============================================================================
  # 3.1 Parse the current NetCDF file
  # ============================================================================

  # Inform the log file
  print(
    paste(
      Sys.time(),
      " pid ",
      stringr::str_pad(
        Sys.getpid(),
        width = 5L,
        side  = "left",
        pad   = " "
      ),
      " is processing ", basename(nc_file),
      "...",
      sep = ""
    )
  )

  # Open the NetCDF file
  nc <- ncdf4::nc_open(
    filename  = nc_file,
    write     = FALSE,
    readunlim = FALSE
  )

  # Read the NetCDF file's attributes
  nc_att <- ncdf4::ncatt_get(nc = nc, varid = 0L)

  # Read the name of the file's climatic variable
  nc_var <- nc_att$variable_id

  # Read the file's experiment variable (SSP)
  nc_ssp <- nc_att$experiment_id

  # Read the latitude vector
  nc_lat <- ncdf4::ncvar_get(nc = nc, varid = "lat")

  # Read the longitude vector
  nc_lon <- ncdf4::ncvar_get(nc = nc, varid = "lon")

  # Recode the longitude vector from 0°-360° to -180°-180°
  nc_lon <- ((nc_lon + 180L) %% 360L) - 180L

  # Read the time vector in PCICt (POSIXct-like) format
  nc_obs <- ncdf4.helpers::nc.get.time.series(
    f = nc,
    v = nc_var,
    time.dim.name = "time"
  )

  # Read the 3D climate array
  nc_arr <- ncdf4::ncvar_get(nc = nc, varid = nc_var)

  # Release the NetCDF file from memory
  ncdf4::nc_close(nc = nc)

  # ============================================================================
  # 3.2 Extract the climatic variables for each sample airport (inner loop)
  # ============================================================================

  dt_nc <- lapply(

    # For each airport (passed as a vector so lapply treats them one by one)
    X = as.vector(dt_smp$icao),

    # Fetch the climate data corresponding to the airport's spatial grid cell
    FUN = function(x) {

      # Find the row index of the latitude nearest to the airport's
      lat_idx <- which.min(abs(nc_lat - dt_smp[icao == x, lat]))

      # Find the row index of the longitude nearest to the airport's
      lon_idx <- which.min(abs(nc_lon - dt_smp[icao == x, lon]))

      # Extract the climate variable's time series at those spatial indices
      nc_val <- nc_arr[lon_idx, lat_idx, ]

      # Assemble the results into a data table
      dt_apt <- data.table(
        icao = as.factor(dt_smp[icao == x, icao]), # Airport's ICAO code
        obs      = PCICt::as.POSIXct.PCICt(
          x      = nc_obs,
          tz     = "GMT",
          format = "%Y-%m-%d %H:%M:%S"
        ),
        ssp  = as.factor(nc_ssp), # Experiment (SSP)
        tas  = as.vector(nc_val)  # Air temperature
      )

      # Remove cases beyond the time horizon
      return(subset(x = dt_apt, subset = obs < horizon))

    } # End lapply function

  ) # End lapply

  # ============================================================================
  # 3.3 Consolidate the outputs and write them to the database
  # ============================================================================

  # Consolidate the data tables
  dt_nc <- rbindlist(l = dt_nc, use.names = FALSE)

  # Connect to the database
  conn <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

  # Write to the database
  dbWriteTable(
    conn      = conn,
    name      = tolower(dat$tas),
    value     = dt_nc,
    append    = TRUE,
    row.names = FALSE
  )

  # Disconnect from the database
  dbDisconnect(conn)

} # End of the fn_import function

lapply(
  X   = nc_files,
  FUN = fn_import
)

# ==============================================================================
# 5 Index the database table
# ==============================================================================

fn_sql_qry(
  statement = paste(
    "CREATE INDEX idx ON",
    tolower(dat$tas),
    "(icao, ssp);",
    sep = " "
  )
)

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Stop the script timer
Sys.time() - start_time

# EOF
