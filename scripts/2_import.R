# ==============================================================================
#    NAME: scripts/2_import.R
#   INPUT: NetCDF files downloaded from the Earth System Grid Federation (ESGF)
# ACTIONS: Extract time series of climate variables for each airport coordinates
#  OUTPUT: 312,008 rows of climate data written to a parquet file
# RUNTIME: ~20 minutes (MacBook Pro M3 Max)
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2024
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Clear the environment
rm(list = ls())

# Load the required libraries
library(data.table)
library(lubridate)
library(ncdf4)
library(ncdf4.helpers)
library(tidyverse)

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Load the airport data
# ==============================================================================

# Load the airport data and index by ICAO code
dt_apt <- fread(
  file       = "data/apt/airports.csv",
  header     = TRUE,
  colClasses =
    c("character", "character", "character", "numeric", "numeric", "character")
) |> setkey(cols = icao, verbose = TRUE)

# List the NetCDF files from which to extract the airports' climatic conditions
nc_files <- list.files(
  path       = "data/cdf",
  pattern    = "tas_6hrPlevPt_MPI-ESM1-2-HR", # Air temperature only
  full.names = TRUE
)
# ) |> tail(2L) # FOR TESTING ONLY

# ==============================================================================
# 2 Define a function to parse the NetCDF files
# ==============================================================================

fn_import <- function(nc_file) {

  # ============================================================================
  # 2.1 Parse each NetCDF file
  # ============================================================================

  # Inform the log file
  print(
    paste(
      Sys.time(),
      " Processing ", basename(nc_file),
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
  # 2.2 Extract the climatic variables for each sample airport (inner loop)
  # ============================================================================

  dt_nc <- lapply(

    # For each airport (passed as a vector so lapply treats them one by one)
    X = as.vector(dt_apt$icao),
    # X = head(as.vector(dt_apt$icao), 10L), # FOR TESTING ONLY

    # Fetch the climate data corresponding to the airport's spatial grid cell
    FUN = function(x) {

      # Assemble the results into a data table
      return(
        data.table(
          icao = as.factor(x), # Airport's ICAO code
          var  = as.factor(nc_var), # Climate variable
          ssp  = as.factor(nc_ssp), # Experiment (SSP)
          year = as.factor(    # Year of the observation
            year(
              PCICt::as.POSIXct.PCICt(
              x      = nc_obs,
              tz     = "GMT",
              format = "%Y-%m-%d %H:%M:%S"
              )
            )
          ),
          val  = as.vector(         # Climate value
            nc_arr[
              which.min(abs(nc_lon - dt_apt[icao == x, lon])),
              which.min(abs(nc_lat - dt_apt[icao == x, lat])),
            ]
          )
        )
      )

    } # End function

  ) # End lapply

  # ============================================================================
  # 2.3 Consolidate the outputs and save them
  # ============================================================================

  # Consolidate the data tables
  rbindlist(l = dt_nc, use.names = FALSE)

} # End of the fn_import function

# Run the function across all NetCDF files and all airports, then consolidate and index
dt_cli <- lapply(
  X   = nc_files,
  FUN = fn_import
) |>
rbindlist(use.names = FALSE) |>
setkey(icao, var, ssp, year)

# Group the data
dt_cli <- dt_cli[,
  .(min = min(val), lq = quantile(val, probs = .25), mean = mean(val), median = median(val), uq = quantile(val, probs = .75), max = max(val)),
  by = .(icao, var, ssp, year)]

# Convert tas from K to C
cols <- c("min", "lq", "mean", "median", "uq", "max")
dt_cli[var == "tas", (cols) := lapply(.SD, "-", 273.15), .SDcols = cols]

# Calculate difference in values from the first year in every group (icao, var, and ssp)
cols_dif <- paste(cols_dif, "dif", sep = "_")
dt_cli[,
       (cols_dif) := lapply(
         X     = .SD,
         FUN   = function(x) { (x - x[1:1]) }
       ),
       by      = .(icao, var, ssp),
       .SDcols = cols
]

# Reduce decimal precision to save space
dt_cli[, (cols) := lapply(.SD, round, digits = 2), .SDcols = cols]

# Remove data beyond the year 2100
dt_cli <- dt_cli[year != "2101"]

# Save to CSV file
fwrite(
  x        = dt_cli,
  file     = "data/cli/cli.csv",
  compress = "auto"
)

# ==============================================================================
# 3 Housekeeping
# ==============================================================================

# Stop the script timer
Sys.time() - start_time

# EOF