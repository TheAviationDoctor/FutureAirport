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
library(parallel)
library(tidyverse)
library(tmaptools)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# Set the number of CPU cores for parallel processing
crs <- 10L

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
  statement = paste("DROP TABLE IF EXISTS ", tolower(dat$imp), ";", sep = "")
)

# Create the table
fn_sql_qry(
  statement = paste(
    "CREATE TABLE", tolower(dat$imp),
    "(
    id   INT UNSIGNED NOT NULL AUTO_INCREMENT,
    obs  DATETIME NOT NULL,
    icao CHAR(4) NOT NULL,
    lat  FLOAT NOT NULL,
    lon  FLOAT NOT NULL,
    zone CHAR(11) NOT NULL,
    ssp  CHAR(6) NOT NULL,
    var  CHAR(4) NOT NULL,
    val  FLOAT NOT NULL,
    PRIMARY KEY (id)
    );",
    sep = " "
  )
)

# ==============================================================================
# 2 Fetch the data that we need
# ==============================================================================

# Fetch the list of unique airports in the sample
dt_smp <- fn_sql_qry(
  statement = paste(
    "SELECT icao, lat, lon, zone",
    "FROM", dat$pop,
    "WHERE traffic >", sim$pop_thr,
    "GROUP BY icao;",
    sep = " "
  )
)

# Recast column types
set(x = dt_smp, j = "zone", value = as.factor(dt_smp[, zone]))

# Index the data table to speed up subsequent lookups
setkey(x = dt_smp, cols = icao, verbose = TRUE)

# List the NetCDF files from which to extract the airports' climatic conditions
nc_files <- list.files(path = dir$cli, pattern = "\\.nc$", full.names = TRUE)

# ==============================================================================
# 3 Parse the NetCDF files
# ==============================================================================

fn_import <- function(nc_file) {

  # ============================================================================
  # 3.1 Parse the current NetCDF file
  # ============================================================================

  # Offset the start of each worker by a random duration to spread disk I/O load
  Sys.sleep(time = sample(1:(crs * 10L), 1L))

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
  # 3.2 Plot the climate model's spatial grid cell
  # ============================================================================

  # Check if the plot already exists
  if (
    file.exists(
      paste(dir$plt, "4_map_of_climate_model_spatial_grid.png", sep = "/")
    ) == FALSE
  ) {

    # Find the grid cells occupied by sample airports
    grid  <- expand.grid(lat = nc_lat, lon = nc_lon)
    match <- lapply(
      X   = as.vector(dt_smp$icao),
      FUN = function(x) {
        which.min(
          abs(grid$lat - dt_smp[icao == x, lat]) +
            abs(grid$lon - dt_smp[icao == x, lon])
        )
      }
    )
    match <- unique(unlist(match))

    # Find the mean distance between two grid cell latitudes
    off_lat <- mean(abs(diff(as.vector(nc_lat))))

    # Find the mean distance between two grid cell latitudes
    off_lon <- mean(abs(diff(abs(as.vector(nc_lon)))))

    # Define the world object from the Natural Earth package
    world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

    # Plot the grid cells of the NetCDF file onto a world map
    ggplot() +
      geom_sf(data = world, fill = "white") +
      coord_sf(datum = NA, expand = FALSE) +
      # Add the airports
      geom_rect(
        color   = NA,
        fill    = "blue",
        linewidth = 0L,
        data    = data.frame(
          xmin  = grid[match, "lon"] - off_lon / 2L,
          xmax  = grid[match, "lon"] + off_lon / 2L,
          ymin  = grid[match, "lat"] - off_lat / 2L,
          ymax  = grid[match, "lat"] + off_lat / 2L
        ),
        mapping = aes(
          xmin  = xmin,
          xmax  = xmax,
          ymin  = ymin,
          ymax  = ymax
        )
      ) +
      # Add the parallels
      geom_hline(
        color      = "blue",
        linewidth  = .05,
        yintercept = nc_lat - off_lat / 2L
      ) +
      # Add the meridians
      geom_vline(
        color      = "blue",
        linewidth  = .05,
        xintercept = nc_lon - off_lon / 2L
      ) +
      theme_light() +
      theme(
        axis.title  = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(-.8, 0L, -.8, 0L, "in")
      )

    # Find the aspect ratio of the map
    ar <- tmaptools::get_asp_ratio(world)

    # Save the plot
    ggsave(
      filename = "4_map_of_climate_model_spatial_grid.png",
      plot     = last_plot(),
      device   = "png",
      path     = "plots",
      scale    = 1L,
      height   = 9L / ar,
      width    = 9L,
      units    = "in",
      dpi      = "retina"
    )

  } # End plot creation

  # ============================================================================
  # 3.2 Extract the climatic variables for each sample airport (inner loop)
  # ============================================================================

  dt_nc <- lapply(

    # For each airport (passed as a vector so lapply treats them one by one)
    X   = as.vector(dt_smp$icao),

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
        obs      = PCICt::as.POSIXct.PCICt(
          x      = nc_obs,
          tz     = "GMT",
          format = "%Y-%m-%d %H:%M:%S"
        ),
        icao = as.factor(dt_smp[icao == x, icao]), # Airport's ICAO code
        lat  = dt_smp[icao == x, lat],             # Airport's latitude
        lon  = dt_smp[icao == x, lon],             # Airport's longitude
        zone = as.factor(dt_smp[icao == x, zone]), # Airport's climate zone
        ssp  = as.factor(nc_ssp),                  # Experiment (SSP)
        var  = as.factor(nc_var),                  # Climatic variable name
        val  = as.vector(nc_val)                   # Climatic variable value
      )

      # All climate variables except 'hurs' are 6-hourly mean samples at 06:00
      # (i.e. a mean of 03:00-09:00), 12:00 (i.e. a mean of 09:00-15:00), 18:00
      # (i.e. a mean of 15:00-21:00), and 00:00 (i.e. a mean of 21:00-03:00).
      # 'hurs' is instead sampled 6-hourly at a specified time point within the
      # time period (03:00, 09:00, 15:00, 21:00). For the observation times to
      # line up with those of other variables, hurs must be normalized. To do so
      # the rolling mean of time & value is computed for every row pair of hurs
      if (nc_var == "hurs") {
        # Advance the observation time by 3 hours (which is the same as
        # averaging the times of the current and next six-hourly observations)
        set(x = dt_apt, j = "obs", value = dt_apt[, obs] + 3600L * 3L)
        # Average the current and next observation values
        set(
          x     = dt_apt,
          j     = "val",
          value = frollmean(x = dt_apt[, val], n = 2L, align = "left")
        )
        # The last value of 'hurs' for every NetCDF file and airport (365.25
        # days x 5 years x 4 daily observations = every 7,305th observation)
        # would be empty as a result of the left-centered rolling mean, so we
        # impute it by carrying the last observation forward ("locf")
        setnafill(x = dt_apt, type = "locf", cols = "val")
      }

      # Remove cases beyond the time horizon
      return(subset(x = dt_apt, subset = obs < horizon))

    } # End lapply function

  ) # End lapply

  # ============================================================================
  # 3.3 Consolidate the outputs and write them to the database
  # ============================================================================

  # Consolidate the data tables
  dt_nc <- rbindlist(l = dt_nc, use.names = FALSE)

  # Connect the worker to the database
  conn <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

  # Write to the database
  dbWriteTable(
    conn      = conn,
    name      = tolower(dat$imp),
    value     = dt_nc,
    append    = TRUE,
    row.names = FALSE
  )

  # Disconnect the worker from the database
  dbDisconnect(conn)

} # End of the fn_import function

# ==============================================================================
# 4 Handle the parallel computation
# ==============================================================================

# Distribute the NetCDF files across the CPU cores
fn_par_lapply(
  crs = crs,
  pkg = c(
    "data.table",
    "DBI",
    "ggplot2",
    "ncdf4",
    "ncdf4.helpers",
    "PCICt",
    "rnaturalearth",
    "stringr",
    "tmaptools"
  ),
  lst = nc_files,
  fun = fn_import
)

# ==============================================================================
# 5 Index the database table
# ==============================================================================

fn_sql_qry(
  statement = paste(
    "CREATE INDEX idx ON",
    tolower(dat$imp),
    "(icao);",
    sep = " "
  )
)

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Stop the script timer
Sys.time() - start_time

# EOF
