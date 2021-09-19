
################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(DBI)
library(ggplot2)
library(R.utils)

# Clear the console
cat("\014")

################################################################################
# List all the experiments (SSPs) from the NetCDF file names                   #
################################################################################

nc_path  <- "data/climate/2_netcdf_downloads"                                   # Set the file path where the NetCDF files are located
nc_files <- list.files(path = nc_path, pattern = "\\.nc$", full.names = TRUE)   # List all the NetCDF files (ending in .nc)
nc_exps  <- unique(lapply(strsplit(basename(nc_files), "_"), "[", 4))           # List all the experiments (SSPs) from the NetCDF file names

################################################################################
# Set up the database connection                                               #
################################################################################

# Set the database parameters
db_cnf <- ".my.cnf"                                                             # Import the database connection parameters from the cnf file
db_grp <- "phd"                                                                 # Group name within the cnf file

# Open the connection to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

################################################################################
# Load the airport sample                                                      #
################################################################################

# Set the name of the table where population data are stored
# db_tbl <- "population"

# Retrieve the sample airports, sorted from largest to smallest traffic
# threshold <- 10^6                                                               # Minimum traffic size for the airport to be sampled
# db_qry <- paste("SELECT DISTINCT icao FROM ", db_tbl, " WHERE traffic > ", threshold, " ORDER BY traffic DESC;", sep = "")
# db_res <- dbSendQuery(db_con, db_qry)
# df_smp <- suppressWarnings(dbFetch(db_res, n = Inf))
# dbClearResult(db_res)

################################################################################
# Plot the air density over time across all sample airports                    #
################################################################################

# Set the name of the table where air density data are stored
db_tbl <- "rho"

# Define the latitude brackets for which we want to plot the air density
lat.lower <- c(0, 23.4365, 66.5635, 0)
lat.upper <- c(23.4365, 66.5635, 90, 90)
lat.names <- c("Tropics", "Temperate", "Frigid", "All")

# Query for all airports
# db_qry <- paste(paste(paste("SELECT obs, AVG(val) AS avg_rho, '", nc_exps,"' AS exp FROM ", db_tbl, "_", nc_exps, " GROUP BY obs", sep = ""), collapse = " UNION "), ";", sep = "") # Build the query

for(i in 1:(length(lower))) {

  # Query to average the air density for each experiment (SSP) separately, across all sample airports within each of the latitude brackets defined earlier
  db_qry <- paste(paste(paste("SELECT obs, AVG(val) AS avg_rho, '", nc_exps,"' AS exp FROM ", db_tbl, "_", nc_exps, " INNER JOIN population ON (", db_tbl, "_", nc_exps, ".apt = population.icao) WHERE ABS(population.lat) BETWEEN ", lower[i]," AND ", upper[i]," GROUP BY ", db_tbl, "_", nc_exps, ".obs", sep = ""), collapse = " UNION "), ";", sep = "") # Build the query
  db_res <- dbSendQuery(db_con, db_qry)                                           # Send the query to the database
  dt_rho <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), check.names = TRUE)) # Convert the query output from data frame to data table by reference (using setDT rather than as.data.table) to avoid duplication in memory
  dt_rho[, obs := as.POSIXct(obs, format = "%Y-%m-%d %H:%M:%S")]                  # Convert the observations' time stamps back to POSIXct format for plotting
  dt_rho[, exp := as.factor(exp)]                                                 # Convert the experiment from character to factor
  print(nrow(dt_rho))
  dbClearResult(db_res)

  # Plot the air density over time
  plot <- ggplot(data = dt_rho, mapping = aes(x = obs, y = avg_rho)) +
    geom_line() +
    geom_smooth(method = "lm", formula = y ~ x) +
    labs(x = "Time", y = "Air density", title = "Average air density") +
    facet_wrap(~ exp, ncol = 2) +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

  # Save the plot to a file
  ggsave(
    paste("rho_", tolower(lat.names[i]), ".png"),
    plot = plot,
    device = "png",
    path = "plots/",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print",
    limitsize = TRUE,
    bg = NULL
  )

}

# Release the database connection
dbDisconnect(db_con)

# EOF