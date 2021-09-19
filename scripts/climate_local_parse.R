################################################################################
# /scripts/climate_parse.R                                                     #
# Parses the per-airport csv files to describe climate statistics              #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(DBI)
library(ggplot2)
library(parallel)
library(R.utils)

# Clear the console
cat("\014")

################################################################################
# List all the experiments (SSPs) from the NetCDF file names                   #
################################################################################

nc_path  <- "data/climate/2_netcdf_downloads"                                   # Set the file path where the NetCDF files are located
nc_files <- list.files(path = nc_path, pattern = "\\.nc$", full.names = TRUE)   # List all the NetCDF files (ending in .nc)
nc_exps  <- unique(lapply(strsplit(basename(nc_files), "_"), "[", 4))           # List all the experiments (SSPs) from the NetCDF file names

###############################################################################
# Load and describe the airport sample                                        #
###############################################################################

# Set the database parameters
db_cnf <- ".my.cnf"                                                             # Import the database connection parameters from the cnf file
db_grp <- "phd"                                                                 # Group name within the cnf file
db_tbl <- "population"                                                          # Name the table that will store the population data
threshold <- 10^6                                                               # Set the airport size threshold for sampling

# Open the connection to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

# Retrieve the sample airports, sorted from largest to smallest traffic
db_qry <- paste("SELECT DISTINCT icao FROM ", db_tbl, " WHERE traffic > ", threshold, " ORDER BY traffic DESC;", sep = "")
db_res <- dbSendQuery(db_con, db_qry)
df_smp <- suppressWarnings(dbFetch(db_res, n = Inf))
dbClearResult(db_res)

# Release the database connection
dbDisconnect(db_con)

################################################################################
# Plot the air density over time at each sample airport                        #
################################################################################

nc_parse <- function(exp) {

  # Set the prefix of the air density tables
  db_tbl <- "rho"
  
  # Open the connection to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Retrieve the average air density across all sample airports
  db_qry <- paste("SELECT obs, AVG(val) AS avg_rho FROM ", db_tbl, "_", exp, " GROUP BY obs;", sep = "") # Calculate the average air density across all airports for each observation (timestamp)
  db_res <- dbSendQuery(db_con, db_qry)                                                                  # Send the query to the database
  dt_rho <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "obs", check.names = TRUE))           # Convert the query output from data frame to data table by reference (using setDT rather than as.data.table) to avoid duplication in memory
  # db_out[['obs']] <- as.POSIXct(db_out[['obs']], format = "%Y-%m-%d %H:%M:%S")                         # Convert the observations' timestamps back to POSIXct format for plotting
  dt_rho[, obs := as.POSIXct(obs, format = "%Y-%m-%d %H:%M:%S")]                                         # Convert the observations' timestamps back to POSIXct format for plotting
  dbClearResult(db_res)

  # Release the database connection
  dbDisconnect(db_con)
  
  print(str(dt_rho))
  print(head(dt_rho))
  print(tail(dt_rho))
  
  plot <- ggplot(data = dt_rho, mapping = aes(x = obs, y = avg_rho)) +
    geom_line() +
    geom_smooth(method = "lm", formula = y ~ x) +
    labs(x = "Time", y = "Air density", title = "Average air density") +
    # facet_grid(exp) +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())
  
  ggsave(
      paste("rho_", exp, ".png"),
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

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################

# Set the number of cores/workers to use in the cluster. Here we set as many workers as there are experiments (SSPs) in the NetCDF files
cores <- length(nc_exps)

# Set and clear the output file for cluster logging
outfile <- ".climate.log"
close(file(outfile, open = "w"))

# Build the cluster of workers and select a file in which to log progress (which can't be printed to the console on the Windows version of RStudio)
cl <- makeCluster(cores, outfile = outfile)

# Have each worker load the libraries that they need to handle the nc_parse function defined above
clusterEvalQ(cl, {
  library(data.table)
  library(DBI)
  library(ggplot2)
})

# Pass the required variables from the main scope to the workers' scope
clusterExport(cl, c("db_cnf", "db_grp", "df_smp"))

# Distribute the parallel parsing of NetCDF files across the workers, with the climate variables as input parameter
parLapply(cl, nc_exps, nc_parse)

# Terminate the cluster once finished
stopCluster(cl)

# EOF