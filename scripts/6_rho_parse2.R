################################################################################
# /scripts/6_rho_parse.R                                                       #
# Plots the air density over time                                              #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(DBI)
library(ggplot2)
library(readr)
library(R.utils)

# Clear the console
cat("\014")

################################################################################
# List all the experiments (SSPs) from the NetCDF file names                   #
################################################################################

nc_path  <- "data/climate/netcdf"                                               # Set the file path where the NetCDF files are located
nc_files <- list.files(path = nc_path, pattern = "\\.nc$", full.names = TRUE)   # List all the NetCDF files (ending in .nc)
nc_exps  <- unique(lapply(strsplit(basename(nc_files), "_"), "[", 4))           # List all the experiments (SSPs) from the NetCDF file names

################################################################################
# Extract the sample airports for each climate zone                            #
################################################################################

# Define the latitude brackets for the climate zones
lat_lower <- c(0, 23.4365, 66.5635, 0)
lat_upper <- c(23.4365, 66.5635, 90, 90)
lat_names <- c("Tropical", "Temperate", "Frigid", "All")

# Open the connection to the database
db_cnf <- ".my.cnf"                                                             # Set the file name that contains the database connection parameters
db_grp <- "phd"                                                                 # Set the group name within the cnf file that contains the connection parameters
db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

# For each latitude bracket
for (i in 1:length(lat_names)) {

  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Retrieve the sample airports in those latitudes
  db_tbl <- "population"                                                        # Set the name of the table where the population data are stored
  threshold <- 10^6                                                             # Set the sample airports' minimum traffic size
  db_qry <- paste("SELECT DISTINCT icao FROM", db_tbl, "WHERE traffic >", threshold, "AND ABS(lat) BETWEEN", lat_lower[i], "AND", lat_upper[i], "ORDER BY icao ASC", sep = " ")
  db_res <- dbSendQuery(db_con, db_qry)                                         # Send the query to the database
  dt_apt <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "icao", check.names = TRUE)) # Convert the query output from data frame to data table by reference (using setDT rather than as.data.table) to avoid duplication in memory
  # df_apt <- suppressWarnings(dbFetch(db_res, n = Inf))                                            
  dbClearResult(db_res)

  # print(paste(paste(paste("SELECT obs, AVG(val) AS avg_rho, '", nc_exps,"' AS exp FROM ", db_tbl, "_", nc_exps, " WHERE apt IN (", dt_apt[, 1], ")", collapse = " UNION ", sep = ""), sep = ""), ";", sep = ""))
  
  # Calculate the average air density for each experiment (SSP)
  db_tbl <- "rho"                                                          # Set the name of the table where the air density data are stored
  db_qry <- paste(paste(paste("SELECT obs, AVG(val) AS avg_rho, '", nc_exps,"' AS exp FROM ", db_tbl, "_", nc_exps, " WHERE apt IN ('AYPY', 'KATL')", collapse = " UNION ", sep = ""), sep = ""), ";", sep = "")
  db_res <- dbSendQuery(db_con, db_qry)                                           # Send the query to the database
  dt_rho <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), check.names = TRUE)) # Convert the query output from data frame to data table by reference (using setDT rather than as.data.table) to avoid duplication in memory
  dt_rho[, obs := as.POSIXct(obs, format = "%Y-%m-%d %H:%M:%S")]                  # Convert the observations' time stamps back to POSIXct format for plotting
  dt_rho[, exp := as.factor(exp)]                                                 # Convert the experiment from character to factor
  dbClearResult(db_res)

  View(dt_rho)
  
  dbDisconnect(db_con)
  
}

# Release the database connection
# dbDisconnect(db_con)

stop()

################################################################################
# Function to plot the air density over time across all sample airports        #
# Each worker is a CPU node that gets assigned one climate zone to parse       #
################################################################################

# Declare the function with the experiment (SSP) as input parameter
nc_parse <- function(lat) {
  
  # Open the worker's connection to the database
  db_cnf <- ".my.cnf"                                                           # Set the file name that contains the database connection parameters
  db_grp <- "phd"                                                               # Set the group name within the cnf file that contains the connection parameters
  db_tbl <- "rho"                                                               # Set the name of the table where the air density data are stored
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), " Worker ", Sys.getpid(), " is querying the database for the average air density values across the", lat$name, "zones...", sep = " "))
  
  # Query to average the air density for each experiment (SSP) separately, across all sample airports within each of the latitude brackets defined earlier
  db_qry <- paste(paste(paste("SELECT obs, AVG(val) AS avg_rho, '", nc_exps,"' AS exp FROM ", db_tbl, "_", nc_exps, " INNER JOIN population ON (", db_tbl, "_", nc_exps, ".apt = population.icao) WHERE ABS(population.lat) BETWEEN ", lat$lower," AND ", lat$upper," GROUP BY ", db_tbl, "_", nc_exps, ".obs", sep = ""), collapse = " UNION "), ";", sep = "") # Build the query
  db_res <- dbSendQuery(db_con, db_qry)                                           # Send the query to the database
  dt_rho <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), check.names = TRUE)) # Convert the query output from data frame to data table by reference (using setDT rather than as.data.table) to avoid duplication in memory
  dt_rho[, obs := as.POSIXct(obs, format = "%Y-%m-%d %H:%M:%S")]                  # Convert the observations' time stamps back to POSIXct format for plotting
  dt_rho[, exp := as.factor(exp)]                                                 # Convert the experiment from character to factor
  dbClearResult(db_res)
  
  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), " Worker ", Sys.getpid(), " finished querying the database for the", lat$name, "zones and is now plotting the air density...", sep = " "))
  
  # Plot the air density over time
  plot <- ggplot(data = dt_rho, mapping = aes(x = obs, y = avg_rho)) +
    geom_line() +
    geom_smooth(method = "lm", formula = y ~ x) +
    labs(x = "Time", y = "Air density", title = paste("Average Air Density Across", lat$name, "zones", sep = " ")) +
    facet_wrap(~ exp, ncol = 2) +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

  # Save the plot to a file
  ggsave(
    paste("rho_", tolower(lat$name), ".png"),
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

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), " Worker ", Sys.getpid(), " finished plotting the air density for the", lat$name, "zones.", sep = " "))
  
  # Release the database connection
  dbDisconnect(db_con)
  
} # End of the nc_parse function definition

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################

# Set the number of cores/workers to use in the cluster. Here we set as many workers as there are world climate zones for which we want to plot average rho
cores <- 4

# Set and clear the output file for cluster logging
outfile <- "logs/rho_parse.log"
close(file(outfile, open = "w"))

# Build the cluster of workers and select a file in which to log progress (which can't be printed to the console on the Windows version of RStudio)
cl <- makeCluster(cores, outfile = outfile)

# Have each worker load the libraries that they need to handle the nc_parse function defined above
clusterEvalQ(cl, {
  library(data.table)
  library(DBI)
})

# Pass the required variables from the main scope to the workers' scope
clusterExport(cl, c("nc_exps"))

# Distribute the parallel parsing of NetCDF files across the workers, with the climate variables as input parameter
parLapply(cl, lats, nc_parse)

# Terminate the cluster once finished
stopCluster(cl)

# EOF