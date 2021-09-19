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
# List the climate zones to parse                                              #
################################################################################

# Define the latitude brackets for which we want to plot the air density
lat_names <- c("Tropics", "Temperate", "Frigid", "All")
lat_lower <- c(0, 23.4365, 66.5635, 0)
lat_upper <- c(23.4365, 66.5635, 90, 90)

lats <- list(
  list(name = "Tropical",  lower = 0,       upper = 23.4465),
  list(name = "Temperate", lower = 23.4365, upper = 66.5635),
  list(name = "Frigid",    lower = 66.5635, upper = 90),
  list(name = "All",       lower = 0,       upper = 90)
)

################################################################################
# Function to plot the air density over time across all sample airports        #
# Each worker is a CPU node that gets assigned one climate zone to parse       #
################################################################################

# Declare the function with the experiment (SSP) as input parameter
nc_parse <- function(lat) {
  
  print(lat$name)
  print(lat$lower)
  print(lat$upper)
  
  # Open the worker's connection to the database
  db_cnf <- ".my.cnf"                                                           # Set the file name that contains the database connection parameters
  db_grp <- "phd"                                                               # Set the group name within the cnf file that contains the connection parameters
  db_tbl <- "rho"                                                               # Set the name of the table where the air density data are stored
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

  # Query to average the air density for each experiment (SSP) separately, across all sample airports within each of the latitude brackets defined earlier
  db_qry <- paste(paste(paste("SELECT obs, AVG(val) AS avg_rho, '", nc_exps,"' AS exp FROM ", db_tbl, "_", nc_exps, " INNER JOIN population ON (", db_tbl, "_", nc_exps, ".apt = population.icao) WHERE ABS(population.lat) BETWEEN ", lat$lower," AND ", lat$upper," GROUP BY ", db_tbl, "_", nc_exps, ".obs", sep = ""), collapse = " UNION "), ";", sep = "") # Build the query
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

  # Release the database connection
  dbDisconnect(db_con)
  
} # End of the nc_parse function definition

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################

# Set the number of cores/workers to use in the cluster. Here we set as many workers as there are world climate zones for which we want to plot average rho
cores <- length(lat_names)

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