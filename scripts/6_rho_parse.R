################################################################################
# /scripts/6_rho_parse.R                                                       #
# Plots the air density over time                                              #
#  Took 20 mins. to run on the researchers' config (https://bit.ly/3ChCBAP)    #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(parallel)

# Clear the console
cat("\014")

################################################################################
# List all the experiments (SSPs) from the NetCDF file names                   #
################################################################################

nc_path  <- "data/climate/netcdf"                                               # Set the file path where the NetCDF files are located
nc_files <- list.files(path = nc_path, pattern = "\\.nc$", full.names = TRUE)   # List all the NetCDF files (ending in .nc)
nc_exps  <- unique(lapply(strsplit(basename(nc_files), "_"), "[", 4))           # List all the experiments (SSPs) from the NetCDF file names

################################################################################
# Define the latitude brackets for which we want to plot the air density       #
################################################################################

lats <- list(
  list(name = "Tropical",  lower = 0,       upper = 23.4365),
  list(name = "Temperate", lower = 23.4365, upper = 66.5635),
  list(name = "Frigid",    lower = 66.5635, upper = 90),
  list(name = "All",       lower = 0,       upper = 90)
)

################################################################################
# Function to plot the air density over time across all sample airports        #
# Each worker is a CPU node that gets assigned one climate zone to parse       #
################################################################################

# Declare the function with the latitude bracket as input parameter
nc_parse <- function(lat) {

  # Open the worker's connection to the database
  db_cnf <- ".my.cnf"                                                           # Set the file name that contains the database connection parameters
  db_grp <- "phd"                                                               # Set the group name within the cnf file that contains the connection parameters
  db_pop <- "population"                                                        # Set the name of the table where the population data are stored
  db_rho <- "rho"                                                               # Set the prefix of the table where the air density data are stored
  thresh <- 10^6                                                                # Set the sample airports' minimum traffic size
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), "Worker", Sys.getpid(), "is averaging the air density across", tolower(lat$name), "zones...", sep = " "))

  # Query to average the air density for each experiment (SSP) separately, across all sample airports within each of the latitude brackets defined earlier
  ifelse(
    lat$name == "All",
    db_qry <- paste(paste(paste("SELECT obs, AVG(val) AS avg_rho, '", nc_exps, "' AS exp FROM ", db_rho, "_", nc_exps, " GROUP BY obs", sep = ""), collapse = " UNION "), ";", sep = ""),
    db_qry <- paste("WITH cte1 AS (SELECT DISTINCT icao FROM ", db_pop, " WHERE traffic > ", thresh," AND ABS(lat) BETWEEN ", lat$lower," AND ", lat$upper,") ", paste(paste("SELECT obs, AVG(val) AS avg_rho, '", nc_exps, "' AS exp FROM ", db_rho, "_", nc_exps, " INNER JOIN cte1 WHERE ", db_rho, "_", nc_exps, ".icao = cte1.icao GROUP BY obs", sep = ""), collapse = " UNION "), ";", sep = "")
  )
  db_res <- dbSendQuery(db_con, db_qry)                                           # Send the query to the database
  dt_rho <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), check.names = TRUE)) # Convert the query output from data frame to data table by reference (using setDT rather than as.data.table) to avoid duplication in memory
  dt_rho[, obs := as.POSIXct(obs, format = "%Y-%m-%d %H:%M:%S")]                  # Convert the observations' time stamps back to POSIXct format for plotting
  dt_rho[, exp := as.factor(exp)]                                                 # Convert the experiment from character to factor
  dbClearResult(db_res)

  # Print to the log file the average air density in the first and last years of the dataset, for every experiment (SSP)
  print(dt_rho[obs < "2016-01-01 00:00:00", .("2015" = mean(avg_rho)), by = .(exp)])
  print(dt_rho[obs > "2100-01-01 00:00:00", .("2100" = mean(avg_rho)), by = .(exp)])
  
  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), "Worker", Sys.getpid(), "is now plotting the air density across the", tolower(lat$name), "zones...", sep = " "))

  # Plot the air density over time
  plot <- ggplot(data = dt_rho, mapping = aes(x = obs, y = avg_rho)) +
    geom_line() +
    geom_smooth(method = "lm", formula = y ~ x) +
    labs(x = "Time", y = "Air density", title = paste("Average air density at airports across", tolower(lat$name), "zones", sep = " ")) +
    facet_wrap(~ exp, ncol = 2) +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
    ylim(1.13, 1.5)

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
  print(paste(Sys.time(), "Worker", Sys.getpid(), "finished its work on the", tolower(lat$name), "zones.", sep = " "))

  # Release the database connection
  dbDisconnect(db_con)
  
} # End of the nc_parse function definition

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################

# Set the number of cores/workers to use in the cluster. Here we set as many workers as there are world climate zones for which we want to plot average rho
cores <- length(lats)

# Set and clear the output file for cluster logging
outfile <- "logs/rho_parse.log"
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
clusterExport(cl, c("nc_exps"))

# Distribute the parallel parsing of NetCDF files across the workers, with the climate variables as input parameter
parLapply(cl, lats, nc_parse)

# Terminate the cluster once finished
stopCluster(cl)

# EOF