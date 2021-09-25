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

# Import the constants
source("0_constants.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

################################################################################
# Function to plot the air density over time across all sample airports        #
# Each worker is a CPU node that gets assigned one climate zone to parse       #
################################################################################

# Declare the function with the latitude bracket as input parameter
fn_rho_plot <- function(lat) {
  
  # Import the constants
  source("0_constants.R")
  
  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

  # Output the worker's progress to the log file defined in makeCluster()
  print(paste(Sys.time(), "Worker", Sys.getpid(), "is averaging the air density across", tolower(lat$name), "zones...", sep = " "))

  # Query to average the air density for each experiment (SSP) separately, across all sample airports within each of the latitude brackets defined earlier
  ifelse(
    lat$name == "All",
    db_qry <- paste(paste(paste("SELECT obs, AVG(val) AS avg_rho, '", nc_exps, "' AS exp FROM ", db_rho, "_", nc_exps, " GROUP BY obs", sep = ""), collapse = " UNION "), ";", sep = ""),
    db_qry <- paste("WITH cte1 AS (SELECT DISTINCT icao FROM ", db_pop, " WHERE traffic > ", pop_thr," AND ABS(lat) BETWEEN ", lat$lower," AND ", lat$upper,") ", paste(paste("SELECT obs, AVG(val) AS avg_rho, '", nc_exps, "' AS exp FROM ", db_rho, "_", nc_exps, " INNER JOIN cte1 WHERE ", db_rho, "_", nc_exps, ".icao = cte1.icao GROUP BY obs", sep = ""), collapse = " UNION "), ";", sep = "")
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

  # Disconnect from the database
  dbDisconnect(db_con)
  
} # End of the fn_rho_plot function definition

################################################################################
# Handle the parallel computation across multiple cores                        #
################################################################################

# Set the number of cores/workers to use in the cluster. Here we set as many workers as there are world climate zones for which we want to plot average rho
cores <- length(nc_lats)

# Set and clear the output file for cluster logging
close(file(log_par, open = "w"))

# Build the cluster of workers and select a file in which to log progress (which can't be printed to the console on the Windows version of RStudio)
cl <- makeCluster(cores, outfile = log_par)

# Have each worker load the libraries that they need to handle the fn_rho_plot function defined above
clusterEvalQ(cl, {
  library(data.table)
  library(DBI)
  library(ggplot2)
})

# Distribute the parallel parsing of NetCDF files across the workers, with the climate variables as input parameter
parLapply(cl, nc_lats, fn_rho_plot)

# Terminate the cluster once finished
stopCluster(cl)

################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF