# ==============================================================================
#    NAME: scripts/9_analyze.R
#   INPUT: X rows of simulated takeoff from the database
# ACTIONS: Assemble the aircraft, calibration, and climatic data
#          Perform simulated takeoffs for each aircraft type and climate obs.
#          Write the resulting takeoff distance required to the database
#          Index the database table
#  OUTPUT: 442,769,456 rows of takeoff data written to the database
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load required libraries
# library(parallel)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Set a Function to plot the air density over time across all sample airports
# ==============================================================================

# Declare the function with the latitude bracket as input parameter
fn_analyze <- function(zone) {
  
  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      "Averaging the air density across", tolower(zone$name), "zones",
      sep = " "
    )
  )
  
  # Build a query to average the air density for each experiment/SSP separately,
  # across all sample airports within each of the latitude brackets.
  ifelse(
    zone$name == "All",
    db_qry <- paste(
      "SELECT obs, AVG(rho) AS avg_rho, exp FROM", db$cli,
      "GROUP BY obs, exp;",
      sep = " "
    ),
    db_qry <- paste(
      "WITH cte1 AS (SELECT DISTINCT icao FROM ", db$pop,
      " WHERE traffic > ", sim$pop_thr,
      " AND ABS(lat) BETWEEN ", zone$lower,
      " AND ", zone$upper,") ",
      paste(
        "SELECT obs, AVG(rho) AS avg_rho, exp FROM ", db$cli,
        " INNER JOIN cte1 ON ", db$cli,
        ".icao = cte1.icao GROUP BY obs, exp",
        sep = ""
      ),
      ";",
      sep = ""
    )
  )

  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)

  # Save the output to a data table
  dt_rho <- suppressWarnings(
    setDT(
      dbFetch(
        db_res,
        n = Inf
      ),
      check.names = TRUE
    )
  )
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Convert the observations' time stamps back to POSIXct format for plotting
  dt_rho[, obs := as.POSIXct(obs, format = "%Y-%m-%d %H:%M:%S")]

  # Convert the experiment (SSP) from character to factor
  dt_rho[, exp := as.factor(exp)]

  # Print to the log file the average air density in the first and last years of
  # the dataset, for every experiment (SSP)
  print(dt_rho[
    obs < "2016-01-01 00:00:00",
    .("2015" = mean(avg_rho)), by = .(exp)
    ]
  )
  print(dt_rho[
    obs > "2100-01-01 00:00:00",
    .("2100" = mean(avg_rho)),
    by = .(exp)
    ]
  )

  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      "Plotting the air density across the", tolower(zone$name), "zones",
      sep = " "
    )
  )
  
  # Plot the air density over time
  (ggplot(data = dt_rho, mapping = aes(x = obs, y = avg_rho)) +
    geom_line() +
    geom_smooth(method = "lm", formula = y ~ x) +
    labs(
      x = "Time",
      y = "Air density",
      title = paste(
        "Average air density at airports across",
        tolower(zone$name),
        "zones",
        sep = " "
      )
    ) +
    facet_wrap(~ exp, ncol = 2) +
    theme_light() +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    ) +
    ylim(1.1, 1.5)) %>%
    ggsave(
      paste("rho_", tolower(zone$name), ".png"),
      device = "png",
      path = d$plt,
      scale = 1,
      width = 6,
      height = NA,
      units = "in",
      dpi = "print"
    )

  # Disconnect the worker from the database
  dbDisconnect(db_con)
  
} # End of the fn_rho_plot function definition

# ==============================================================================
# 2 Handle the parallel computation across multiple cores
# ==============================================================================

# Latitudinal boundaries for the Earth's climate zones
# Subtropical is defined in Cortlett (2013) [https://doi.org/gw6j]
zones <- list(
  list(name = "Tropical",   lower = 0,       upper = 23.4365),
  list(name = "Subropical", lower = 23.4365, upper = 30),
  list(name = "Temperate",  lower = 23.4365, upper = 66.5635),
  list(name = "Frigid",     lower = 66.5635, upper = 90),
  list(name = "All",        lower = 0,       upper = 90)
)

# # Set the number of workers to use in the cluster
# # Here we set as many workers as there are climate zones to plot
# cores <- length(nc_lats)
# 
# # Clear the log file
# close(file(description = f$out, open = "w"))
# 
# # Build the cluster of workers
# cl <- makeCluster(spec = cores, outfile = f$out)
# 
# # Have each worker load the libraries that they need
# clusterEvalQ(cl, {
#   library(data.table)
#   library(DBI)
#   library(ggplot2)
# })
# 
# # Distribute the zones across the workers
# parLapply(cl, nc_lats, fn_rho_plot)
# 
# # Terminate the cluster once finished
# stopCluster(cl)

# Distribute the work across the cluster
parallel_lapply(
  crs = length(nc_lats),
  pkg = c("data.table", "DBI", "tidyverse"),
  lst = zones,
  fun = fn_analyze
)

# ==============================================================================
# 3 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF