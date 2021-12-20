################################################################################
#    NAME: scripts/7_calibrate.R                                               #
#   INPUT: Takeoff model contained in 6_model.R                                #
#          Published aircraft geometric and engine characteristics             #
#          Published takeoff mass/distance measured by aircraft manufacturers  #
# ACTIONS: Assume a starting value for the unknown lift coefficient cL         #
#          Simulate takeoffs at each mass value using that assumed cL          #
#          Compare the resulting TODR with that published by the OEMs          #
#          Vary cL values until the residual error (RSS) is minimized          #
#  OUTPUT: Optimized values for cL and cD (which is derived from cL)           #
# RUNTIME: ~X hours on the researcher's config (https://bit.ly/3ChCBAP)        #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(DBI)
library(ggplot2)
library(magrittr)

# Import the common settings
source("scripts/0_common.R")

# Import the simulation functions
source("scripts/6_model.v1.1.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

################################################################################
# Assemble the calibration data                                                #
################################################################################
  
  ##############################################################################
  # Import the aircraft characteristics (from Sun et al., 2020)                #
  ##############################################################################
  
  dt_act <- fread(
    file = paste(path_aer, aer_act, sep = "/"),
    header = TRUE,
    colClasses = c(rep("factor", 3), rep("integer", 3), rep("numeric", 8))
  )
  
  ##############################################################################
  # Import the takeoff performance calibration data                            #
  ##############################################################################
  
  # List the CSV files to import
  f <- list.files(path = path_oem, pattern = "\\.csv$", full.names = TRUE)
  
  # Combine all the files into a list and add a column for the aircraft type
  l1 <- Map(
    cbind,
    type = sub('\\.csv$', '', basename(f)),
    lapply(
      f,
      fread,
      sep = ",",
      header = FALSE,
      col.names = c("m", "todr_cal"),
      colClasses = c("integer", "numeric")
    )
  )
  
  # List every integer between the minimum and maximum mass values by aircraft
  l2 <- lapply(l1, function(x){
    data.table(
      type = first(x[["type"]]),
      m = seq(from = floor(min(x[["m"]])), to = ceiling(max(x[["m"]])), by = 1),
      todr_cal = NA
    )
  })
  
  # Combine the lists into a single data table
  dt_cal <- rbindlist(c(l1, l2))
  
  # Remove duplicates values of type and mass created in l2
  dt_cal <- unique(dt_cal, by = c("type", "m"))
  
  # Reorder the resulting data frame
  dt_cal <- dt_cal[order(type, m)]
  
  # Interpolate missing TODR values by aircraft type
  dt_cal <- dt_cal[, lapply(.SD, na.approx), by = type]
  
  # Change the class of the mass column to integer
  dt_cal <- dt_cal[, m := as.integer(m)]
  
  ##############################################################################
  # Import the climatic observations (ISA conditions)                          #
  ##############################################################################
  
  dt_cal[, hurs := sim$hurs ] # Sea-level relative humidity in % at ISA
  dt_cal[, ps   := sim$ps ]   # Sea-level air pressure in Pa at ISA
  dt_cal[, tas  := sim$tas ]  # Sea-level air temperature in K at ISA
  dt_cal[, rho  := sim$rho ]  # Sea-level air density in kg/m³ at ISA 
  dt_cal[, hdw  := sim$hdw ]  # Headwind in m/s
  
  ##############################################################################
  # Perform final assembly of the calibration data                             #
  ##############################################################################
  
  # FOR TESTING ONLY
  # Original values before interpolation
  dt_cal <- dt_cal[m %% 250 == 0]
  
  # Merge the climatic observations and aircraft characteristics
  dt <- merge(x = dt_act, y = dt_cal, by = "type")
  
  # Set the aircraft weight in N based upon the starting mass
  dt[, W := sim$g * m]
  
  # Set the takeoff thrust reduction to zero (calibration was done at TOGA)
  dt[, rto := 0L]
  
  # Initialize columns for the lift and drag coefficients
  dt[, cL := 0]
  dt[, cD := 0]
  
################################################################################
# Define calibration functions                                                 #
################################################################################
  
  ##############################################################################
  # Function fn_calibrate                                                      #
  # Calibrate cL and cD for every m and TODR value pair                        #
  ##############################################################################
  
  fn_calibrate <- function(cal_cL, i) {
    
    # Save the assumed lift coefficient to the data table
    dt[i, cL := cal_cL]
    
    # Calculate the cD portion attributable to flaps in non-clean configuration
    delta_cD_flaps <- dt[i, lambda_f] * dt[i, cfc]^1.38 *
      dt[i, SfS] * sin(sim$flap_angle * pi / 180)^2
    
    # Calculate the cD portion attributable to the landing gear
    delta_cD_gear <- dt[i, m] * sim$g / dt[i, S] * 3.16E-5 * dt[i, m]^-.215
    
    # Calculate the total drag coefficient in non-clean configuration
    cD0_total <- dt[i, cD0] + delta_cD_flaps + delta_cD_gear
    
    # Calculate the Oswald efficiency factor for the selected flap deflection
    delta_e_flaps <- .0026 * sim$flap_angle
    
    # Calculate the aspect ratio
    ar <- dt[i, span]^2 / dt[i, S]
    
    # Calculate the lift-induced coefficient k in non-clean configuration
    k_total <- 1 / (1 / dt[i, k] + pi * ar * delta_e_flaps)
    
    # Calculate the total drag coefficient in non-clean configuration
    # cD <- cD0_total + k_total * dt[i, cL]^2
    dt[i, cD := cD0_total + k_total * cL^2]
    
    # Save the TODR to the data table rounded to the nearest greater integer
    dt[i, todr_sim := ceiling(fn_todr(dt[i, ]))]
    
    # Save the ratio of cD over cL for verification purposes
    dt[i, ratio := cD/cL]
    
    # Save the percentage of difference between calibrated and simulated TODR
    dt[i, diff := round(abs(todr_cal - todr_sim) / todr_cal * 100, 2)]
    
    # Return the residual sum of squares
    return(sum((dt[i, todr_sim] - dt[i, todr_cal])^2))
    
  } # End of the fn_sim_cal function
  
  ##############################################################################
  # Function fn_optimize                                                       #
  # Run a minimization optimizer to achieve the best-fit cL                    #
  ##############################################################################
  
  fn_optimize <- function(batch) {
    
    print(batch)
    # print(total / cores * batch)
    
    # # For each calibrated takeoff mass/distance pair
    # for (i in (total / cores * batch):nrow(dt)) {
    #   
    #   # Output progress to the console
    #   print(paste("Optimizing cL for obs. ", i, "/", nrow(dt), ".", sep = ""))
    #   
    #   # Run the optimizer
    #   opt <- optimize(f = fn_calibrate, interval = c(0.1, 1), tol = .0015)
    #   
    #   # Save the optimal cL
    #   dt[i, cL := opt$minimum]
    #   
    # } # End of the for loop

  } # End of the fn_optimize function
  
  ##############################################################################
  # Perform parallel row-wise optimization of cL                               #
  ##############################################################################
  
  # Set the number of workers to use in the cluster
  cores <- 8

  # Set the log file for the cluster defined in 0_common.R
  outfile <- log_7

  # Clear the log file
  close(file(outfile, open = "w"))

  # Build the cluster of workers
  cl <- makeCluster(cores, outfile = outfile)

  # Have each worker load the libraries that they need
  clusterEvalQ(cl, {
    library(data.table)
  })
  
  # Count the number of rows in the data table to parse
  total <- nrow(dt)
  
  # Pass the required variables from the main scope to the workers' scope
  clusterExport(cl, c("db_cnf", "db_cli", "db_grp", "dt", "fn_calibrate", "fn_todr", "cores", "sim", "total"))

  # Distribute the unique airports across the workers
  # parApply(cl = cl, X = dt[, type], MARGIN = 1, FUN = fn_optimize)
  # parLapply(cl = cl, X = seq(1:cores), fun = fn_optimize)
  parLapply(cl = cl, X = dt[, mass], fun = fn_optimize)

  # Terminate the cluster once finished
  stopCluster(cl)

# stop("Test ended.")
  
  ##############################################################################
  # Validate the calibration results                                           #
  ##############################################################################
  
  # Validate only the mass-TODR value pairs from the calibrated data
  dt_val <- dt[m %% 250 == 0]
  
  # Summarize the ratio of cD over cL overall
  summary(dt_val[, ratio])
  
  # Summarize the ratio of cD over cL by aircraft type
  dt_val[, as.list(summary(ratio)), by = type]
  
  # Box plot the ratio of cD over cL by aircraft type
  (ggplot(data = dt_val[, .(type, ratio)], aes(x = type, y = ratio)) +
    geom_boxplot() +
    labs(x = "Aircraft type", y = "cD / cL") +
    theme_light()) %>%
    ggsave(
      filename = "cal_ratio.png",
      device = "png",
      path = path_plt,
      scale = 1,
      width = 6,
      height = NA,
      units = "in",
      dpi = "print"
    )
  
  # Summarize the calibration results overall
  summary(dt_val[, diff])
  
  # Summarize the calibration results by aircraft type
  dt_val[, as.list(summary(diff)), by = type]
  
  # Box plot the calibration results by aircraft type
  (ggplot(data = dt_val[, .(type, diff)], aes(x = type, y = diff)) +
    geom_boxplot() +
    labs(
      x = "Aircraft type",
      y = "Difference (in %) between calibrated and simulated TODR") +
    theme_light()) %>%
    ggsave(
      filename = "cal_diff.png",
      device = "png",
      path = path_plt,
      scale = 1,
      width = 6,
      height = NA,
      units = "in",
      dpi = "print"
    )
  
  # Plot the calibrated vs. simulated mass over TODR for each aircraft type
  # (ggplot(data = dt) +
  (ggplot(data = dt_val) +
    geom_point(mapping = aes(x = todr_cal, y = m), color = "black", size = 2) +
    geom_line(mapping = aes(x = todr_sim, y = m), color = "gray", size = 1) +
    scale_x_continuous("Regulatory TODR in m", labels = scales::comma) +
    scale_y_continuous("Takeoff mass in kg", labels = scales::comma) +
    facet_wrap(~ type, ncol = 2, scales = "free") +
    theme_light()) %>%
    ggsave(
      filename = "cal_todr.png",
      device = "png",
      path = path_plt,
      scale = 1,
      width = 6,
      height = NA,
      units = "in",
      dpi = "print"
    )

################################################################################
# Save the results to the database                                             #
################################################################################

  ##############################################################################
  # Set up the lookup table to store the calibration data                      #
  ##############################################################################
  
  # Connect to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

  # Build the query to drop the table, if it exists
  db_qry <- paste("DROP TABLE IF EXISTS ", tolower(db_cal), ";", sep = "")

  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)

  # Release the database resource
  dbClearResult(db_res)

  # Build the query to create the table
  db_qry <- paste(
    "CREATE TABLE",
    tolower(db_cal),
    "(id INT UNSIGNED NOT NULL AUTO_INCREMENT,
    type CHAR(4) NOT NULL,
    m MEDIUMINT NOT NULL,
    todr_cal SMALLINT NOT NULL,
    todr_sim SMALLINT NOT NULL,
    cL FLOAT NOT NULL,
    cD FLOAT NOT NULL,
    PRIMARY KEY (id));",
    sep = " ")

  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)

  # Release the database resource
  dbClearResult(db_res)
  
  ##############################################################################
  # Write the calibration data to the lookup table                             #
  # Here we use the deprecated RMySQL::MySQL() driver instead of the newer     #
  # RMariaDB::MariaDB()) driver because it was found to be faster.             #
  ##############################################################################
  
  # Select which columns to write to the database and in which order
  cols <- c("type", "m", "todr_cal", "todr_sim", "cL", "cD")
  
  # Write the data
  dbWriteTable(
    conn = db_con,
    name = tolower(db_cal),
    # value = dt,
    value = dt[, ..cols],
    append = TRUE,
    row.names = FALSE
  )

  ##############################################################################
  # Add an index to the database table                                         #
  ##############################################################################
  
  # Set the index name
  db_idx <- "idx"
  
  # Build the query to create the index
  # The simulation will look up data using aircraft type and mass in that order.
  db_qry <- paste(
    "CREATE INDEX ", tolower(db_idx),
    " ON ", tolower(db_cal), " (type, m);",
    sep = ""
  )
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect from the database
  dbDisconnect(db_con)

################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF