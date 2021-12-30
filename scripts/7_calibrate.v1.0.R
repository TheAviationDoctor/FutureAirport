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
library(ggplot2)
library(magrittr)
library(zoo)

# Import the common settings
source("scripts/0_common.R")

# Import the simulation functions
source("scripts/6_model.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

################################################################################
# Assemble the input data into a model lookup table                            #
################################################################################
  
  ##############################################################################
  # Import the OEM takeoff performance data used for calibrating the model     #
  ##############################################################################
  
  # List the CSV files
  oem <- list.files(path = "data/oem", pattern = "\\.csv$", full.names = TRUE)
  
  # Combine all the files into a list
  l1 <- lapply(
    oem,
    fread,
    sep = ",",
    header = TRUE,
    colClasses = c("factor", "numeric", "numeric")
  )
  
  ##############################################################################
  # Expand the data to add every integer mass value for model lookup purposes  #
  ##############################################################################
  
  # List every integer between the minimum and maximum mass values by aircraft
  l2 <- lapply(l1, function(x){
    data.table(
      type = first(x[["type"]]),
      m = seq(from = floor(min(x[["m"]])), to = ceiling(max(x[["m"]])), by = 1),
      todr_cal = NA)
  })
  
  # Combine the lists
  dt_cal <- rbindlist(c(l1, l2))
  
  # Reorder the resulting data frame
  dt_cal <- dt_cal[order(type, m)]
  
  ##############################################################################
  # Interpolate TODR values for every newly-added integer mass value           #
  ##############################################################################
  
  # Interpolate missing TODR values
  dt_cal[, todr_cal := na.approx(dt_cal$todr_cal, dt_cal$m, na.rm = TRUE)]
  str(dt_cal)
  
  # Remove the original rows from l1 where the mass is not an integer
  dt_cal <- dt_cal[m %% 1 == 0]
  
  # Change the class of the mass column to integer
  dt_cal <- dt_cal[, m := as.integer(m)]
  
  ##############################################################################
  # Import and merge the aircraft characteristics for model lookup purposes    #
  ##############################################################################
  
  # Import the CSV data (from Sun et al., 2020) into a data table
  dt_act <- fread(
    file = "data/aer/aircraft.csv",
    header = TRUE,
    colClasses = c(rep("factor", 3), rep("integer", 3), rep("numeric", 8))
  )
  
  # Merge the aircraft and OEM takeoff performance data into one data table
  dt <- merge(x = dt_act, y = dt_cal, by = "type", allow.cartesian = TRUE)
  
  # The model won't consider mass in excess of MTOM, so remove values above
  dt <- dt[m < mtom]
  
  # The model won't decrement mass by more than 10%, so remove values below
  dt <- dt[m >= mtom * .99]
  
  # Calibration data used TOGA, so set the takeoff thrust reduction to zero
  sim$reg_rto <- 0L
  
  ##############################################################################
  # Add ISA conditions to the model lookup table                               #
  ##############################################################################
  
  # Set the sea-level relative humidity in % at ISA
  dt[, hurs := sim$hurs]
  
  # Set the sea-level air pressure in Pa at ISA
  dt[, ps := sim$ps]
  
  # Set the sea-level air temperature in K at ISA
  dt[, tas := sim$tas]
  
  # Set the sea-level air density in kg/m³ at ISA
  dt[, rho := sim$rho]
  
  # Set the headwind in m/s
  dt[, hdw := sim$hdw]
  
### TEMPORARY REDUCE DT SIZE FOR TESTING
  
  # dt <- head(dt, 5)
  
### END TEMPORARY

################################################################################
# Optimize the model by finding the cL that minimizes the RSS for each pair    #
################################################################################
  
  ##############################################################################
  # Define a function to calculate TODR in m for each assumed cL               #
  ##############################################################################
  
  fn_sim_cal <- function(cal_cL) {

    # Save the lift coefficient to the data table
    dt[i, cL := cal_cL]

    # Save the drag coefficient to the data table
    dt[i, cD := fn_sim_cd(dt[i, ], cal_cL)]

    # Save the resulting simulated TODR to the data table
    dt[i, todr_sim := fn_sim_tko(dt[i, ])]

    # Save the percentage of difference between the calibrated and simulated TODR to the data table
    dt[i, diff := round(abs(todr_cal - todr_sim) / todr_cal * 100, 3)]

    # Save the residual sum of squares between calibrated and simulated TODR values to the data table
    dt[i, rss := sum((todr_sim - todr_cal)^2)]

    # Return the residual sum of squares
    return(dt[i, rss])

  } # End of the fn_sim_cal function
  
  ##############################################################################
  # Run the function row-wise over the model lookup table                      #
  ##############################################################################
  
  # For each calibrated takeoff mass/distance pair
  for (i in 1:nrow(dt)) {

    # Output progress to the console
    print(paste("Optimizing cL for observation ", i, " out of ", nrow(dt), ".", sep = ""))

    # Run the optimizer
    opt <- optimize(f = fn_sim_cal, interval = c(0.01, 100), tol = .0001)

    # Save the optimal cL
    dt[i, cL := opt$minimum]

  } # End of the for loop
  
  fwrite(dt, file = "output.csv")
  
################################################################################
# Save the results to the database                                             #
################################################################################
  
  ##############################################################################
  # Set up the database table to store the lookup data                         #
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
  db_qry <- paste("CREATE TABLE", tolower(db_cal), "(id INT UNSIGNED NOT NULL AUTO_INCREMENT, type CHAR(4) NOT NULL, m MEDIUMINT NOT NULL, cl FLOAT NOT NULL, PRIMARY KEY (id));", sep = " ")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Release the database resource
  dbClearResult(db_res)
  
  ##############################################################################
  # Write the data in tidy format to the database                              #
  ##############################################################################
  
  # Select which columns to write to the database and in which order
  cols <- c("type", "m", "cl")
  
  # Write the data
  # Here we use the deprecated RMySQL::MySQL() driver instead of the newer RMariaDB::MariaDB()) driver because it was found to be ~2.8 times faster here
  dbWriteTable(conn = db_con, name = tolower(db_cal), value = dt[, ..cols], append = TRUE, row.names = FALSE)
  
  # Disconnect from the database
  dbDisconnect(db_con)
  
################################################################################
# Plot the results                                                             #
################################################################################

# Build and save the plot to a file
(ggplot(data = dt) +
  geom_point(mapping = aes(x = todr_cal, y = m), color = "black", size = 2) +
  geom_line(mapping = aes(x = todr_sim, y = m), color = "magenta", size = 1) +
  scale_x_continuous("Regulatory takeoff distance required in m", labels = scales::comma) +
  scale_y_continuous("Takeoff mass in kg", labels = scales::comma) +
  facet_wrap(~ type, ncol = 2, scales = "free") +
  theme_light()) %>%
  ggsave(
    filename = "todr_cal.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

################################################################################
# Write the calibration data to the database                                   #
################################################################################

# # Select which columns to write to the database and in which order
# cols <- c("obs", "icao", "exp", "hurs", "ps", "tas", "uas", "vas", "rho", "wnd_dir", "wnd_spd", "wnd_hdw", "rwy", "toda")
# 
# # Write the data
# # Here we use the deprecated RMySQL::MySQL() driver instead of the newer RMariaDB::MariaDB()) driver because it was found to be ~2.8 times faster here
# dbWriteTable(conn = db_con, name = tolower(db_cli), value = dt_nc[, ..cols], append = TRUE, row.names = FALSE)
# 
# # Disconnect the worker from the database
# dbDisconnect(db_con)

################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF