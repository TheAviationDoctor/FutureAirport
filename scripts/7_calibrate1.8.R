# ==============================================================================
#    NAME: scripts/7_calibrate.R
#   INPUT: Takeoff model in 6_model.R
#          Published aircraft geometric and engine characteristics
#          Published takeoff mass/distance measured by aircraft manufacturers
# ACTIONS: Assume a starting value for the unknown lift coefficient cL
#          Simulate takeoffs at each mass value using that assumed cL
#          Compare the resulting TODR with that published by the OEMs
#          Vary cL values until the residual error is minimized
#  OUTPUT: Optimized values for cL and cD
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load required libraries
library(data.table)
library(DBI)
library(ggplot2)
library(magrittr)
library(parallel)
library(zoo)

# Import the common settings
source("scripts/0_common.R")
source("scripts/6_model1.8.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Prepare the input data
# ==============================================================================

# ==============================================================================
# 1.1 Import the aircraft characteristics (from Sun et al., 2020)
# ==============================================================================

dt_act <- fread(
  file = f$act,
  header = TRUE,
  colClasses = c(rep("factor", 3), rep("integer", 3), rep("numeric", 8)),
  key = "type"
)

# ==============================================================================
# 1.2 Import the takeoff performance calibration data
# ==============================================================================

# List the CSV files to import
l0 <- list.files(path = d$cal, pattern = "\\.csv$", full.names = TRUE)

# Combine all the files into a list and add a column for the aircraft type
l1 <- Map(
  cbind,
  type = sub("\\.csv$", "", basename(l0)),
  lapply(
    l0,
    fread,
    sep = ",",
    header = FALSE,
    col.names = c("m", "todr_cal"),
    colClasses = c("integer", "numeric")
  )
)

# List every integer between the minimum and maximum mass values by aircraft
l2 <- lapply(l1, function(x) {
  data.table(
    type = first(x[["type"]]),
    m = seq(from = floor(min(x[["m"]])), to = ceiling(max(x[["m"]])), by = 1),
    todr_cal = NA
  )
})

# Combine the lists into a single data table
dt_cal <- rbindlist(c(l1, l2))

# FOR TESTING ONLY
# dt_cal <- rbindlist(l1)

# Remove duplicates values of type and mass created in l2
dt_cal <- unique(dt_cal, by = c("type", "m"))

# Reorder the resulting data frame
dt_cal <- dt_cal[order(type, m)]

# Interpolate missing TODR values by aircraft type
dt_cal <- dt_cal[, lapply(.SD, zoo::na.approx), by = type]

# Calculate the horizontal airborne distance after liftoff up to screen height
# Adapted from Gratton et al, 2020
dis_air <- sim$ft_to_m * sim$scrn_hght / cos(sim$climb_angle) * sim$tod_mul

# Decrease the TODR by the airborne distance, which is assumed constant
set(x = dt_cal, j = "todr_cal", value = dt_cal[, todr_cal] - dis_air)

# Round up the TODR values to the nearest greatest integer
# set(x = dt_cal, j = "todr_cal", value = ceiling(dt_cal[, todr_cal]))

# Change the class of the mass column to integer
# set(x = dt_cal, j = "m", value = as.integer(dt_cal[, m]))

# ==============================================================================
# 1.3 Set the climatic observations used for calibration
# ==============================================================================

set(x = dt_cal, j = "hurs", value = 0L)      # Sea-level relative humidity in %
set(x = dt_cal, j = "ps",   value = 101325L) # Sea-level air pressure in Pa
set(x = dt_cal, j = "tas",  value = 273.15)  # Sea-level air temperature in K
set(x = dt_cal, j = "rho",  value = 1.225)   # Sea-level air density in kg/m³
set(x = dt_cal, j = "hdw",  value = 0L)      # Near-surface headwind in m/s

# ==============================================================================
# 1.4 Assemble the calibration inputs
# Adapted from from Sun et al. (2020).
# ==============================================================================

# Combine calibration and aircraft data
dt_tko <- merge(x = dt_act, y = dt_cal, by = "type")

# Calculate the speed of sound in m/s for the given temperature in dry air
set(x = dt_tko, j = "Vsnd", value = sqrt(sim$gamma * sim$Rd * dt_tko[, tas]))

# Calculate the weight force W in N
set(x = dt_tko, j = "W", value = sim$g * dt_tko[, m])

# Calculate the air pressure ratio dP
# For calibration we assume sea-level ISA conditions (i.e. dP = 1)
set(x = dt_tko, j = "dP", value = dt_tko[, ps] / sim$ps_isa)

# Calculate the coefficients of thrust for the given air pressure ratio
set(x = dt_tko, j = "G0", value = .0606 * dt_tko[, bpr] + .6337)
set(
  x = dt_tko,
  j = "A",
  value = -.4327 * dt_tko[, dP]^2 + 1.3855 * dt_tko[, dP] + .0472
)
set(
  x = dt_tko,
  j = "Z",
  value = .9106 * dt_tko[, dP]^3 - 1.7736 * dt_tko[, dP]^2 + 1.8697 *
    dt_tko[, dP]
)
set(
  x = dt_tko,
  j = "X",
  value = .1377 * dt_tko[, dP]^3 - .4374 * dt_tko[, dP]^2 + 1.3003 *
    dt_tko[, dP]
)

# Maximum percentage of thrust reduction permissible under FAA AC 25-13 (1988)
# For calibration we assume 0% thrust reduction (TOGA thrust).
set(x = dt_tko, j = "rto", value = 0L)

# Calculate the cD portion attributable to flaps in non-clean configuration
set(
  x = dt_tko,
  j = "delta_cD_flaps",
  value = dt_tko[, lambda_f] * dt_tko[, cfc]^1.38 * dt_tko[, SfS] *
    sin(sim$flap_angle * pi / 180)^2
)

# Calculate the cD portion attributable to the landing gear
set(
  x = dt_tko,
  j = "delta_cD_gear",
  value = dt_tko[, m] * sim$g / dt_tko[, S] * 3.16E-5 * dt_tko[, m]^-.215
)

# Calculate the total drag coefficient in non-clean configuration
set(
  x = dt_tko,
  j = "cD0_total",
  value = dt_tko[, cD0] + dt_tko[, delta_cD_flaps] + dt_tko[, delta_cD_gear]
)

# Calculate the Oswald efficiency factor for the selected flap deflection
set(x = dt_tko, j = "delta_e_flaps", value = .0026 * sim$flap_angle)

# Calculate the aspect ratio
set(x = dt_tko, j = "ar", value = dt_tko[, span]^2 / dt_tko[, S])

# Calculate the lift-induced coefficient k in non-clean configuration
set(
  x = dt_tko,
  j = "k_total",
  value = 1 / (1 / dt_tko[, k] + pi * dt_tko[, ar] * dt_tko[, delta_e_flaps])
)

# ==============================================================================
# 2 Define a function to calibrate cL and cD for every m and TODR value pair
# Adapted from from Sun et al. (2020) and Blake (2009).
# ==============================================================================

fn_calibrate <- function(cL, i) {

  # Save the assumed lift coefficient
  set(x = dt_tko, i = i, j = "cL", value = cL)

  # Calculate and save the total drag coefficient cD in non-clean configuration
  set(
    x = dt_tko,
    i = i,
    j = "cD",
    value = dt_tko[i, cD0_total] + dt_tko[i, k_total] * cL^2
  )

  # Calculate the stall speed in m/s
  set(
    x = dt_tko,
    i = i,
    j = "Vs",
    value = sqrt(dt_tko[i, W] / (.5 * dt_tko[i, rho] * dt_tko[i, S] * cL))
  )

  # Calculate the liftoff speed in m/s
  set(
    x = dt_tko,
    i = i,
    j = "Vlof",
    value = dt_tko[i, Vs] * sim$vs_to_vlof
  )

  # Calculate the takeoff distance required TODR in m
  set(
    x = dt_tko,
    i = i,
    j = "todr_sim",
    value = fn_todr(DT = dt_tko[i, ])
  )

  # Calculate the lift/drag ratio
  set(
    x = dt_tko,
    j = "ratio",
    value = dt_tko[, cL] / dt_tko[, cD]
  )

  # Calculate the percentage of difference between calibrated and simulated TODR
  set(
    x = dt_tko,
    j = "diff",
    value = abs(dt_tko[, todr_sim] - dt_tko[, todr_cal]) /
      dt_tko[, todr_cal] * 100
  )

  # Return the absolute residual error in m to be minimized
  return(abs(dt_tko[i, todr_sim] - dt_tko[i, todr_cal]))

} # End of the fn_calibrate function

# ==============================================================================
# 3 Run an optimizer to find the cL that minimizes the TODR residual error
# ==============================================================================

# For each calibrated takeoff mass/distance pair
for (i in seq_len(nrow(dt_tko))) {

  # Output progress to the console
  print(paste(i, "/", nrow(dt_tko), sep = " "))

  # Run the optimizer on fn_calibrate to minimize the residual error
  optimize(
    f = function(cL) fn_calibrate(cL, i),
    interval = sim$clmax_range,
    tol = sim$optim_tol
  )

} # End of the for loop

# ==============================================================================
# 4 Save the results to the database
# ==============================================================================

# ==============================================================================
# 4.1 Set up the database table to store the calibration data
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build the query to drop the table, if it exists
db_qry <- paste("DROP TABLE IF EXISTS ", tolower(db$cal), ";", sep = "")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Build the query to create the table
db_qry <- paste(
  "CREATE TABLE",
  tolower(db$cal),
  "(id INT UNSIGNED NOT NULL AUTO_INCREMENT,
    type CHAR(4) NOT NULL,
    m MEDIUMINT NOT NULL,
    todr_cal SMALLINT NOT NULL,
    todr_sim SMALLINT NOT NULL,
    vlof FLOAT NOT NULL,
    cL FLOAT NOT NULL,
    cD FLOAT NOT NULL,
    ratio FLOAT NOT NULL,
    diff FLOAT NOT NULL,
    PRIMARY KEY (id));",
  sep = " "
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# ==============================================================================
# 4.2 Write the calibration data to the database table
# ==============================================================================

# Select which columns to write to the database and in which order
cols <- c(
  "type", "m", "todr_cal", "todr_sim", "Vlof", "cL", "cD", "ratio", "diff"
)

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Write the data
dbWriteTable(
  conn = db_con,
  name = tolower(db$cal),
  value = dt_tko[, ..cols],
  append = TRUE,
  row.names = FALSE
)

# Disconnect from the database
dbDisconnect(db_con)

# ==============================================================================
# 4.3 Index the database table
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build the query to create the index
db_qry <- paste(
  "CREATE INDEX ", tolower(db$idx),
  " ON ", tolower(db$cal), " (type, m);",
  sep = ""
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# ==============================================================================
# 5 Validate the calibration results
# ==============================================================================

# Keep only the mass-TODR value pairs from the calibrated data for validation
dt_tko <- dt_tko[m %% 250 == 0]

# ==============================================================================
# 5.1 Output summary statistics to the console
# ==============================================================================

# Summarize the takeoff speeds by aircraft type
dt_tko[, as.list(summary(Vlof)), by = type]

# Summarize the lift coefficients by aircraft type
dt_tko[, as.list(summary(cL)), by = type]

# Summarize the drag coefficients by aircraft type
dt_tko[, as.list(summary(cD)), by = type]

# Summarize the lift/drag ratios by aircraft type
dt_tko[, as.list(summary(ratio)), by = type]

# Summarize the differences between calibrated & simulated TODR by aircraft type
dt_tko[, as.list(summary(diff)), by = type]

# ==============================================================================
# 5.2 Generate and save plots
# ==============================================================================

# Box-plot the lift coefficient by aircraft type
(ggplot(data = dt_tko[, .(type, cL)], aes(x = type, y = cL)) +
  geom_boxplot() +
  labs(x = "Aircraft type", y = "cL") +
  theme_light()) %>%
  ggsave(
    filename = "cal_cl.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Box-plot the drag coefficient by aircraft type
(ggplot(data = dt_tko[, .(type, cD)], aes(x = type, y = cD)) +
  geom_boxplot() +
  labs(x = "Aircraft type", y = "cD") +
  theme_light()) %>%
  ggsave(
    filename = "cal_cd.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Box-plot the lift/drag ratio by aircraft type
(ggplot(data = dt_tko[, .(type, ratio)], aes(x = type, y = ratio)) +
  geom_boxplot() +
  labs(x = "Aircraft type", y = "cD / cL") +
  theme_light()) %>%
  ggsave(
    filename = "cal_ratio.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Box-plot the calibration accuracy by aircraft type
(ggplot(data = dt_tko[, .(type, diff)], aes(x = type, y = diff)) +
  geom_boxplot() +
  labs(
    x = "Aircraft type",
    y = "Difference (in %) between calibrated and simulated TODR"
  ) +
  theme_light()) %>%
  ggsave(
    filename = "cal_diff.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Plot the calibrated vs. simulated mass over TODR for each aircraft type
(ggplot(data = dt_tko) +
  geom_point(mapping = aes(x = todr_cal, y = m), color = "black", size = 2) +
  geom_line(mapping = aes(x = todr_sim, y = m), color = "gray", size = 1) +
  scale_x_continuous("TODR in m", labels = scales::comma) +
  scale_y_continuous("Takeoff mass in kg", labels = scales::comma) +
  facet_wrap(~type, ncol = 2, scales = "free") +
  theme_light()) %>%
  ggsave(
    filename = "cal_todr.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Plot the takeoff speed for each aircraft type
(ggplot(data = dt_tko[, .(type, Vlof)], aes(x = type, y = Vlof)) +
    geom_boxplot() +
    labs(
      x = "Aircraft type",
      y = "Takeoff speed in m/s"
    ) +
    theme_light()) %>%
  ggsave(
    filename = "cal_vlof.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF
