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
source("scripts/6_model2.0.R")

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

# Load the file
dt_act <- fread(
  file = f$act,
  header = TRUE,
  colClasses = c(rep("factor", 3), rep("integer", 3), rep("numeric", 7)),
  key = "type"
)

# Reduce the file to the aircraft selected for calibration
dt_act <- dt_act[type %in% sim$act_cal]

# ==============================================================================
# 1.2 Import the takeoff performance calibration data
# ==============================================================================

# List the CSV files to import corresponding to the aircraft to calibrate
l0 <- paste(d$cal, "/", sim$act_cal, ".csv", sep = "")

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
# dt_cal <- rbindlist(l1) # FOR TESTING ONLY

# Remove duplicates values of type and mass created in l2
dt_cal <- unique(dt_cal, by = c("type", "m"))

# Reorder the resulting data frame
dt_cal <- dt_cal[order(type, m)]

# Interpolate missing TODR values by aircraft type
dt_cal <- dt_cal[, lapply(.SD, zoo::na.approx), by = type]

# Calculate the horizontal regulatory component of the calibrated TODR in m
set(
  x = dt_cal,
  j = "todr_cal_reg",
  value = dt_cal[, todr_cal] - dt_cal[, todr_cal] / sim$tod_mul
)

# Calculate the net airborne component of the calibrated TODR in m
# Adapted from Gratton et al, 2020
dis_air <- sim$ft_to_m * sim$scrn_hght / cos(sim$climb_angle)

# Set the horizontal airborne component of the calibrated TODR in m
set(
  x = dt_cal,
  j = "todr_cal_air",
  value = dis_air
)

# Calculate the horizontal ground component of the calibrated TODR in m
set(
  x = dt_cal,
  j = "todr_cal_gnd",
  value = dt_cal[, todr_cal] - dt_cal[, todr_cal_reg] - dt_cal[, todr_cal_air]
)

# ==============================================================================
# 1.3 Set the climatic observations used for calibration to ISA conditions
# ==============================================================================

set(x = dt_cal, j = "hurs", value = 0L)      # Sea-level relative humidity in %
set(x = dt_cal, j = "ps",   value = 101325L) # Sea-level air pressure in Pa
set(x = dt_cal, j = "tas",  value = 273.15)  # Sea-level air temperature in K
set(x = dt_cal, j = "rho",  value = 1.225)   # Sea-level air density in kg/m³
set(x = dt_cal, j = "hdw",  value = 0L)      # Near-surface headwind in m/s

# ==============================================================================
# 1.4 Assemble the calibration inputs
# ==============================================================================

# Combine calibration and aircraft data
dt_tko <- merge(x = dt_act, y = dt_cal, by = "type")

# For calibration we assume 0% thrust reduction (i.e., maximum takeoff thrust)
set(x = dt_tko, j = "rto", value = 0L)

# ==============================================================================
# 1.5 Calculate the lift-induced drag coefficient k in takeoff configuration
# Adapted from from Sun et al. (2020).
# ==============================================================================

# Calculate the wing aspect ratio
set(x = dt_tko, j = "ar", value = dt_tko[, span]^2 / dt_tko[, S])

# Calculate the lift-induced coefficient k in takeoff configuration
set(x = dt_tko, j = "k", value = 1 / (pi * dt_tko[, ar] * dt_tko[, e]))

# ==============================================================================
# 2 Define a function that calibrates cL and cD for every m and TODR value pair
# Adapted from from Sun et al. (2020) and Blake (2009).
# ==============================================================================

fn_calibrate <- function(cL, i) {

  # Set the assumed lift coefficient passed by the optimizer
  set(x = dt_tko, i = i, j = "cL", value = cL)

  # Calculate the lift-induced drag coefficient
  set(
    x = dt_tko,
    i = i,
    j = "cDi",
    value = dt_tko[i, k] * cL^2 
  )

  # Calculate the total drag coefficient (zero-lift + lift-induced)
  set(
    x = dt_tko,
    i = i,
    j = "cD",
    value = dt_tko[i, cD0] + dt_tko[i, cDi]
  )

  # Calculate the liftoff speed in m/s
  set(x = dt_tko, i = i, j = "Vlof", value = fn_Vlof(DT = dt_tko[i, ]))

  # Calculate the ground component of the simulated TODR in m
  set(
    x = dt_tko,
    i = i,
    j = "todr_sim_gnd",
    value = fn_todr_gnd(DT = dt_tko[i, ])
  )

  # Set the airborne component of the simulated TODR in m
  set(
    x = dt_tko,
    i = i,
    j = "todr_sim_air",
    value = dis_air
  )

  # Calculate the regulatory component of the simulated TODR in m
  set(
    x = dt_tko,
    i = i,
    j = "todr_sim_reg",
    value = (dt_tko[i, todr_sim_gnd] + dt_tko[i, todr_sim_air]) *
      (sim$tod_mul - 1)
  )

  # Calculate the total simulated TODR in m
  set(
    x = dt_tko,
    i = i,
    j = "todr_sim",
    value = dt_tko[i, todr_sim_gnd] + dt_tko[i, todr_sim_air] +
      dt_tko[i, todr_sim_reg]
  )

  # Calculate the lift/drag ratio
  set(
    x = dt_tko,
    i = i,
    j = "ratio",
    value = dt_tko[i, cL] / dt_tko[i, cD]
  )

  # Calculate the absolute difference in m between calibrated and simulated TODR
  set(
    x = dt_tko,
    i = i,
    j = "diff_m",
    value = abs(dt_tko[i, todr_sim] - dt_tko[i, todr_cal])
  )

  # Calculate the absolute difference in % between calibrated and simulated TODR
  set(
    x = dt_tko,
    i = i,
    j = "diff_per",
    value = abs(dt_tko[i, todr_sim] - dt_tko[i, todr_cal]) /
      dt_tko[i, todr_cal] * 100
  )

  # Return the absolute residual error in m to be minimized by the optimizer
  return(dt_tko[i, diff_m])

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
    todr_cal_gnd SMALLINT NOT NULL,
    todr_cal_air SMALLINT NOT NULL,
    todr_cal_reg SMALLINT NOT NULL,
    todr_sim SMALLINT NOT NULL,
    todr_sim_gnd SMALLINT NOT NULL,
    todr_sim_air SMALLINT NOT NULL,
    todr_sim_reg SMALLINT NOT NULL,
    vlof FLOAT NOT NULL,
    cL FLOAT NOT NULL,
    cD FLOAT NOT NULL,
    cD0 FLOAT NOT NULL,
    cDi FLOAT NOT NULL,
    e FLOAT NOT NULL,
    k FLOAT NOT NULL,
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
# 4.2 Write the calibration results to the database
# ==============================================================================

# Select which columns to write to the database and in which order
cols <- c(
  "type", "m", "todr_cal", "todr_cal_gnd", "todr_cal_air", "todr_cal_reg",
  "todr_sim", "todr_sim_gnd", "todr_sim_air", "todr_sim_reg", "Vlof", "cL",
  "cD", "cD0", "cDi", "e", "k"
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
dt_tko[, as.list(summary(diff_m)), by = type]

# ==============================================================================
# 5.2 Generate and save plots
# ==============================================================================

# Box-plot the lift coefficient by aircraft type
(ggplot(data = dt_tko[, .(type, cL)], aes(x = type, y = cL)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(x = "Aircraft type", y = "Lift coefficient (cL)") +
  theme_light()) %>%
  ggsave(
    filename = "cal_box_cl.png",
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
  stat_summary(fun = mean) +
  labs(x = "Aircraft type", y = "Drag coefficient (cD)") +
  theme_light()) %>%
  ggsave(
    filename = "cal_box_cd.png",
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
  stat_summary(fun = mean) +
  labs(x = "Aircraft type", y = "Lift/drag ratio (cL/cD)") +
  theme_light()) %>%
  ggsave(
    filename = "cal_box_cl_cd_ratio.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Box-plot the calibration accuracy in m by aircraft type
(ggplot(data = dt_tko[, .(type, diff_m)], aes(x = type, y = diff_m)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(
    x = "Aircraft type",
    y = "Difference (in m) between calibrated and simulated TODR"
  ) +
  theme_light()) %>%
  ggsave(
    filename = "cal_box_diff_m.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Box-plot the calibration accuracy in % by aircraft type
(ggplot(data = dt_tko[, .(type, diff_per)], aes(x = type, y = diff_per)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(
    x = "Aircraft type",
    y = "Difference (in %) between calibrated and simulated TODR"
  ) +
  theme_light()) %>%
  ggsave(
    filename = "cal_box_diff_per.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Box-plot the calibration accuracy in m by aircraft type (log)
(ggplot(data = dt_tko[, .(type, diff_m)], aes(x = type, y = diff_m)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(
    x = "Aircraft type",
    y = "Difference (in m) between calibrated and simulated TODR"
  ) +
  scale_y_continuous(trans='log10') +
  theme_light()) %>%
  ggsave(
    filename = "cal_box_diff_m_log.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Box-plot the calibration accuracy in % by aircraft type (log)
(ggplot(data = dt_tko[, .(type, diff_per)], aes(x = type, y = diff_per)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(
    x = "Aircraft type",
    y = "Difference (in %) between calibrated and simulated TODR"
  ) +
  scale_y_continuous(trans='log10') +
  theme_light()) %>%
  ggsave(
    filename = "cal_box_diff_per_log.png",
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
    stat_summary(fun = mean) +
    labs(
      x = "Aircraft type",
      y = "Liftoff speed in m/s"
    ) +
    theme_light()) %>%
  ggsave(
    filename = "cal_box_vlof.png",
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
  geom_point(mapping = aes(x = m, y = todr_cal), color = "black", size = 2) +
  geom_line(mapping = aes(x = m, y = todr_sim), color = "gray", size = 1) +
  scale_x_continuous("Takeoff mass in kg", labels = scales::comma) +
  scale_y_continuous("Regulatory TODR in m", labels = scales::comma) +
  facet_wrap(~type, ncol = 2, scales = "free") +
  theme_light()) %>%
  ggsave(
    filename = "cal_line_todr_mass.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Plot the calibrated vs. simulated mass over cL for each aircraft type
(ggplot(data = dt_tko) +
  geom_line(mapping = aes(x = m, y = cL), color = "black", size = 1) +
  scale_x_continuous("Takeoff mass in kg", labels = scales::comma) +
  scale_y_continuous("Lift coefficient (cL)", labels = scales::comma) +
  facet_wrap(~type, ncol = 2, scales = "free") +
  theme_light()) %>%
  ggsave(
    filename = "cal_line_cl_mass.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Plot the calibrated vs. simulated mass over Vlof for each aircraft type
(ggplot(data = dt_tko) +
  geom_line(mapping = aes(x = m, y = Vlof), color = "black", size = 1) +
  scale_x_continuous("Takeoff mass in kg", labels = scales::comma) +
  scale_y_continuous("Liftoff speed in m/s", labels = scales::comma) +
  facet_wrap(~type, ncol = 2, scales = "free") +
  theme_light()) %>%
  ggsave(
    filename = "cal_line_vlof_mass.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Plot the cL vs. cD for each aircraft type
(ggplot(data = dt_tko) +
    geom_line(mapping = aes(x = cL, y = cD), color = "black", size = 1) +
    scale_x_continuous("cL", labels = scales::comma) +
    scale_y_continuous("cD", labels = scales::comma) +
    facet_wrap(~type, ncol = 2, scales = "free") +
    theme_light()) %>%
  ggsave(
    filename = "cal_line_cl_cd.png",
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
