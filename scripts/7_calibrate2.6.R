# ==============================================================================
#    NAME: scripts/7_calibrate.R
#   INPUT: Takeoff model in 6_model.R
#          Published aircraft geometric and engine characteristics
#          Published takeoff mass/distance measured by aircraft manufacturers
# ACTIONS: Assume a starting value for the unknown lift coefficient CL
#          Simulate takeoffs at each mass value using that assumed CL
#          Compare the resulting TODR with that published by the OEMs
#          Vary CL values until the residual error is minimized
#  OUTPUT: Optimized values for CL and CD
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2022
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
library(stringr)
library(zoo)

# Import the common settings
source("scripts/0_common.R")
source("scripts/6_model2.6.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Import the aircraft characteristics (from Sun et al., 2020)
# ==============================================================================

# Load the file to a data table
dt_act <- fread(
  file = fls$act,
  header = TRUE,
  colClasses = c(rep("factor", 2), rep("integer", 5), rep("numeric", 5)),
  key = "type"
)

# Filter the data table for the relevant aircraft types
dt_act <- dt_act[type %in% act]

# Set the mass corresponding to a breakeven load factor
set(
  x = dt_act,
  j = "tom_belf",
  value = dt_act[, tom_max - floor(seats * (1 - sim$lf_belf)) * sim$pax_avg]
)

# Set the mass corresponding to a zero load factor
set(
  x = dt_act,
  j = "tom_zero",
  value = dt_act[, tom_max - (seats * sim$pax_avg)]
)

# ==============================================================================
# 2 Import the takeoff performance calibration data
# ==============================================================================

# List the takeoff performance calibration data files
l0 <- paste(dir$cal, "/", act, ".csv", sep = "")

# Combine all the files into one list
l1 <- Map(
  cbind,
  type = sub("\\.csv$", "", basename(l0)),
  lapply(
    l0,
    fread,
    sep = ",",
    header = FALSE,
    col.names = c("tom", "todr_cal"),
    colClasses = c("integer", "numeric")
  )
)

# Convert the list to a data table for plotting
dt_cal <- rbindlist(l1)

# Plot the calibrated mass over TODR for each aircraft type (pre-interpolation)
(ggplot(data = dt_cal) +
  geom_point(mapping = aes(x = tom, y = todr_cal), color = "black", size = .1) +
  geom_vline(
    data = dt_act,
    mapping = aes(xintercept = tom_belf),
    linetype = "longdash"
  ) +
  geom_vline(data = dt_act, mapping = aes(xintercept = tom_zero)) +
  scale_x_continuous("Takeoff mass in kg", labels = scales::comma) +
  scale_y_continuous("Regulatory TODR in m", labels = scales::comma) +
  facet_wrap(~type, ncol = 2, scales = "free") +
  theme_light()) %>%
  ggsave(
    filename = "7_line_pre_interpolation.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# ==============================================================================
# 3 Interpolate the takeoff performance calibration data
# ==============================================================================

# List every integer between the minimum and maximum mass values by aircraft
l2 <- lapply(l1, function(x) {
  data.table(
    type = first(x[["type"]]),
    tom  = seq(
      from = floor(min(x[["tom"]])),
      to = ceiling(max(x[["tom"]])), by = 1
    ),
    todr_cal = NA
  )
})

# Combine both lists into a single data table
dt_cal <- rbindlist(c(l1, l2))

# Remove duplicates values of type and mass created in l2
dt_cal <- unique(dt_cal, by = c("type", "tom"))

# Reorder the resulting data frame
dt_cal <- dt_cal[order(type, tom)]

# Interpolate missing TODR values by aircraft type
dt_cal <- dt_cal[, lapply(.SD, zoo::na.approx), by = type]

# Plot the calibrated mass over TODR for each aircraft type (post-interpolation)
(ggplot(data = dt_cal) +
  geom_point(mapping = aes(x = tom, y = todr_cal), color = "black", size = .1) +
  geom_vline(
    data = dt_act,
    mapping = aes(xintercept = tom_belf),
    linetype = "longdash"
  ) +
  geom_vline(data = dt_act, mapping = aes(xintercept = tom_zero)) +
  scale_x_continuous("Takeoff mass in kg", labels = scales::comma) +
  scale_y_continuous("Regulatory TODR in m", labels = scales::comma) +
  facet_wrap(~type, ncol = 2, scales = "free") +
  theme_light()) %>%
  ggsave(
    filename = "7_line_post_interpolation.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# ==============================================================================
# 4 Decompose the calibrated TODR values into their components
# ==============================================================================

# Calculate the regulatory component of the calibrated TODR
set(
  x = dt_cal,
  j = "dis_reg_cal",
  value = dt_cal[, todr_cal] - dt_cal[, todr_cal] / sim$tod_mul
)

# Calculate the airborne component of the calibrated TODR
set(
  x = dt_cal,
  j = "dis_air_cal",
  value = fn_dis_air()
)

# Calculate the ground component of the calibrated TODR
set(
  x = dt_cal,
  j = "dis_gnd_cal",
  value = dt_cal[, todr_cal] - dt_cal[, dis_reg_cal] - dt_cal[, dis_air_cal]
)

# ==============================================================================
# 5 Set the takeoff conditions used for calibration
# ==============================================================================

set(x = dt_cal, j = "hurs",    value = sim$isa_hur) # Relative humidity in %
set(x = dt_cal, j = "ps",      value = sim$isa_ps)  # Air pressure in Pa
set(x = dt_cal, j = "tas",     value = sim$isa_tas) # Air temperature in K
set(x = dt_cal, j = "rho",     value = sim$isa_rho) # Air density in kg/m³
set(x = dt_cal, j = "hdw",     value = sim$isa_hdw) # Headwind in m/s
set(x = dt_cal, j = "thr_red", value = sim$thr_rto) # Thrust reduction in %

# ==============================================================================
# 6 Assemble the calibration inputs
# ==============================================================================

# Combine calibration and aircraft data
dt_tko <- merge(x = dt_act, y = dt_cal, by = "type")

# Remove masses below the break-even load factor
dt_tko <- dt_tko[tom >= tom_belf]

# ==============================================================================
# 7 Calculate the lift-induced drag coefficient k in takeoff configuration
# Adapted from from Sun et al. (2020).
# ==============================================================================

# Calculate the wing aspect ratio in extended flaps configuration
set(x = dt_tko, j = "ar", value = dt_tko[, span]^2 / dt_tko[, s])

# Calculate the Oswald factor component attributable to flaps
set(x = dt_tko, j = "e_flaps", value = .0026 * sin(sim$flp_ang * pi / 180))

# Calculate the total Oswald factor in takeoff configuration
set(x = dt_tko, j = "e_total", value = dt_tko[, e_clean] + dt_tko[, e_flaps])

# Calculate the total lift-induced coefficient k in takeoff configuration
set(
  x = dt_tko,
  j = "k_total",
  value = 1 / (1 / dt_tko[, k_clean] + pi * dt_tko[, ar] * dt_tko[, e_flaps])
)

# ==============================================================================
# 8 Define a function to calibrate CL and CD for every TOM and TODR value pair
# Adapted from from Sun et al. (2020) and Blake (2009).
# ==============================================================================

fn_calibrate <- function(clmax, i) {

  # Set the lift coefficient at maximum angle of attack
  set(x = dt_tko, i = i, j = "clmax", value = clmax)

  # Set the lift coefficient at liftoff
  set(x = dt_tko, i = i, j = "cllof", value = clmax / sim$max_lof)

  # Calculate the lift-induced drag coefficient
  set(
    x = dt_tko,
    i = i,
    j = "cdi",
    value = dt_tko[i, k_total] * dt_tko[i, cllof]^2
  )

  # Calculate the total drag coefficient
  set(
    x = dt_tko,
    i = i,
    j = "cd",
    value = dt_tko[i, cd0] + dt_tko[i, cdi]
  )

  # Calculate the liftoff speed in m/s
  set(x = dt_tko, i = i, j = "vlof", value = fn_vlof(DT = dt_tko[i, ]))

  # Calculate the ground component of the simulated TODR in m
  set(
    x = dt_tko,
    i = i,
    j = "dis_gnd_sim",
    value = fn_dis_gnd(DT = dt_tko[i, ])
  )

  # Set the airborne component of the simulated TODR in m
  set(
    x = dt_tko,
    i = i,
    j = "dis_air_sim",
    value = fn_dis_air()
  )

  # Calculate the regulatory component of the simulated TODR in m
  set(
    x = dt_tko,
    i = i,
    j = "dis_reg_sim",
    value = (dt_tko[i, dis_gnd_sim] + dt_tko[i, dis_air_sim]) *
      (sim$tod_mul - 1)
  )

  # Calculate the simulated TODR in m
  set(
    x = dt_tko,
    i = i,
    j = "todr_sim",
    value = dt_tko[i, dis_gnd_sim] + dt_tko[i, dis_air_sim] +
      dt_tko[i, dis_reg_sim]
  )

  # Calculate the absolute difference in m between calibrated and simulated TODR
  set(
    x = dt_tko,
    i = i,
    j = "diff",
    value = abs(dt_tko[i, todr_sim] - dt_tko[i, todr_cal])
  )

  # Return the absolute residual error in m
  return(dt_tko[i, diff])

} # End of the fn_calibrate function

# ==============================================================================
# 9 Run an optimizer to find the CL that minimizes the TODR residual error
# ==============================================================================

# For each calibrated takeoff mass/distance pair
for (i in seq_len(nrow(dt_tko))) {

  # Run the optimizer to minimize the residual error
  optimize(
    f = function(clmax) fn_calibrate(clmax, i),
    interval = sim$opt_rng,
    tol = sim$opt_tol
  )

  # Output results
  print(
    paste(
      "i =",
      str_pad(i, width = 5, side = "left", pad = " "),
      "/",
      str_pad(nrow(dt_tko), width = 5, side = "left", pad = " "),
      "| type = ",
      dt_tko[i, type],
      "| m =",
      str_pad(dt_tko[i, tom], width = 6, side = "left", pad = " "),
      "| CLmax =",
      format(x = dt_tko[i, clmax], digits = 3, nsmall = 3),
      "| CD =",
      str_pad(
        format(x = dt_tko[i, cd], digits = 3, nsmall = 3),
        width = 6, side = "right", pad = " "),
      "| Vlof =",
      format(x = dt_tko[i, vlof], digits = 1, nsmall = 1),
      "| diff =",
      format(x = dt_tko[i, diff], digits = 0, nsmall = 0),
      sep = " "
    )
  )

} # End of the for loop

# ==============================================================================
# 10 Save the results to the database
# ==============================================================================

# ==============================================================================
# 10.1 Set up the database table to store the calibration data
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

# Build the query to drop the table, if it exists
db_qry <- paste("DROP TABLE IF EXISTS ", tolower(dat$cal), ";", sep = "")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Build the query to create the table
db_qry <- paste(
  "CREATE TABLE",
  tolower(dat$cal),
  "(id INT UNSIGNED NOT NULL AUTO_INCREMENT,
    type CHAR(4) NOT NULL,
    tom MEDIUMINT NOT NULL,
    todr_cal SMALLINT NOT NULL,
    todr_sim SMALLINT NOT NULL,
    vlof FLOAT NOT NULL,
    clmax FLOAT NOT NULL,
    cllof FLOAT NOT NULL,
    cd FLOAT NOT NULL,
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
# 10.2 Write the calibration results to the database
# ==============================================================================

# Select which columns to write to the database and in which order
cols <- c(
  "type", "tom", "todr_cal", "todr_sim", "vlof", "clmax", "cllof", "cd"
)

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

# Write the data
dbWriteTable(
  conn = db_con,
  name = tolower(dat$cal),
  value = dt_tko[, ..cols],
  append = TRUE,
  row.names = FALSE
)

# Disconnect from the database
dbDisconnect(db_con)

# ==============================================================================
# 10.3 Index the database table
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

# Build the query to create the index
db_qry <- paste(
  "CREATE INDEX ", tolower(dat$idx),
  " ON ", tolower(dat$cal), " (type, tom);",
  sep = ""
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# ==============================================================================
# 11 Output summary statistics to the console
# ==============================================================================

# Summarize the takeoff speeds by aircraft type
dt_tko[, as.list(summary(vlof)), by = type]

# Summarize the lift coefficients by aircraft type
dt_tko[, as.list(summary(clmax)), by = type]

# Summarize the drag coefficients by aircraft type
dt_tko[, as.list(summary(cd)), by = type]

# Summarize the differences between calibrated & simulated TODR by aircraft type
dt_tko[, as.list(summary(diff)), by = type]

# ==============================================================================
# 12 Generate and save plots
# ==============================================================================

# Box-plot the lift coefficient by aircraft type
(ggplot(data = dt_tko[, .(type, clmax)], aes(x = type, y = clmax)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(x = "Aircraft type", y = "Lift coefficient (CLmax)") +
  theme_light()) %>%
  ggsave(
    filename = "7_box_clmax.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Box-plot the drag coefficient by aircraft type
(ggplot(data = dt_tko[, .(type, cd)], aes(x = type, y = cd)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(x = "Aircraft type", y = "Drag coefficient (CD)") +
  theme_light()) %>%
  ggsave(
    filename = "7_box_cd.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Box-plot the calibration accuracy in m by aircraft type
(ggplot(data = dt_tko[, .(type, diff)], aes(x = type, y = diff)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(
    x = "Aircraft type",
    y = "Difference (in m) between calibrated and simulated TODR"
  ) +
  theme_light()) %>%
  ggsave(
    filename = "7_box_diff.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Plot the takeoff speed for each aircraft type
(ggplot(data = dt_tko[, .(type, vlof)], aes(x = type, y = vlof)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(
    x = "Aircraft type",
    y = "Liftoff speed in m/s"
  ) +
  theme_light()) %>%
  ggsave(
    filename = "7_box_vlof.png",
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
  geom_point(mapping = aes(x = tom, y = todr_cal), color = "black", size = 2) +
  geom_line(mapping = aes(x = tom, y = todr_sim), color = "gray", size = 1) +
  scale_x_continuous("Takeoff mass in kg", labels = scales::comma) +
  scale_y_continuous("Regulatory TODR in m", labels = scales::comma) +
  facet_wrap(~type, ncol = 2, scales = "free") +
  theme_light()) %>%
  ggsave(
    filename = "7_line_todr_mass.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# ==============================================================================
# 13 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF
