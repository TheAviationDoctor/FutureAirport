# ==============================================================================
#    NAME: scripts/7_calibrate.R
#   INPUT: OEM takeoff performance data under ISA conditions in dir$cal
# ACTIONS: Optimize lift and drag coefficients in 6_model.R to fit the OEM data
#  OUTPUT: 28,627 rows of takeoff calibration data written to the dat$cal table
# RUNTIME: ~50 minutes (3.8 GHz CPU / 128 GB DDR4 RAM / SSD)
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2023
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Clear the environment
rm(list = ls())

# Load the required libraries
library(data.table)
library(DBI)
library(ggplot2)
library(magrittr)
library(parallel)
library(stringr)
library(zoo)

# Import the common settings
source("scripts/0_common.R")

# Import the takeoff performance model
source("scripts/6_model.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Import the aircraft characteristics (from Sun et al., 2020)
# ==============================================================================

# Load the file to a data table
dt_act <- fread(
  file       = fls$act,
  header     = TRUE,
  colClasses = c(rep("factor", 2L), rep("integer", 5L), rep("numeric", 5L)),
  key        = "type"
)

# Filter the data table for the relevant aircraft types
dt_act <- dt_act[type %in% act]

# Set the mass corresponding to a breakeven load factor
set(
  x     = dt_act,
  j     = "tom_belf",
  value = dt_act[, tom_max - floor(seats * (1L - sim$lf_belf)) * sim$pax_avg]
)

# Set the mass corresponding to a zero load factor
set(
  x     = dt_act,
  j     = "tom_zero",
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
    X          = l0,
    FUN        = fread,
    sep        = ",",
    header     = TRUE,
    col.names  = c("tom", "todr_cal"),
    colClasses = c("integer", "numeric")
  )
)

# Convert the list to a data table for plotting
dt_cal <- rbindlist(l1)

# Plot the calibrated mass over TODR for each aircraft type (pre-interpolation)
(ggplot(data = dt_cal) +
  geom_point(mapping = aes(x = tom, y = todr_cal), color = "black", size = .1) +
  geom_vline(
    data     = dt_act,
    mapping  = aes(xintercept = tom_belf),
    linetype = "longdash"
  ) +
  geom_vline(data = dt_act, mapping = aes(xintercept = tom_zero)) +
  scale_x_continuous("Takeoff mass in kg", labels = scales::comma) +
  scale_y_continuous("Regulatory TODR in m", labels = scales::comma) +
  facet_wrap(~type, ncol = 2L, scales = "free") +
  theme_light()) %>%
  ggsave(
    filename = "7_pre_interpol.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 6L,
    height   = NA,
    units    = "in",
    dpi      = "print"
  )

# ==============================================================================
# 3 Interpolate the takeoff performance calibration data
# ==============================================================================

# List every integer between the minimum and maximum mass values by aircraft
l2 <- lapply(X = l1, FUN = function(x) {
  data.table(
    type   = first(x[["type"]]),
    tom    = seq(
      from = floor(min(x[["tom"]])),
      to   = ceiling(max(x[["tom"]])),
      by   = 1L
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
    data     = dt_act,
    mapping  = aes(xintercept = tom_belf),
    linetype = "longdash"
  ) +
  geom_vline(data = dt_act, mapping = aes(xintercept = tom_zero)) +
  scale_x_continuous("Takeoff mass in kg", labels = scales::comma) +
  scale_y_continuous("Regulatory TODR in m", labels = scales::comma) +
  facet_wrap(~type, ncol = 2L, scales = "free") +
  theme_light()) %>%
  ggsave(
    filename = "7_post_interpol.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 6L,
    height   = NA,
    units    = "in",
    dpi      = "print"
  )

# ==============================================================================
# 4 Decompose the calibrated TODR values into their components
# ==============================================================================

# Calculate the regulatory component of the calibrated TODR
set(
  x     = dt_cal,
  j     = "dis_reg_cal",
  value = dt_cal[, todr_cal] - dt_cal[, todr_cal] / sim$tod_mul
)

# Calculate the airborne component of the calibrated TODR
set(
  x     = dt_cal,
  j     = "dis_air_cal",
  value = fn_dis_air()
)

# Calculate the ground component of the calibrated TODR
set(
  x     = dt_cal,
  j     = "dis_gnd_cal",
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
set(x = dt_tko, j = "ar", value = dt_tko[, span]^2L / dt_tko[, s])

# Calculate the Oswald factor component attributable to flaps
set(x = dt_tko, j = "e_flaps", value = .0026 * sin(sim$flp_ang * pi / 180L))

# Calculate the total Oswald factor in takeoff configuration
set(x = dt_tko, j = "e_total", value = dt_tko[, e_clean] + dt_tko[, e_flaps])

# Calculate the total lift-induced coefficient k in takeoff configuration
set(
  x     = dt_tko,
  j     = "k_total",
  value = 1L / (1L / dt_tko[, k_clean] + pi * dt_tko[, ar] * dt_tko[, e_flaps])
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
    x     = dt_tko,
    i     = i,
    j     = "cdi",
    value = dt_tko[i, k_total] * dt_tko[i, cllof]^2L
  )

  # Calculate the total drag coefficient
  set(
    x     = dt_tko,
    i     = i,
    j     = "cd",
    value = dt_tko[i, cd0] + dt_tko[i, cdi]
  )

  # Calculate the liftoff speed in m/s
  set(x = dt_tko, i = i, j = "vlof", value = fn_vlof(DT = dt_tko[i, ]))

  # Calculate the ground component of the simulated TODR in m
  set(
    x     = dt_tko,
    i     = i,
    j     = "dis_gnd_sim",
    value = fn_dis_gnd(DT = dt_tko[i, ])
  )

  # Set the airborne component of the simulated TODR in m
  set(
    x     = dt_tko,
    i     = i,
    j     = "dis_air_sim",
    value = fn_dis_air()
  )

  # Calculate the regulatory component of the simulated TODR in m
  set(
    x     = dt_tko,
    i     = i,
    j     = "dis_reg_sim",
    value = (dt_tko[i, dis_gnd_sim] + dt_tko[i, dis_air_sim]) *
      (sim$tod_mul - 1L)
  )

  # Calculate the simulated TODR in m
  set(
    x     = dt_tko,
    i     = i,
    j     = "todr_sim",
    value = dt_tko[i, dis_gnd_sim] + dt_tko[i, dis_air_sim] +
      dt_tko[i, dis_reg_sim]
  )

  # Calculate the absolute difference in m between calibrated and simulated TODR
  set(
    x     = dt_tko,
    i     = i,
    j     = "diff",
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
    f        = function(clmax) fn_calibrate(clmax, i),
    interval = sim$opt_cls,
    tol      = sim$opt_tol
  )

  # Output results
  print(
    paste(
      "i =",
      str_pad(i, width = 5L, side = "left", pad = " "),
      "/",
      str_pad(nrow(dt_tko), width = 5L, side = "left", pad = " "),
      "| type = ",
      dt_tko[i, type],
      "| m =",
      str_pad(dt_tko[i, tom], width = 6L, side = "left", pad = " "),
      "| CLmax =",
      format(x = dt_tko[i, clmax], digits = 3L, nsmall = 3L),
      "| CD =",
      str_pad(
        format(x = dt_tko[i, cd], digits = 3L, nsmall = 3L),
        width = 6L, side = "right", pad = " "),
      "| Vlof =",
      format(x = dt_tko[i, vlof], digits = 1L, nsmall = 1L),
      "| diff =",
      format(x = dt_tko[i, diff], digits = NULL, nsmall = 0L),
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

fn_sql_qry(
  statement = paste("DROP TABLE IF EXISTS ", tolower(dat$cal), ";", sep = "")
)

fn_sql_qry(
  statement = paste(
    "CREATE TABLE",
    tolower(dat$cal),
    "(
      id       INT UNSIGNED NOT NULL AUTO_INCREMENT,
      type     CHAR(4) NOT NULL,
      tom      MEDIUMINT NOT NULL,
      todr_cal SMALLINT NOT NULL,
      todr_sim SMALLINT NOT NULL,
      vlof     FLOAT NOT NULL,
      clmax    FLOAT NOT NULL,
      cllof    FLOAT NOT NULL,
      cd       FLOAT NOT NULL,
    PRIMARY KEY (id));",
    sep = " "
  )
)

# ==============================================================================
# 10.2 Write the calibration results to the database
# ==============================================================================

# Select which columns to write to the database and in which order
cols <- c(
  "type", "tom", "todr_cal", "todr_sim", "vlof", "clmax", "cllof", "cd"
)

# Connect to the database
conn <- dbConnect(RMySQL::MySQL(), default.file = dat$cnf, group = dat$grp)

# Write the data
dbWriteTable(
  conn      = conn,
  name      = tolower(dat$cal),
  value     = dt_tko[, ..cols],
  append    = TRUE,
  row.names = FALSE
)

# Disconnect from the database
dbDisconnect(conn)

# ==============================================================================
# 10.3 Index the database table
# ==============================================================================

# Create the index
fn_sql_qry(
  statement = paste(
    "CREATE INDEX", tolower(dat$idx),
    "ON", tolower(dat$cal), "(type, tom);",
    sep = " "
  )
)

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
    filename = "7_clmax.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 6L,
    height   = NA,
    units    = "in",
    dpi      = "retina"
  )

# Box-plot the drag coefficient by aircraft type
(ggplot(data = dt_tko[, .(type, cd)], aes(x = type, y = cd)) +
  geom_boxplot() +
  stat_summary(fun = mean) +
  labs(x = "Aircraft type", y = "Drag coefficient (CD)") +
  theme_light()) %>%
  ggsave(
    filename = "7_cd.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 6L,
    height   = NA,
    units    = "in",
    dpi      = "retina"
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
    filename = "7_diff.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 6L,
    height   = NA,
    units    = "in",
    dpi      = "retina"
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
    filename = "7_vlof.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 6L,
    height   = NA,
    units    = "in",
    dpi      = "retina"
  )

# Plot the calibrated vs. simulated mass over TODR for each aircraft type
(ggplot(data = dt_tko) +
  geom_point(mapping = aes(x = tom, y = todr_cal), color = "black", size = 2L) +
  geom_line(mapping = aes(x = tom, y = todr_sim), color = "gray", size = 1L) +
  scale_x_continuous("Takeoff mass in kg", labels = scales::comma) +
  scale_y_continuous("Regulatory TODR in m", labels = scales::comma) +
  facet_wrap(~type, ncol = 2L, scales = "free") +
  theme_light()) %>%
  ggsave(
    filename = "7_todr_mass.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 6L,
    height   = NA,
    units    = "in",
    dpi      = "retina"
  )

# ==============================================================================
# 13 Housekeeping
# ==============================================================================

# Stop the script timer
Sys.time() - start_time

# EOF
