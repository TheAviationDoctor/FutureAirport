# ==============================================================================
#    NAME: scripts/7_calibrate.R
#   INPUT: Takeoff model contained in 6_model.R
#          Published aircraft geometric and engine characteristics
#          Published takeoff mass/distance measured by aircraft manufacturers
# ACTIONS: Assume a starting value for the unknown lift coefficient cL
#          Simulate takeoffs at each mass value using that assumed cL
#          Compare the resulting TODR with that published by the OEMs
#          Vary cL values until the residual error (RSS) is minimized
#  OUTPUT: Optimized values for cL and cD (which is derived from cL)
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load required libraries
library(data.table)
library(DBI)
library(ggplot2)
library(magrittr)
library(zoo)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Set the simulation variables
# ==============================================================================

# Maximum percentage of thrust reduction permissible under FAA AC 25-13 (1988)
# Use 0% thrust reduction, as the OEM calibration data were collected at TOGA.
reg_rto <- 0L

# Safety margin in percent applied to the horizontal distance along the takeoff
# path assuming all engines operating, from the start of the takeoff to a point
# equidistant between the point at which VLOF is reached and the point at which
# the airplane is 35 ft above the surface, according to 14 CFR § 25.113 (1998).
# Use 0% margin as the OEM calibration uses physical distances, not regulatory.
reg_dis <- 100L

# ==============================================================================
# 2 Assemble the calibration data
# ==============================================================================

# ==============================================================================
# 2.1 Import the aircraft characteristics (from Sun et al., 2020)
# ==============================================================================

dt_act <- fread(
  file = f$act,
  header = TRUE,
  colClasses = c(rep("factor", 3), rep("integer", 3), rep("numeric", 8))
)

# ==============================================================================
# 2.2 Import the takeoff performance calibration data
# ==============================================================================

# List the CSV files to import
f <- list.files(path = d$cal, pattern = "\\.csv$", full.names = TRUE)

# Combine all the files into a list and add a column for the aircraft type
l1 <- Map(
  cbind,
  type = sub("\\.csv$", "", basename(f)),
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
l2 <- lapply(l1, function(x) {
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
dt_cal <- dt_cal[, lapply(.SD, zoo::na.approx), by = type]

# Change the class of the mass column to integer
dt_cal[, m := as.integer(m)]

# ==============================================================================
# 2.3 Set the climatic observations used for calibration
# ==============================================================================

dt_cal[, hurs := 0L]      # Sea-level relative humidity in % at ISA
dt_cal[, ps   := 101325L] # Sea-level air pressure in Pa at ISA
dt_cal[, tas  := 273.15]  # Sea-level air temperature in K at ISA
dt_cal[, rho  := 1.225]   # Sea-level air density in kg/m³ at ISA
dt_cal[, hdw  := 0L]      # Near-surface headwind in m/s

# ==============================================================================
# 2.4 Perform final assembly of the calibration data
# ==============================================================================

# Merge the climatic observations and aircraft characteristics
dt <- merge(x = dt_act, y = dt_cal, by = "type")

# ==============================================================================
# 3 Define the function to calibrate cL and cD for every m and TODR value pair
# ==============================================================================


fn_calibrate <- function(cal_cL) {

  # ============================================================================
  # 3.1 Calculate the dimensionless drag coefficient cD
  # ============================================================================

  # Save the assumed lift coefficient to the data table
  dt[i, cL := cal_cL]

  # Calculate the cD portion attributable to flaps in non-clean configuration
  dt[i, delta_cD_flaps :=
    lambda_f * cfc^1.38 * SfS * sin(sim$flap_angle * pi / 180)^2]

  # Calculate the cD portion attributable to the landing gear
  dt[i, delta_cD_gear := m * sim$g / S * 3.16E-5 * m^-.215]

  # Calculate the total drag coefficient in non-clean configuration
  dt[i, cD0_total := cD0 + delta_cD_flaps + delta_cD_gear]

  # Calculate the Oswald efficiency factor for the selected flap deflection
  dt[i, delta_e_flaps := .0026 * sim$flap_angle]

  # Calculate the aspect ratio
  dt[i, ar := span^2 / S]

  # Calculate the lift-induced coefficient k in non-clean configuration
  dt[i, k_total := 1 / (1 / k + pi * ar * delta_e_flaps)]

  # Calculate the total drag coefficient in non-clean configuration
  dt[i, cD := cD0_total + k_total * cL^2]

  # ============================================================================
  # 3.2 Calculate the weight force W in N
  # ============================================================================

  dt[i, W := sim$g * m]

  # ============================================================================
  # 3.3 Calculate the speed in m/s at which L equals W, from Blake (2009)
  # ============================================================================

  dt[i, Vlof := sqrt(W / (.5 * rho * S * cL))]

  # Create airspeed intervals up to the minimum takeoff airspeed
  dt[i, Vtas := Map(seq, from = hdw, to = Vlof, length.out = sim$int)]

  # Create groundspeed intervals up to the minimum takeoff airspeed
  dt[i, Vgnd := Map(seq, from = 0, to = Vlof - hdw, length.out = sim$int)]

  # ============================================================================
  # 3.4 Calculate the dynamic pressure q in Pa, adapted from Blake (2009)
  # ============================================================================

  dt[i, q := Map("*", Vtas, (.5 * rho))]

  # ============================================================================
  # 3.5 Calculate the propulsive force F in N, adapted from Sun et al. (2020)
  # ============================================================================

  # Calculate the speed of sound in m/s for the given temperature in dry air
  dt[i, Vsnd := sqrt(sim$gamma * sim$Rd * tas)]

  # Calculate the Mach number for each airspeed interval
  dt[i, Vmach := Map("/", Vtas, Vsnd)]

  # Calculate the air pressure ratio
  dt[i, dP := ps / sim$ps_isa]

  # Calculate the coefficients of thrust
  dt[i, G0 := .0606 * bpr + .6337]
  dt[i, A := -.4327 * dP^2 + 1.3855 * dP + .0472]
  dt[i, Z := .9106 * dP^3 - 1.7736 * dP^2 + 1.8697 * dP]
  dt[i, X := .1377 * dP^3 - .4374 * dP^2 + 1.3003 * dP]

  # Calculate the thrust ratio
  dt[i, tr := Map(
    function(A, bpr, G0, Z, X, Vmach) {
      A - 0.377 * (1 + bpr) / sqrt((1 + 0.82 * bpr) * G0) * Z * Vmach +
        (0.23 + 0.19 * sqrt(bpr)) * X * Vmach^2
    },
    A = A, bpr = bpr, G0 = G0, Z = Z, X = X, Vmach = Vmach
  )]

  # Calculate the maximum takeoff thrust in N for each Mach number
  dt[i, Fmax := Map("*", tr, slst * n)]

  # Apply the maximum takeoff thrust reduction permissible
  dt[i, Frto := Map("*", Fmax, (100 - reg_rto) / 100)]

  # ============================================================================
  # 3.6 Calculate the acceleration in m/s² up to liftoff, from Blake (2009)
  # ============================================================================

  dt[i, a := Map(
    function(W, Frto, cD, cL, q, S) {
      sim$g / W * (Frto * 2 - (sim$mu * W) -
        (cD - (sim$mu * cL)) * (q * S) - (W * sin(sim$theta)))
    },
    W = W, Frto = Frto, cD = cD, cL = cL, q = q, S = S
  )]

  # ============================================================================
  # 3.7 Calculate the horizontal takeoff distances in m, from Blake (2009)
  # ============================================================================

  # Calculate the average acceleration between two groundspeed increments
  dt[i, a_avg := frollmean(x = a, n = 2, fill = a[[1]][1], align = "right")]

  # Calculate the average groundspeed between two groundspeed increments
  dt[i, Vgnd_avg := frollmean(x = Vgnd, n = 2, fill = 0, align = "right")]

  # Extract the size of the groundspeed interval
  dt[i, Vgnd_inc := sapply(dt[i, Vgnd], "[[", 2)]

  # Calculate the distance in meters covered within each speed increment
  dt[i, inc := Map("/", Map("*", Vgnd_avg, Vgnd_inc), a_avg)]

  # Increment the cumulative (running total) distance accordingly
  dt[i, cum := Map(cumsum, inc)]

  # Add the airborne distance from Vlof to screen height (Gratton et al, 2020)
  # and add the regulatory safety margin of 15% as per 14 CFR § 25.113 (1998)
  # applied to the takeoff run distance, which is from brake release to Vlof
  # + up to the middle point between Vlof and reaching screen height.
  dt[i, todr_sim := unlist(Map(
    function(cum) {
      (max(cum) + .3048 * 35 / cos(7.7) / 2) *
        (reg_dis / 100)
    },
    cum = cum
  ))]

  # ============================================================================
  # 3.8 Assemble the results
  # ============================================================================

  # Save the ratio of cD over cL for verification purposes
  dt[i, ratio := cD / cL]

  # Save the percentage of difference between calibrated and simulated TODR
  dt[i, diff := abs(todr_cal - todr_sim) / todr_cal * 100]

  # Calculate the residual sum of squares
  dt[i, rss := abs(todr_sim - todr_cal)^2]

  # Return the residual sum of squares
  return(dt[i, rss])
} # End of the fn_sim_cal function

# ==============================================================================
# 3.9 Run a minimization optimizer to find the best-fit cL
# ==============================================================================

# For each calibrated takeoff mass/distance pair
for (i in 1:nrow(dt)) {

  # Output progress to the console
  print(paste(i, "/", nrow(dt), sep = " "))

  # Run the optimizer
  opt <- optimize(f = fn_calibrate, interval = c(0.1, 1), tol = .0015)

  # Save the optimal cL
  dt[i, cL := opt$minimum]
} # End of the for loop

# ==============================================================================
# 4 Save the results to the database
# ==============================================================================

# ==============================================================================
# 4.1 Set up the lookup table to store the calibration data
# ==============================================================================

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
  sep = " "
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# ==============================================================================
# 4.2 Write the calibration data to the lookup table
# ==============================================================================

# Select which columns to write to the database and in which order
cols <- c("type", "m", "todr_cal", "todr_sim", "cL", "cD")

# Write the data
dbWriteTable(
  conn = db_con,
  name = tolower(db_cal),
  value = dt[, ..cols],
  append = TRUE,
  row.names = FALSE
)

# ==============================================================================
# 4.3 Add an index to the database table
# ==============================================================================

# Set the index name
db_idx <- "idx"

# Build the query to create the index
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

# ==============================================================================
# 5 Validate the calibration results
# ==============================================================================

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
    path = "plots",
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
(ggplot(data = dt_val) +
  geom_point(mapping = aes(x = todr_cal, y = m), color = "black", size = 2) +
  geom_line(mapping = aes(x = todr_sim, y = m), color = "gray", size = 1) +
  scale_x_continuous("Regulatory TODR in m", labels = scales::comma) +
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

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF
