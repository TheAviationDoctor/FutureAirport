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
source("scripts/6_model.R")

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

# Round up the TODR values
# set(x = dt_cal, j = "todr_cal", value = ceiling(dt_tko[, todr_cal]))

# Change the class of the mass column to integer
dt_cal[, m := as.integer(m)]

# ==============================================================================
# 1.3 Set the climatic observations used for calibration
# ==============================================================================

set(x = dt_cal, j = "hurs", value = 0L)      # Sea-level relative humidity in %
set(x = dt_cal, j = "ps",   value = 101325L) # Sea-level air pressure in Pa
set(x = dt_cal, j = "tas",  value = 273.15)  # Sea-level air temperature in K
set(x = dt_cal, j = "rho",  value = 1.225)   # Sea-level air density in kg/m³
set(x = dt_cal, j = "hdw",  value = 0L)      # Near-surface headwind in m/s
 
# ==============================================================================
# 1.4 Perform final assembly of the calibration data
# ==============================================================================

# Merge the climatic observations and aircraft characteristics
dt_tko <- merge(x = dt_act, y = dt_cal, by = "type")


# Calculate the speed of sound in m/s for the given temperature in dry air
set(x = dt_tko, j = "Vsnd", value = sqrt(sim$gamma * sim$Rd * dt_tko[, tas]))

# Calculate the weight force W in N
# dt_tko[, W := sim$g * m]
set(x = dt_tko, j = "W", value = sim$g * dt_tko[, m])



# Calculate the air pressure ratio
set(x = dt_tko, j = "dP", value = dt_tko[, ps] / sim$ps_isa)

# Calculate the coefficients of thrust for the given air pressure ratio
set(x = dt_tko, j = "G0", value = .0606 * dt_tko[, bpr] + .6337)
set(x = dt_tko, j = "A", value = -.4327 * dt_tko[, dP]^2 + 1.3855 * dt_tko[, dP] + .0472)
set(x = dt_tko, j = "Z", value = .9106 * dt_tko[, dP]^3 - 1.7736 * dt_tko[, dP]^2 + 1.8697 * dt_tko[, dP])
set(x = dt_tko, j = "X", value = .1377 * dt_tko[, dP]^3 - .4374 * dt_tko[, dP]^2 + 1.3003 * dt_tko[, dP])

# Maximum percentage of thrust reduction permissible under FAA AC 25-13 (1988)
# Use 0% thrust reduction, as the OEM calibration data were collected at TOGA.
set(x = dt_tko, j = "rto", value = 0L)

# Safety margin in percent applied to the horizontal distance along the takeoff
# path assuming all engines operating, from the start of the takeoff to a point
# equidistant between the point at which VLOF is reached and the point at which
# the airplane is 35 ft above the surface, according to 14 CFR § 25.113 (1998).
# Use 0% margin as the OEM calibration uses physical distances, not regulatory.
set(x = dt_tko, j = "sft", value = 1L)

# Calculate the cD portion attributable to flaps in non-clean configuration
# dt_tko[, delta_cD_flaps :=
#   lambda_f * cfc^1.38 * SfS * sin(sim$flap_angle * pi / 180)^2]
set(x = dt_tko, j = "delta_cD_flaps",  value = dt_tko[, lambda_f] * dt_tko[, cfc]^1.38 * dt_tko[, SfS] * sin(sim$flap_angle * pi / 180)^2)

# Calculate the cD portion attributable to the landing gear
# dt_tko[, delta_cD_gear := m * sim$g / S * 3.16E-5 * m^-.215]
set(x = dt_tko, j = "delta_cD_gear", value = dt_tko[, m] * sim$g / dt_tko[, S] * 3.16E-5 * dt_tko[, m]^-.215)

# Calculate the total drag coefficient in non-clean configuration
# dt_tko[, cD0_total := cD0 + delta_cD_flaps + delta_cD_gear]
set(x = dt_tko, j = "cD0_total", value = dt_tko[, cD0] + dt_tko[, delta_cD_flaps] + dt_tko[, delta_cD_gear])

# Calculate the Oswald efficiency factor for the selected flap deflection
# dt_tko[, delta_e_flaps := .0026 * sim$flap_angle]
set(x = dt_tko, j = "delta_e_flaps", value = .0026 * sim$flap_angle)

# Calculate the aspect ratio
# dt_tko[, ar := span^2 / S]
set(x = dt_tko, j = "ar", value = dt_tko[, span]^2 / dt_tko[, S])

# Calculate the lift-induced coefficient k in non-clean configuration
# dt_tko[, k_total := 1 / (1 / k + pi * ar * delta_e_flaps)]
set(x = dt_tko, j = "k_total", value = 1 / (1 / dt_tko[, k] + pi * dt_tko[, ar] * dt_tko[, delta_e_flaps]))

# ==============================================================================
# 2 Define a function to calibrate cL and cD for every m and TODR value pair
# ==============================================================================

fn_calibrate <- function(cL) {

  # ============================================================================
  # 2.1 Calculate the dimensionless drag coefficient cD
  # ============================================================================

  # Save the assumed lift coefficient to the data table
  # dt_tko[, cL := cal_cL]
  set(x = dt_tko, i = i, j = "cL",  value = cL)

  

  # Calculate the total drag coefficient in non-clean configuration
  # dt_tko[, cD := cD0_total + k_total * cL^2]
  set(x = dt_tko, i = i, "cD", value = dt_tko[i, cD0_total] + dt_tko[i, k_total] * cL^2)



  # Calculate the speed Vlof in m/s at which lift L equals weight W
  # Adapted from Blake (2009).
  set(
    x = dt_tko,
    i = i,
    j = "Vlof",
    value = sqrt(
      dt_tko[i, W] / (.5 * dt_tko[i, rho] * dt_tko[i, S] * cL)
    )
  )

  # ========================================================================
  # 2.3.2 Calculate the takeoff distance required TODR in m
  # ========================================================================

  set(x = dt_tko, i = i, j = "todr", value = fn_todr(dt_tko[i, ]))

  # ============================================================================
  # 3.8 Assemble the results
  # ============================================================================

  # Save the ratio of cD over cL for verification purposes
  # dt_tko[, ratio := cD / cL]
  # set(x = dt_tko, j = "ratio", value = dt_tko[, cD] / cL)

  # Save the percentage of difference between calibrated and simulated TODR
  # dt_tko[, diff := abs(todr_cal - todr) / todr_cal * 100]
  # set(x = dt_tko, j = "diff", value = abs(dt_tko[, todr_cal] - dt_tko[, todr]) / dt_tko[, todr_cal] * 100)

  # Calculate the residual sum of squares
  # dt_tko[, rss := abs(todr - todr_cal)^2]
  set(x = dt_tko, i = i, j = "rss", value = abs(dt_tko[i, todr] - dt_tko[i, todr_cal])^2)

  # Return the residual sum of squares
  return(dt_tko[i, rss])
  
} # End of the fn_calibrate function

# ==============================================================================
# 3.9 Run a minimization optimizer to find the best-fit cL
# ==============================================================================

fn_test <- function() {
  
  print(i)
  
}

# For each calibrated takeoff mass/distance pair
for (i in seq_len(nrow(dt_tko))) {

  # Output progress to the console
  print(paste(i, "/", nrow(dt_tko), sep = " "))

  # Run the optimizer
  opt <- optimize(f = fn_calibrate, interval = c(0.1, 1), tol = .0015)

  # Save the optimal cL
  dt_tko[, cL := opt$minimum]
  
  # fn_test()

} # End of the for loop

# dt_tko[, cL := optimize(f = fn_calibrate, interval = c(0.1, 1), tol = .0015)$minimum, by = seq_len(nrow(dt_tko))]

# dt_tko[, cL := apply(.SD, 1, optimize, f = fn_calibrate, interval = c(0.1, 1), tol = .0015), .SDcols=c("A", "bpr", "G0", "hdw", "n", "rho", "S", "slst", "Vsnd", "W", "X", "Z"), by = .I]



stop()

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
  value = dt_tko[, ..cols],
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
dt_val <- dt_tko[m %% 250 == 0]

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
