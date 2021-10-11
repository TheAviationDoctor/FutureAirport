################################################################################
#    NAME: scripts/6_takeoff.R                                                 #
#   INPUT: 442,769,456 rows from database table nc created in 5_transform.R    #
# ACTIONS: Pivot the database table imp from long to wide/tidy                 #
#          Calculate the takeoff distance required (TODR)                      #
#          Write the resulting TODR to the database table nc                   #
#  OUTPUT: 442,769,456 rows of database table nc updated with one new column   #
# RUNTIME: ~X hours on the researcher's config (https://bit.ly/3ChCBAP)      #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(DBI)
library(parallel)

# Import the constants
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

################################################################################
# Import the sample airports and runways                                       #
################################################################################

# Build the query to retrieve the sample airports
db_qry <- paste("SELECT * FROM", db_nc, "WHERE id < 11;", sep = " ")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results
dt_smp <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "id", check.names = FALSE))

# Release the database resource
dbClearResult(db_res)

# Count the resulting number of runways
nrow(dt_smp)

View(dt_smp)

################################################################################
# Step integration of distance as per Table 18-2 of Blake (2019)               #
################################################################################

# Initialize the cumulative takeoff distance
cum <- 0

# Initialize an array to display the results later
out1 <- array(
  dim = c(0, 5),
  dimnames = list(NULL, c("GS - knots", "TAS - knots", "accel ft/sec/sec", "s - feet", "Σs - feet"))
)

# For each runway
for (i in 1:nrow(dt_smp)) {
  
  ################################################################################
  # Variables definition - Speeds                                                #
  ################################################################################
  
  # Groundspeed increments (in knots) at which the acceleration is calculated
  GS <- seq(from = 0, to = 160, by = 20)
  
  # True airspeed (groundspeed + headwind)
  TAS <- GS + dt_smp$wnd_hdw[i]
  
  dt_smp$wnd_hdw[i]
    
  ################################################################################
  # Acceleration calculation                                                     #
  ################################################################################
  
  # Dynamic pressure. The speed is converted to feet per second
  q <- (rho * (TAS * 1.68781)^2) / 2
  
  # Acceleration in feet per second per second
  a <- g / W * (T * 2 - (mu * W) - (CD - (mu * CL)) * (q * S) - (W * sin(theta)))
  
  # For each groundspeed increment
  for (j in 1:length(GS)) {
    
    # Calculate the average speed (i.e., midpoint speed), skipping the zero speed row
    avg <- ifelse(GS[j] == 0, GS[j], mean(c(GS[j - 1], GS[i])))
  
    # Calculate the speed increment (i.e., new speed minus previous speed), skipping the zero speed row
    inc <- ifelse(GS[j] == 0, GS[j], GS[j] - GS[j - 1])
    
    # Calculate the average acceleration (i.e., midpoint acceleration)
    acc <- mean(c(a[j - 1], a[j]))
    
    # Calculate the distance covered in that increment (converting speed from knots to feet/s)
    dis <- avg * inc * 1.6878^2 / acc
  
    # Increment the cumulative (total) distance by that incremental distance
    cum <- cum + dis
    
    # Save incremental results for later display to the console
    out1 <- rbind(out1, c(GS[j], TAS[j], round(a[j], digits = 1), round(dis, digits = 0), round(cum, digits = 0)))
  }

}

# Output results to the console
out1

################################################################################
# Housekeeping                                                                 #
################################################################################

# Disconnect from the database
dbDisconnect(db_con)

# Display the script execution time
Sys.time() - start_time

# EOF