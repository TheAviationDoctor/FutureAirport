# ==============================================================================
#    NAME: scripts/9_analyze.R
#   INPUT: 
# ACTIONS: 
#  OUTPUT: 
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load required libraries
library(data.table)
library(DBI)
library(ggplot2)
library(magrittr)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1 Prepare the input data
# ==============================================================================

# ==============================================================================
# 1.1 Import the takeoff performance calibration data
# ==============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db$cnf, group = db$grp)

# Build a query to retrieve the calibration data for the aircraft to simulate
db_qry <- paste("SELECT type, m, cL, cD FROM ",
                tolower(db$cal),
                " WHERE type IN (", paste("'", sim$act_sim, "'", collapse = ", ", sep = ""),
                ");",
                sep = ""
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a data table
dt_cal <- suppressWarnings(
  setDT(
    dbFetch(db_res, n = Inf),
    key = c("type", "m")
  )
)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

# Convert the type column to factor
dt_cal[, type := as.factor(type)]

# Order by type and descending mass
dt_cal <- dt_cal[order(type, -rank(m))]

# Set the minimum mass for which there is a calibrated TODR
dt_cal[, mintom := min(m), by = type]
View(dt_cal)
# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF