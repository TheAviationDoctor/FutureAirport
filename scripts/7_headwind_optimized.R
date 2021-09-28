################################################################################
# scripts/7_headwind_optimized.R                                               #
# Calculates headwind speed and identifies active runway for each observation  #
#  Took ~X hours to run on the researcher's config (https://bit.ly/3ChCBAP)    #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(DBI)
library(parallel)

# Import the constants
source("scripts/0_constants.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

################################################################################
# Create a temporary table to hold the runways in the sample                   #
################################################################################

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

# # Build a query to create a temporary table with only airports above the passenger traffic sampling threshold
# # The query also replaces each runway's name (e.g., RW26R) with its magnetic heading in degrees (e.g., 260) for later headwind calculation
# # Lastly, for two runways with the same magnetic heading at a given airport (e.g., RWY26R and RWY26L), the query returns only the one with the longest TODA (i.e., the most favorable case)
# db_qry <- paste("CREATE TEMPORARY TABLE sample SELECT icao, SUBSTRING(rwy, 3, 2) AS hdg, MAX(toda) FROM", db_pop, "WHERE traffic >", pop_thr, "GROUP BY icao, hdg;", sep = " ")
# 
# # Release the database resource
# dbClearResult(db_res)

# Set the name of the table where the eastward wind data are stored for the current experiment (SSP)
db_uas <- paste("uas_", tolower(nc_exps), sep = "")
db_uas

# Set the name of the table where the northward wind data are stored for the current experiment (SSP)
db_vas <- paste("vas_", tolower(nc_exps), sep = "")
db_vas

# Build a query to create a temporary table for wind component data for every airport/observation pair
db_qry <- paste("CREATE TABLE wnd ", paste(paste("SELECT uas.obs, uas.icao, '", nc_exps, "' AS exp, uas.val AS uas, vas.val AS vas FROM ", db_uas, " AS uas, ", db_vas, " AS vas WHERE uas.id = vas.id", sep = ""), collapse = " UNION ALL "), ";", sep = "")

# FOR TESTING ONLY
print(db_qry)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Build a query to count the rows in the temporary table
db_qry <- "SELECT COUNT(*) FROM wnd;"

# FOR TESTING ONLY
print(db_qry)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results
dt_cnt <- dbFetch(db_res, n = Inf)

# Release the database resource
dbClearResult(db_res)

# Show the count
dt_cnt

# Disconnect from the database
dbDisconnect(db_con)