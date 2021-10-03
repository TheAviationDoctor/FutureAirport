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

# Build the query to create a temporary table with only airports above the passenger traffic sampling threshold
# The query also replaces each runway's name (e.g., RW26R) with its magnetic heading in degrees (e.g., 260) for later headwind calculation
# Lastly, for two runways with the same magnetic heading at a given airport (e.g., RWY26R and RWY26L), the query returns only the one with the longest TODA (i.e., the most favorable case)
db_qry <- paste("CREATE TEMPORARY TABLE rwys SELECT icao, SUBSTRING(rwy, 3, 2) AS hdg, MAX(toda) FROM", db_pop, "WHERE traffic >", pop_thr, "GROUP BY icao, hdg;", sep = " ")

# Release the database resource
dbClearResult(db_res)

##############################################################################
# Define a function to return the headwind speed for a given runway          #
##############################################################################

fn_headwind <- function(rwy) {
  
  # Calculate the angle (in degrees) between the runway heading and the direction that the wind is coming from
  alpha <- abs(rwy - wnd_dir)
  
  # Calculate and return the headwind speed (in m/s)
  wnd_spd * cos(alpha * pi / 180)
  
}

# Extract the list of unique runways for this observation's airport
rwys <- dt_smp[icao == dt_wnd[i, icao], rwy]

# Calculate the headwind speed for each runway at this observation's airport
l <- lapply(rwys, fn_headwind)

# Return the maximum headwind speed across all runways at this observation's airport
val <- max(unlist(l))

# Return the runway that has the maximum headwind speed (presumed to be the active runway at the time of the observation)
rwy <- rwys[which.max(unlist(l))]

# Add the headwind and active runway to the data table
dt_wnd[i, val := val]
dt_wnd[i, rwy := rwy]




















# Disconnect from the database
dbDisconnect(db_con)