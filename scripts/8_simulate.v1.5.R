#===============================================================================
#    NAME: scripts/8_simulate.R
#   INPUT: 442,769,456 rows from database table cli created in 5_transform.R
# ACTIONS: Perform simulated takeoffs and return the takeoff distance required
#          Write the resulting TODR to the database table cli
#  OUTPUT: 442,769,456 rows of database table tko containing takeoff data
#===============================================================================

#===============================================================================
# 0 Housekeeping
#===============================================================================

# Load required libraries
library(data.table)
library(DBI)
library(parallel)

# Import the common settings
source("scripts/0_common.R")

# Import the simulation functions
source("scripts/6_model.v1.4.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

#===============================================================================
# 1 Prepare the simulation
#===============================================================================

#===============================================================================
# 1.1 Set up the database table to store the simulation results
#===============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

# Build the query to drop the table, if it exists
db_qry <- paste("DROP TABLE IF EXISTS ", tolower(db_tko), ";", sep = "")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Build the query to create the table
db_qry <- paste(
  "CREATE TABLE IF NOT EXISTS",
  tolower(db_tko),
  "(id INT UNSIGNED NOT NULL AUTO_INCREMENT,
    obs DATETIME NOT NULL,
    icao CHAR(4) NOT NULL,
    lat FLOAT NOT NULL,
    lon FLOAT NOT NULL,
    exp CHAR(6) NOT NULL,
    type CHAR(4) NOT NULL,
    mtom MEDIUMINT NOT NULL,
    hurs FLOAT NOT NULL,
    ps FLOAT NOT NULL,
    tas FLOAT NOT NULL,
    rho FLOAT NOT NULL,
    hdw FLOAT NOT NULL,
    rwy CHAR(5) NOT NULL,
    toda SMALLINT NOT NULL, 
    m MEDIUMINT NOT NULL,
    rto SMALLINT NOT NULL,
    todr SMALLINT NOT NULL,
    i SMALLINT NOT NULL,
    PRIMARY KEY (id));",
  sep = " ")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

#===============================================================================
# 1.2 Set the simulation variables
#===============================================================================

# Maximum percentage of thrust reduction permissible under FAA AC 25-13 (1988)
reg_rto <- 25L

# Safety margin in percent applied to the horizontal distance along the takeoff
# path assuming all engines operating, from the start of the takeoff to a point
# equidistant between the point at which VLOF is reached and the point at which
# the airplane is 35 ft above the surface, according to 14 CFR § 25.113 (1998).
reg_dis <- 115L

#===============================================================================
# 1.3 Assemble the simulation data
#===============================================================================

#===============================================================================
# 1.3.1 Import the aircraft characteristics (from Sun et al., 2020)
#===============================================================================

dt_act <- fread(
  file = paste(path_aer, aer_act, sep = "/"),
  header = TRUE,
  colClasses = c(rep("factor", 3), rep("integer", 3), rep("numeric", 8)),
  drop = c("name", "eng", "span", "cD0", "k", "lambda_f", "cfc", "SfS"),
  key = "type"
)

#===============================================================================
# 1.3.2 Import the takeoff performance calibration data
#===============================================================================

# Build a query to retrieve the calibration data
db_qry <- paste("SELECT type, m, cl, cd FROM", db_cal, ";", sep = " ")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a data table
dt_cal <- suppressWarnings(
  setDT(
    dbFetch(db_res, n = Inf), key = c("type", "m")
  )
)

# Convert the type column to factor
dt_cal[, type := as.factor(type)]

# Restore proper capitalization given that MySQL stores columns in lowercase
setnames(dt_cal, c("cl", "cd"), c("cL", "cD"))

# Release the database resource
dbClearResult(db_res)

#===============================================================================
# 1.3.3 Import the list of sample airports, excluding those already processed
#===============================================================================

# Build a query to retrieve the sample airports above the traffic threshold
db_qry <- paste(
  "SELECT DISTINCT icao, lat, lon FROM", db_pop,
  "WHERE traffic >", pop_thr, ";",
  sep = " "
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a data table
dt_apt <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "icao"))

# Release the database resource
dbClearResult(db_res)

# Build a query to check if simulation outputs already exists in the database
db_qry <- paste(
  "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = '",
  db_grp,
  "' AND table_name = '", db_tko, "';",
  sep = ""
)

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Return the results to a boolean
tbl_exists <- suppressWarnings(as.logical(dbFetch(db_res, n = Inf)))

# Release the database resource
dbClearResult(db_res)

# If the takeoff table already exists in the database
if (tbl_exists == TRUE) {
  
  # Build a query to retrieve the airports that were already processed
  db_qry <- paste("SELECT DISTINCT icao FROM", db_tko, ";", sep = " ")
  
  # Send the query to the database
  db_res <- dbSendQuery(db_con, db_qry)
  
  # Return the results to a data table of airports to exclude
  dt_exc <- suppressWarnings(
    setDT(
      dbFetch(db_res, n = Inf),
      key = "icao",
      check.names = FALSE
    )
  )
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Remove the airports already processed (inverse left outer join)
  dt_apt <- dt_apt[!dt_exc]
  
} # End if

# Disconnect from the database
dbDisconnect(db_con)

#===============================================================================
# 2 Define a function to simulate takeoffs at each airport
#===============================================================================

fn_simulate <- function(icao) {
  
  # Create a random timer in s proportional to the number of cores
  sleep <- sample(0:cores, 1) * 60
  
  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      "icao", icao,
      "Waiting", sleep, "seconds...", sep = " "
    )
  )
  
  # Stagger the start of each core to avoid a database I/O bottleneck
  Sys.sleep(sleep)
  
  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      "icao", icao,
      "Loading simulation data...",
      sep = " "
    )
  )
  
  #=============================================================================
  # 2.1 Import the climatic observations for the current airport
  #=============================================================================

  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Build a query to retrieve the takeoff conditions
  db_qry <- paste(
    "SELECT obs, exp, icao, hurs, ps, tas, rho, hdw, rwy, toda FROM ", db_cli,
    " WHERE icao = '", icao, "';",
    sep = ""
  )
  
  # Send the query to the database
  db_res <- suppressWarnings(dbSendQuery(db_con, db_qry))
  
  # Return the results to a data table
  dt_cli <- suppressWarnings(setDT(dbFetch(db_res, n = Inf), key = "toda"))
  
  # Convert several columns to their correct class
  dt_cli[, obs  := as.POSIXct(obs)]
  dt_cli[, exp  := as.factor(exp)]
  dt_cli[, icao := as.factor(icao)]
  dt_cli[, rwy  := as.factor(rwy)]
  
  # Release the database resource
  dbClearResult(db_res)
  
  # Disconnect the worker from the database
  dbDisconnect(db_con)
  
  #=============================================================================
  # 2.2 Combine climatic observations with aircraft and calibration data
  #=============================================================================
  
  # Combine climatic observations with aircraft data (Cartesian product)
  dt_tko <- dt_cli[, as.list(dt_act), by = dt_cli]
  
  # Convert airport code to a factor
  dt_tko[, icao := as.factor(icao)]
  
  # Initialize the starting mass in kg to the maximum takeoff mass
  dt_tko[, m := mtom]
  
  # Initialize the starting takeoff thrust reduction to its simulation value
  # Add 1 as the repeat loop starts by decrementing rto by 1
  dt_tko[, rto := reg_rto + 1L]
  
  # Initialize the starting TODR to an arbitrary value greater than the max TODA
  # This is so that the repeat loop captures all observations in its first pass
  dt_tko[, todr := max(toda) + 1L]
  
  # Initialize a counter to track the number of iterations of each takeoff
  dt_tko[, i := 0L]
  
  # Combine climatic observations with calibration data (cL and cD)
  dt_tko <- dt_cal[dt_tko, on = c("type", "m")]
  
  # Add the airport's latitude and longitude to the table for later analysis
  dt_tko <- dt_apt[dt_tko, on = c("icao")]
  
  #=============================================================================
  # 2.3 Perform vectorized takeoff simulations iteratively until TODR < TODA
  #=============================================================================
  
  repeat {
    
    #===========================================================================
    # 2.3.1 Prepare the data
    #===========================================================================
    
    # Count how many observations have a TODR greater than TODA
    obs <- dim(dt_tko[todr > toda])[1]
    
    # For those observations only
    if (obs > 0) {
      
      # Increment the iteration counter
      dt_tko[todr > toda, i := i + 1L]
      
      # Inform the log file
      print(
        paste(
          Sys.time(),
          "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
          "icao", icao,
          "Starting iteration #", mean(dt_tko[todr > toda, i]),
          sep = " "
        )
      )
      
      # If thrust is already at TOGA, then decrease the mass by 1 kg instead
      dt_tko[todr > toda & rto == 0, m   := m   - 1L]
      
      # Otherwise increase thrust by 1 percentage point (up to TOGA)
      dt_tko[todr > toda & rto >  0, rto := rto - 1L]
      
      # Remove the existing cL and cD values
      dt_tko[todr > toda, c("cD", "cL") := NA]
      
      # Add the calibration data (cD and cL) again for the new mass
      dt_tko[dt_cal, cD := ifelse(is.na(cD), i.cD, cD), on = c("type", "m")]
      dt_tko[dt_cal, cL := ifelse(is.na(cL), i.cL, cL), on = c("type", "m")]

      #=========================================================================
      # 2.3.2 Calculate the weight force W in N
      #=========================================================================
      
      # Inform the log file
      print(
        paste(
          Sys.time(),
          "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
          "icao", icao,
          "i =", str_pad(
            mean(dt_tko[todr > toda, i]),
            width = 3, side = "left", pad = "0"),
          "t/o =", format(obs, big.mark = ","),
          "rto =", scales::percent(mean(dt_tko[todr > toda, rto]) / 100),
          "Calculating weight",
          sep = " "
        )
      )
      
      # Calculate the weight force
      dt_tko[todr > toda, W := sim$g * m]
      
      #=========================================================================
      # 2.3.3 Calculate the speed in m/s at which lift equals weight
      # Adapted from Blake (2009).
      #=========================================================================
      
      # Inform the log file
      print(
        paste(
          Sys.time(),
          "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
          "icao", icao,
          "i =", str_pad(
            mean(dt_tko[todr > toda, i]),
            width = 3, side = "left", pad = "0"),
          "t/o =", format(obs, big.mark = ","),
          "rto =", scales::percent(mean(dt_tko[todr > toda, rto]) / 100),
          "Calculating speeds",
          sep = " "
        )
      )
      
      # Calculate the speed at which lift equals weight
      dt_tko[todr > toda, Vlof := sqrt(W / (.5 * rho * S * cL))]
      
      # Create airspeed intervals up to the minimum takeoff airspeed
      dt_tko[todr > toda, Vtas :=
               Map(seq, from = hdw, to = Vlof, length.out = sim$int)]
      
      # Create groundspeed intervals up to the minimum takeoff airspeed
      dt_tko[todr > toda, Vgnd :=
               Map(seq, from = 0, to = Vlof - hdw, length.out = sim$int)]
      
      #=========================================================================
      # 2.3.4 Calculate the dynamic pressure q in Pa
      # Adapted from Blake (2009).
      #=========================================================================
      
      # Inform the log file
      print(
        paste(
          Sys.time(),
          "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
          "icao", icao,
          "i =", str_pad(
            mean(dt_tko[todr > toda, i]),
            width = 3, side = "left", pad = "0"),
          "t/o =", format(obs, big.mark = ","),
          "rto =", scales::percent(mean(dt_tko[todr > toda, rto]) / 100),
          "Calculating dynamic pressure",
          sep = " "
        )
      )
      
      # Calculate the dynamic pressure
      dt_tko[todr > toda, q := Map("*", Vtas, (.5 * rho))]
      
      #=========================================================================
      # 2.3.5 Calculate the propulsive force F in N
      # Adapted from Sun et al. (2020).
      #=========================================================================
      
      # Inform the log file
      print(
        paste(
          Sys.time(),
          "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
          "icao", icao,
          "i =", str_pad(
            mean(dt_tko[todr > toda, i]),
            width = 3, side = "left", pad = "0"),
          "t/o =", format(obs, big.mark = ","),
          "rto =", scales::percent(mean(dt_tko[todr > toda, rto]) / 100),
          "Calculating thrust",
          sep = " "
        )
      )
      
      # Calculate the speed of sound in m/s for the given temperature in dry air
      dt_tko[todr > toda, Vsnd := sqrt(sim$gamma * sim$Rd * tas)]
      
      # Calculate the Mach number for each airspeed interval
      dt_tko[todr > toda, Vmach := Map("/", Vtas, Vsnd)]
      
      # Calculate the air pressure ratio
      dt_tko[todr > toda, dP := ps / sim$ps_isa]
      
      # Calculate the coefficients of thrust
      dt_tko[todr > toda, G0 :=  .0606 * bpr  +  .6337]
      dt_tko[todr > toda, A  := -.4327 * dP^2 + 1.3855 * dP   +  .0472]
      dt_tko[todr > toda, Z  :=  .9106 * dP^3 - 1.7736 * dP^2 + 1.8697 * dP]
      dt_tko[todr > toda, X  :=  .1377 * dP^3 -  .4374 * dP^2 + 1.3003 * dP]
      
      # Calculate the thrust ratio
      dt_tko[todr > toda, tr := Map(
        function(A, bpr, G0, Z, X, Vmach) {
          A - 0.377 * (1 + bpr) / sqrt((1 + 0.82 * bpr) * G0) * Z * Vmach +
            (0.23 + 0.19 * sqrt(bpr)) * X * Vmach^2
        },
        A = A, bpr = bpr, G0 = G0, Z = Z, X = X, Vmach = Vmach
      )]
      
      # Calculate the maximum takeoff thrust in N for each Mach number
      dt_tko[todr > toda, Fmax := Map("*", tr, slst * n)]
      
      # Apply the maximum takeoff thrust reduction permissible
      dt_tko[todr > toda, Frto := Map("*", Fmax, (100 - rto) / 100)]
      
      #=========================================================================
      # 2.3.6 Calculate the acceleration in m/s² up to liftoff
      # Adapted from Blake (2009).
      #=========================================================================
      
      # Inform the log file
      print(
        paste(
          Sys.time(),
          "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
          "icao", icao,
          "i =", str_pad(
            mean(dt_tko[todr > toda, i]),
            width = 3, side = "left", pad = "0"),
          "t/o =", format(obs, big.mark = ","),
          "rto =", scales::percent(mean(dt_tko[todr > toda, rto]) / 100),
          "Calculating acceleration",
          sep = " "
        )
      )
      
      # Calculate the acceleration
      dt_tko[todr > toda, a := Map(
        function(W, Frto, cD, cL, q, S) {
          sim$g / W * (Frto * 2 - (sim$mu * W) -
          (cD - (sim$mu * cL)) * (q * S) - (W * sin(sim$theta)))
        },
        W = W, Frto = Frto, cD = cD, cL = cL, q = q, S = S
      )]
      
      #=========================================================================
      # 2.3.7 Calculate the horizontal takeoff distances in m
      # Adapted from Blake (2009).
      #=========================================================================
      
      # Inform the log file
      print(
        paste(
          Sys.time(),
          "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
          "icao", icao,
          "i =", str_pad(
            mean(dt_tko[todr > toda, i]),
            width = 3, side = "left", pad = "0"),
          "t/o =", format(obs, big.mark = ","),
          "rto =", scales::percent(mean(dt_tko[todr > toda, rto]) / 100),
          "Calculating takeoff distance",
          sep = " "
        )
      )
      
      # Calculate the average acceleration between two groundspeed increments
      dt_tko[todr > toda, a_avg :=
               frollmean(x = a, n = 2, fill = a[[1]][1], align = "right")]
      
      # Calculate the average groundspeed between two groundspeed increments
      dt_tko[todr > toda, Vgnd_avg :=
               frollmean(x = Vgnd, n = 2, fill = 0, align = "right")]
      
      # Extract the size of the groundspeed interval
      dt_tko[todr > toda, Vgnd_inc :=
               sapply(dt_tko[todr > toda, Vgnd], "[[", 2)]
      
      # Calculate the distance in meters covered within each speed increment
      dt_tko[todr > toda, inc := Map("/", Map("*", Vgnd_avg, Vgnd_inc), a_avg)]
      
      # Increment the cumulative (running total) distance accordingly
      dt_tko[todr > toda, cum := Map(cumsum, inc)]
      
      # Add airborne distance from Vlof to screen height (Gratton et al, 2020)
      # and add regulatory safety margin of 15% as per 14 CFR § 25.113 (1998)
      # applied to the takeoff run distance, which is from brake release to Vlof
      # + up to the middle point between Vlof and reaching screen height.
      dt_tko[todr > toda, todr := unlist(Map(
        function(cum) {
          ceiling((max(cum) + .3048 * 35 / cos(7.7) / 2) * (reg_dis / 100))
        },
        cum = cum
      ))]
      
    # Once there are no more observations where TODR > TODA
    } else {
      
      # End the repeat loop
      break
      
    } # End if-else
    
  } # End repeat
  
  #=============================================================================
  # 2.4 Write results to the database
  #=============================================================================
  
  # Select which columns to write to the database and in which order
  cols <- c("obs", "icao", "lat", "lon", "exp", "type", "mtom", "hurs", "ps",
            "tas", "rho", "hdw", "rwy", "toda", "m", "rto =", "todr", "i =")
  
  # FOR TESTING ONLY
  print(str(dt_tko[, ..cols]))
  
  # Connect the worker to the database
  db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)
  
  # Write the data. Here we use the deprecated RMySQL::MySQL() driver instead of
  # the newer RMariaDB::MariaDB()) driver because it was found to be much faster
  dbWriteTable(conn = db_con, name = tolower(db_tko), value = dt_tko[, ..cols],
               append = TRUE, row.names = FALSE)
  
  # Disconnect the worker from the database
  dbDisconnect(db_con)
  
  # Inform the log file
  print(
    paste(
      Sys.time(),
      "pid", str_pad(Sys.getpid(), width = 5, side = "left", pad = " "),
      "icao", icao,
      "Wrote", nrow(dt_tko), "rows to the database",
      sep = " "
    )
  )
  
} # End of the fn_simulate function

#===============================================================================
# 3. Handle the parallel computation across multiple cores
#===============================================================================
  
# Set the number of workers to use in the cluster
cores <- 12

# Set the log file for the cluster
outfile <- "logs/8_simulate.log"

# Clear the log file
close(file(outfile, open = "w"))

# Build the cluster of workers
cl <- makeCluster(cores, outfile = outfile)

# Have each worker load the libraries that they need
clusterEvalQ(cl, {
  library(data.table)
  library(DBI)
  library(stringr)
})

# Pass the required variables from the main scope to the workers' scope
clusterExport(cl, c("cores", "db_cnf", "db_cli", "db_grp", "db_tko", "dt_act",
                    "dt_apt", "dt_cal", "fn_todr", "reg_dis", "reg_rto", "sim"))

# Distribute the unique airports across the workers
parLapply(cl, as.list(dt_apt[, icao]), fn_simulate)

# Terminate the cluster once finished
stopCluster(cl)
  
#===============================================================================
# 4. Index the database table
#===============================================================================

# Connect to the database
db_con <- dbConnect(RMySQL::MySQL(), default.file = db_cnf, group = db_grp)

# Set the index name
db_idx <- "idx"

# Build the query to create the index
db_qry <- paste("CREATE INDEX ", tolower(db_idx),
                " ON ", tolower(db_tko), " (exp, icao);", sep = "")

# Send the query to the database
db_res <- dbSendQuery(db_con, db_qry)

# Release the database resource
dbClearResult(db_res)

# Disconnect from the database
dbDisconnect(db_con)

#===============================================================================
# 5. Housekeeping
#===============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF