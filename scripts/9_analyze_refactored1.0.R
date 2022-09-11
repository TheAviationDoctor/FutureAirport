# ==============================================================================
#    NAME: scripts/9_analyze_refactored.R
#   INPUT: Climate and takeoff performance data output by earlier scripts
# ACTIONS: Create summary tables in MySQL and associated plots
#  OUTPUT: Plot files saved to disk
# RUNTIME: N/A
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2022
# ==============================================================================

# ==============================================================================
# NOTE: Some of the SQL queries below are memory-intensive. If you encounter
# the MySQL error "The total number of locks exceeds the lock table size",
# log as admin into MySQL Workbench and increase the InnoDB buffer size to
# a value of X where X is how much RAM in GB you can dedicate to the process:
# 'SET GLOBAL innodb_buffer_pool_size = X * 1024 * 1024 * 1024;'
# ==============================================================================

# ==============================================================================
# 0 Housekeeping
# ==============================================================================

# Load the required libraries
library(data.table)
library(DBI)
library(ggplot2)
library(scales)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1. Climate change summary
# ==============================================================================

# Create the summary table (runtime: ~X minutes)
#  avg_tas: mean near-surface air temperature
#  avg_rho: mean near-surface air density
#  avg_hdw: mean near-surface headwind
#  max_tas: maximum near-surface air temperature
#  min_rho: minimum near-surface air density
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
      tolower(dat$an1),
    "(
      year YEAR,
      exp CHAR(6),
      zone CHAR(11),
      type CHAR(4),
      cnt INT,
      AVG(tas) FLOAT,
      AVG(rho) FLOAT,
      AVG(hdw) FLOAT,
      MAX(tas) FLOAT,
      MIN(rho) FLOAT
    )
    AS SELECT
      year AS year,
      exp AS exp,
      zone AS zone,
      icao AS icao,
      COUNT(*) AS cnt,
      AVG(tas) AS avg_tas,
      AVG(rho) AS avg_rho,
      AVG(hdw) AS avg_hdw,
      MAX(tas) AS max_tas,
      MIN(rho) AS min_rho
    FROM",
      tolower(dat$cli),
    "GROUP BY
      year,
      exp,
      zone,
      icao
    ;",
    sep = " "
  )
)

# Fetch the data
dt_an1 <- fn_sql_qry(
  statement = paste(
    "SELECT
      *
    FROM",
      tolower(dat$an1),
    ";",
    sep = " "
  )
)

# Recast column types
set(x = dt_an1, j = "year", value = as.integer(dt_an1[, year]))
set(x = dt_an1, j = "exp",  value = as.factor(dt_an1[, exp]))
set(x = dt_an1, j = "type", value = as.factor(dt_an1[, type]))
set(x = dt_an1, j = "zone", value = as.factor(dt_an1[, zone]))
set(x = dt_an1, j = "icao", value = as.factor(dt_an1[, icao]))

# Define the variables of interest
cols_an1 <- c(
  "avg_tas",
  "avg_rho",
  "avg_hdw",
  "max_tas",
  "min_rho"
)

# Define their labels
labs_an1 <- c(
  "Mean Near-Surface Air Temperature in °C",
  "Mean Near-Surface Air Density in kg/m³",
  "Mean Near-Surface Headwind Speed in m/s",
  "Mean Maximum Near-Surface Temperature in °C",
  "Minimum Near-Surface Air Density in kg/m³"
)

# Replace data with local polynomial regression fitting to dampen the volatility
dt_an1[, cols_an1 :=
  lapply(X = .SD, FUN = function(x)
    predict(loess(formula = x ~ year, span = .75, model = TRUE))
  ),
  by = c("exp", "zone"),
  .SDcols = cols_an1
]

# ==============================================================================
# 2. Takeoff simulation summary
# ==============================================================================

# Create the summary table (runtime: ~X minutes)
#  tko_cnt: number of takeoffs
#  itr_sum: number of takeoff iterations
#  tko_ok_thr_min refers to a successful takeoff with thrust at 75% TOGA
#  tko_ok_thr_mid refers to a successful takeoff with thrust at 76%-99% TOGA
#  tko_ok_thr_max refers to a successful takeoff with thrust at 100% TOGA
#  tko_ok refers to a successful takeoff (a + b + c)
#  tko_ko refers to an unsuccessful takeoff
#  tko refers to any takeoff (d + e)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
      tolower(dat$an2),
    "(
      year YEAR,
      exp CHAR(6),
      zone CHAR(11),
      type CHAR(4),
      tko_cnt INT,
      itr_sum BIGINT,
      tko_ok_thr_min MEDIUMINT,
      tko_ok_thr_mid MEDIUMINT,
      tko_ok_thr_max MEDIUMINT,
      tko_ok MEDIUMINT,
      tko_ko MEDIUMINT,
      tko MEDIUMINT,
      todr_avg FLOAT
    )
    AS SELECT
      year,
      exp,
      zone,
      type,
      COUNT(*) AS tko_cnt,
      SUM(itr) AS itr_sum,
      SUM(thr_red =", sim$thr_ini, ") AS tko_ok_thr_min,
      SUM(thr_red BETWEEN 1 AND ", sim$thr_ini - 1L, ") AS tko_ok_thr_mid,
      SUM(thr_red = 0 AND todr <= toda) AS tko_ok_thr_max,
      SUM(todr <= toda) AS tko_ok,
      SUM(todr > toda) AS tko_ko,
      COUNT(*) AS tko,
      AVG(todr) AS todr_avg
    FROM",
      tolower(dat$tko),
    "GROUP BY 
      year,
      exp,
      zone,
      type
    ;",
    sep = " "
  )
)

# Fetch the data
dt_an2 <- fn_sql_qry(
  statement = paste(
    "SELECT
      *
    FROM",
    tolower(dat$an2),
    ";",
    sep = " "
  )
)

# Recast column types
set(x = dt_an2, j = "year", value = as.integer(dt_an2[, year]))
set(x = dt_an2, j = "exp",  value = as.factor(dt_an2[, exp]))
set(x = dt_an2, j = "type", value = as.factor(dt_an2[, type]))
set(x = dt_an2, j = "zone", value = as.factor(dt_an2[, zone]))

# Combine aircraft types
levels(dt_an2$type) <- c(
  "A20n" = "Narrowbody",
  "B39m" = "Narrowbody",
  "A359" = "Widebody",
  "B789" = "Widebody"
)

# Define the variables of interest
cols_an2 <- c(
  "tko_cnt",
  "itr_sum",
  "tko_ok_thr_min",
  "tko_ok_thr_mid",
  "tko_ok_thr_max",
  "tko_ok",
  "tko_ko",
  "tko",
  "todr_avg"
)

# Define their labels
labs_an2 <- c(
  "Takeoff count",
  "Iterations count",
  "75% TOGA",
  "76%-99% TOGA",
  "TOGA",
  "Succesful takeoff",
  "Unsuccessful takeoff",
  "All takeoffs",
  "Mean TODR"
)

# Summarize data by aircraft type
dt_an2 <- dt_an2[, lapply(.SD, mean),
                 by = c("year", "exp", "zone", "type"),
                 .SDcols = c("todr_avg")
]














































