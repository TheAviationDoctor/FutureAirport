# ==============================================================================
#    NAME: scripts/9_analyze.R
#   INPUT: Climate and takeoff performance data processed by earlier scripts
# ACTIONS: Create summary tables in MySQL and plots based on them
#  OUTPUT: Plot files saved to disk
# RUNTIME: N/A
#  AUTHOR: Thomas D. Pellegrin <thomas@pellegr.in>
#    YEAR: 2022
# ==============================================================================

# ==============================================================================
#    NOTE: Some of the SQL queries below are memory-intensive. If you encounter
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
library(magrittr)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1. Research question #1: How much change to near-surface air temperature,
# air density, and headwind will airports experience in the 21st century?
# ==============================================================================

# ==============================================================================
# 1.1 Climate change globally by SSP
# ==============================================================================

# Summarize the climate data (runtime: ~12 minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
    tolower(tmp$q11),
    "AS SELECT
    exp AS exp,
    year AS year,
    AVG(tas) AS avg_tas,
    MAX(tas) AS max_tas,
    AVG(rho) AS avg_rho,
    MIN(rho) AS min_rho,
    AVG(hdw) AS avg_hdw
    FROM", tolower(dat$cli),
    "GROUP BY exp, year;",
    sep = " "
  )
)

# Fetch the data
dt_q11 <- fn_sql_qry(
  statement = paste(
    "SELECT * FROM",
    tolower(tmp$q11),
    "ORDER BY exp ASC, year ASC;",
    sep = " "
  )
)

# Convert the shared socioeconomic pathway (SSP) to a factor
set(x = dt_q11, j = "exp",  value = as.factor(dt_q11[, exp]))

# Convert the year to an interval variable for continuous scaling
set(x = dt_q11, j = "year", value = as.integer(dt_q11[, year]))

# Count the observations by SSP
obs <- dt_q11[, .N, by = exp]

# Define the variables of interest
cols <- c("avg_tas", "max_tas", "avg_rho", "min_rho", "avg_hdw")

# Define their labels
labs <- c(
  "Relative change in the mean temperature in °C",
  "Relative change in the max. temperature in °C",
  "Relative change in the mean air density in kg/m3",
  "Relative change in the min. air density in kg/m3",
  "Relative change in the mean headwind speed in m/s"
)

# Build a data table of the first year values by SSP for each variable
ini <- dt_q11[, .SD[1:1], by = exp][rep(1:.N, times = obs$N)]

# Subtract the starting value (first year) from each variable and observation
dt_q11[, (cols) := dt_q11[, cols, with = FALSE] - ini[, cols, with = FALSE]]

# Save the data for offline analysis
fwrite(x = dt_q11, file = paste(dir$res, "dt_q11.csv", sep = "/"))

# Define a function to generate plots
fn_plot <- function(var1, var2) {
  (
    ggplot(
      data    = dt_q11,
      mapping = aes(x = year, y = dt_q11[[var1]])
    ) +
      geom_line(size = .2) +
      geom_smooth(formula = y ~ x, method = "loess", size = .5) +
      scale_x_continuous("Year") +
      scale_y_continuous(var2) +
      facet_wrap(~toupper(exp), ncol = 2) +
      # guides(color = "none") +
      theme_light()
  ) |>
    ggsave(
      filename = paste("9q11_", as.name(var1), ".png", sep = ""),
      device   = "png",
      path     = "plots",
      scale    = 1,
      width    = 6,
      height   = NA,
      units    = "in",
      dpi      = "print"
    )
}

# Generate the plots for every variable
mapply(
  FUN  = fn_plot,
  var1 = cols,
  var2 = labs
)

# ==============================================================================
# 1.2 Climate change by zone and SSP
# ==============================================================================

# Summarize the climate data (runtime: ~12 minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
    tolower(tmp$q12),
    "AS SELECT
    zone AS zone,
    exp AS exp,
    year AS year,
    AVG(tas) AS avg_tas,
    MAX(tas) AS max_tas,
    AVG(rho) AS avg_rho,
    MIN(rho) AS min_rho,
    AVG(hdw) AS avg_hdw
    FROM", tolower(dat$cli),
    "GROUP BY zone, exp, year;",
    sep = " "
  )
)

# Fetch the data
dt_q12 <- fn_sql_qry(
  statement = paste(
    "SELECT * FROM",
    tolower(tmp$q12),
    "ORDER BY zone ASC, exp ASC, year ASC;",
    sep = " "
  )
)

# Convert the climatic zone to a factor
set(x = dt_q12, j = "zone",  value = as.factor(dt_q12[, zone]))

# Convert the SSP to a factor
set(x = dt_q12, j = "exp",  value = as.factor(dt_q12[, exp]))

# Convert the year to an interval variable for continuous scaling
set(x = dt_q12, j = "year", value = as.integer(dt_q12[, year]))

# Count the observations by zone and SSP
obs <- dt_q12[, .N, by = c("zone", "exp")]

# Build a data table of the first year values by zone and SSP for each variable
ini <- dt_q12[, .SD[1:1], by = c("zone", "exp")][rep(1:.N, times = obs$N)]

# Subtract the starting value (first year) from each variable and observation
dt_q12[, (cols) := dt_q12[, cols, with = FALSE] - ini[, cols, with = FALSE]]

# Save the data for offline analysis
fwrite(x = dt_q12, file = paste(dir$res, "dt_q12.csv", sep = "/"))

# Define a function to generate plots
fn_plot <- function(var1, var2) {
  (
    ggplot(
      data    = dt_q12,
      # mapping = aes(x = year, y = dt_q12[[var1]], color = toupper(exp))
      mapping = aes(x = year, y = dt_q12[[var1]], color = zone)
    ) +
      geom_line(size = .2) +
      geom_smooth(formula = y ~ x, method = "loess", size = .5) +
      scale_x_continuous("Year") +
      scale_y_continuous(var2) +
      # facet_wrap(~zone, ncol = 2) +
      facet_wrap(~toupper(exp), ncol = 2) +
      # guides(color = "none") +
      theme_light()
  ) |>
  ggsave(
    filename = paste("9q12_", as.name(var1), ".png", sep = ""),
    device   = "png",
    path     = "plots",
    scale    = 1,
    width    = 6,
    height   = NA,
    units    = "in",
    dpi      = "print"
  )
}

# Generate the plots for every variable
mapply(
  FUN  = fn_plot,
  var1 = cols,
  var2 = labs
)

# ==============================================================================
# 1.1 Climate change by climatic zone across SSPs
# ==============================================================================

# # Create a list of sample airports by climate zone
# dt_out <- fn_sql_qry(
#   statement = paste(
#     "SELECT DISTINCT(icao) AS icao, ABS(lat) AS lat,",
#     "(CASE",
#       "WHEN ABS(lat) >", lat$tro$lower, "AND ABS(lat) <", lat$tro$upper, "THEN", lat$tro$name,
#       "WHEN ABS(lat) >", lat$sub$lower, "AND ABS(lat) <", lat$sub$upper, "THEN", lat$sub$name,
#       "WHEN ABS(lat) >", lat$tem$lower, "AND ABS(lat) <", lat$tem$upper, "THEN", lat$tem$name,
#       "WHEN ABS(lat) >", lat$fri$lower, "AND ABS(lat) <", lat$fri$upper, "THEN", lat$fri$name,
#     "END) AS zone",
#     "FROM",
#     tolower(dat$pop),
#     "WHERE traffic >",
#     sim$pop_thr,
#     "ORDER BY ABS(lat) DESC;",
#     sep = " "
#   )
# )

# Create a list of sample airports by climate zone
# dt_out <- fn_sql_qry(
#   statement = paste(
#     "SELECT DISTINCT(icao) AS icao,",
#     "(CASE",
#       "WHEN ABS(lat) >", lat$tro$lower, "AND ABS(lat) <", lat$tro$upper, "THEN", lat$tro$name,
#       "WHEN ABS(lat) >", lat$sub$lower, "AND ABS(lat) <", lat$sub$upper, "THEN", lat$sub$name,
#       "WHEN ABS(lat) >", lat$tem$lower, "AND ABS(lat) <", lat$tem$upper, "THEN", lat$tem$name,
#       "WHEN ABS(lat) >", lat$fri$lower, "AND ABS(lat) <", lat$fri$upper, "THEN", lat$fri$name,
#     "END) AS zone",
#     "FROM",
#     tolower(dat$pop),
#     "WHERE traffic >",
#     sim$pop_thr,
#     "ORDER BY ABS(lat) DESC;",
#     sep = " "
#   )
# )
# 
# 
# View(dt_out)



# ==============================================================================
# 2. Research question #2: How much thrust increase and payload removal
# will be needed?
# ==============================================================================

# ==============================================================================
# 2.1 Unique takeoffs
# Takes 4.7 minutes
# ==============================================================================

# ==============================================================================
# 1.1 Summarize the climate data in the data in the database
# Runtime: ~21 minutes
# ==============================================================================

# # Create the visualization table
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#     tolower(vis$tko_cnt),
#     "AS SELECT
#     COUNT(*) AS count,
#     SUM(itr) AS sum,
#     AVG(itr) AS avg
#     FROM", tolower(dat$tko),
#     ";",
#     sep = " "
#   )
# )



# # Fetch the data
# dt_uni <- fn_sql_sel(
#   select = "COUNT(*) AS count",
#   from   = dat$tko,
#   where  = NULL,
#   group  = NULL,
#   order  = NULL
# )
# 
# # Output results to the console
# print(paste("Count of unique takeoffs:", format(dt_uni, big.mark = ",")))

# ==============================================================================
# 2.2 Takeoff iterations
# ==============================================================================

# # Fetch the data
# dt_itr <- fn_sql_sel(
#   select = "SUM(itr) AS sum, AVG(itr) AS avg",
#   from   = dat$tst,
#   where  = NULL,
#   group  = NULL,
#   order  = NULL
# )
# 
# # Output results to the console
# print(paste("Count of iterations:", format(dt_itr[, sum], big.mark = ",")))
# print(paste("Average iterations:", format(dt_itr[, avg], big.mark = ",")))

# ==============================================================================
# 2.3 Takeoff outcomes
# ==============================================================================

# # Fetch the data
# dt_uns <- fn_sql_sel(
#   select = "YEAR(obs) AS year, exp AS exp, COUNT(*) AS count",
#   from   = dat$tst,
#   where  = "todr > toda",
#   group  = "year, exp",
#   order  = NULL
# )
# 
# # Output results to the console
# # Unsuccessful takeoffs are those where TODR > TODA even after increasing the
# #  thrust to 100% and decreasing the mass to BELF mass.
# print(paste("Unsuccessful (n):", format(length(dt_uns), big.mark = ",")))
# print(paste("Successful (n):", format(dt_uni - length(dt_uns), big.mark = ",")))
# print(paste("Successful (%):", format((dt_uni - length(dt_uns)) / dt_uni, nsmall = "2")))
# 
# # Set y-axis limits
# lim <- c(1130E3, 1195E3)
# 
# # Create and save the plot
# (ggplot(data = dt_uns) +
#     geom_line(mapping = aes(x = year, y = count), color = "black", size = 1) +
#     scale_x_continuous(name = "Year") +
#     scale_y_continuous(name = "Count", labels = scales::comma, limits = lim) +
#     facet_wrap(~toupper(exp), ncol = 2) +
#     theme_light()) %>%
#   ggsave(
#     filename = "9_line_uns_to_cnt.png",
#     device = "png",
#     path = "plots",
#     scale = 1,
#     width = 6,
#     height = NA,
#     units = "in",
#     dpi = "print"
#   )
#
# # Add a column for percentage
# set(x = dt_uns, j = "per", value = dt_uns[, count] / dt_uni * 100)
# 
# # Create and save the plot
# (ggplot(data = dt_uns) +
#     geom_line(mapping = aes(x = year, y = per), color = "black", size = 1) +
#     scale_x_continuous(name = "Year") +
#     scale_y_continuous(name = "Count", labels = scales::percent) +
#     facet_wrap(~toupper(exp), ncol = 2) +
#     theme_light()) %>%
#   ggsave(
#     filename = "9_line_uns_to_per.png",
#     device = "png",
#     path = "plots",
#     scale = 1,
#     width = 6,
#     height = NA,
#     units = "in",
#     dpi = "print"
#   )

# ==============================================================================
# 2.4 Takeoff thrust reduction
# ==============================================================================

# # Fetch the data
# dt_thr <- fn_sql_sel(
#   select = "YEAR(obs) AS year, exp AS exp, AVG(thr_red) AS avg",
#   from   = dat$tst,
#   where  = NULL,
#   group  = "year, exp",
#   order  = NULL
# )
# 
# # Output results to the console
# print(paste("Thrust reduction:", format(dt_thr[, avg], big.mark = ",")))
# 
# # Create and save the plot
# (ggplot(data = dt_uns) +
#     geom_line(mapping = aes(x = year, y = count), color = "black", size = 1) +
#     scale_x_continuous(name = "Year") +
#     scale_y_continuous(name = "Percent", labels = scales::percent) +
#     facet_wrap(~toupper(exp), ncol = 2) +
#     theme_light()) %>%
#   ggsave(
#     filename = "9_line_thr_red.png",
#     device = "png",
#     path = "plots",
#     scale = 1,
#     width = 6,
#     height = NA,
#     units = "in",
#     dpi = "print"
#   )

# ==============================================================================
# 2.5 Takeoff payload removal
# ==============================================================================

# # Fetch the data
# dt_pax <- fn_sql_sel(
#   select = "YEAR(obs) AS year, exp AS exp, SUM(tom_red) AS sum, AVG(tom_red) AS avg",
#   from   = dat$tst,
#   where  = NULL,
#   group  = "year, exp",
#   order  = NULL
# )
# 
# # Output results to the console
# print(paste("Sum of payload removal:", format(dt_pax[, sum], big.mark = ",")))
# print(paste("Average payload removal:", format(dt_pax[, avg], big.mark = ",")))
# 
# # Create and save the plot
# (ggplot(data = dt_uns) +
#     geom_line(mapping = aes(x = year, y = count), color = "black", size = 1) +
#     scale_x_continuous(name = "Year") +
#     scale_y_continuous(name = "Count", labels = scales::percent) +
#     facet_wrap(~toupper(exp), ncol = 2) +
#     theme_light()) %>%
#   ggsave(
#     filename = "9_line_tom_red.png",
#     device = "png",
#     path = "plots",
#     scale = 1,
#     width = 6,
#     height = NA,
#     units = "in",
#     dpi = "print"
#   )

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Stop the script timer
Sys.time() - start_time

# EOF