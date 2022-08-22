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

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# Define a function to generate plots
fn_plot <- function(dtid, var1, var2) {
  (
    ggplot(
      data    = dtid,
      mapping = aes(x = year, y = eval(parse(text = paste(as.name(dtid), "[, ", as.name(var1), "]", sep = ""))))
    ) +
      geom_line(size = .2) +
      geom_smooth(formula = y ~ x, method = "loess", size = .5) +
      scale_x_continuous("Year") +
      scale_y_continuous(var2) +
      facet_wrap(~toupper(exp), ncol = 2) +
      theme_light() +
      theme(axis.title.y = element_blank()) # Hide y axis title
  ) |>
    ggsave(
      # filename = paste("9_", as.name(dt),"_", as.name(var1), ".png", sep = ""),
      filename = "test.png",
      device   = "png",
      path     = "plots",
      scale    = 1,
      width    = 6,
      height   = NA,
      units    = "in",
      dpi      = "print"
    )
}

# ==============================================================================
# 1. Research question #1: How much change to near-surface air temperature,
# air density, and headwind will airports experience in the 21st century?
# ==============================================================================

# ==============================================================================
# 1.1 Relative change in the global mean
# ==============================================================================

# # Summarize the climate data (runtime: ~12 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#     tolower(tmp$q11),
#     "AS SELECT
#     exp AS exp,
#     year AS year,
#     AVG(tas) AS avg_tas,
#     MAX(tas) AS max_tas,
#     AVG(rho) AS avg_rho,
#     MIN(rho) AS min_rho,
#     AVG(hdw) AS avg_hdw
#     FROM", tolower(dat$cli),
#     "GROUP BY exp, year;",
#     sep = " "
#   )
# )
# 
# # Fetch the data
# dt_q11 <- fn_sql_qry(
#   statement = paste(
#     "SELECT * FROM",
#     tolower(tmp$q11),
#     "ORDER BY exp ASC, year ASC;",
#     sep = " "
#   )
# )
# 
# # Convert the shared socioeconomic pathway (SSP) to a factor
# set(x = dt_q11, j = "exp",  value = as.factor(dt_q11[, exp]))
# 
# # Convert the year to an interval variable for continuous scaling
# set(x = dt_q11, j = "year", value = as.integer(dt_q11[, year]))
# 
# # Count the observations by SSP
# obs_q11 <- dt_q11[, .N, by = exp]
# 
# # Define the variables of interest
# cols_q11 <- c("avg_tas", "max_tas", "avg_rho", "min_rho", "avg_hdw")
# 
# # Define their labels
# labs_q11 <- c(
#   "Relative Change in the Global Mean Near-Surface Air Temperature in °C",
#   "Relative Change in the Global Maximum Near-Surface Temperature in °C",
#   "Relative Change in the Global Mean Near-Surface Air Density in kg/m³",
#   "Relative Change in the Global Minimum Near-Surface Air Density in kg/m³",
#   "Relative Change in the Global Mean Near-Surface Headwind Speed in m/s"
# )
# 
# # Build a data table of the first year values by SSP for each variable
# ini_q11 <- dt_q11[, .SD[1:1], by = exp][rep(1:.N, times = obs_q11$N)]
# 
# # Subtract the starting value (first year) from each variable and observation
# dt_q11[, (cols_q11) := dt_q11[, cols_q11, with = FALSE] -
#   ini_q11[, cols_q11, with = FALSE]]
# 
# # Define a function to generate plots
# fn_plot_q11 <- function(var1, var2) {
#   (
#     ggplot(
#       data    = dt_q11,
#       mapping = aes(x = year, y = dt_q11[[var1]])
#     ) +
#       geom_line(size = .2) +
#       geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#       scale_x_continuous("Year") +
#       scale_y_continuous(var2) +
#       facet_wrap(~toupper(exp), ncol = 2) +
#       theme_light() +
#       theme(axis.title.y = element_blank()) # Hide y axis title
#   ) |>
#     ggsave(
#       filename = paste("9_q11_", as.name(var1), ".png", sep = ""),
#       device   = "png",
#       path     = "plots",
#       scale    = 1,
#       width    = 6,
#       height   = NA,
#       units    = "in",
#       dpi      = "print"
#     )
# }
# 
# # Generate the plots for every variable
# mapply(
#   FUN  = fn_plot_q11,
#   var1 = cols_q11,
#   var2 = labs_q11
# )
# 
# # Return the local polynomial regression fitting values to smooth the volatility
# dt_q11[, paste("loess_", (cols_q11), sep = "") := lapply(
#     X = .SD,
#     FUN = function(x) {
#       predict(loess(formula = x ~ year, span = .75, model = TRUE))
#     }
#   ),
#   by = "exp",
#   .SDcols = cols_q11
# ]
# 
# # Save the data for later reference
# fwrite(
#   x = dt_q11,
#   file = paste(dir$res, "dt_q11.csv", sep = "/")
# )
# 
# # Display a summary table of the values for the final year
# dt_q11[dt_q11[, .I[year == max(year)], by = "exp"]$V1]

# ==============================================================================
# 1.2 Relative change in the global maximum near-surface temperature in °C
# ==============================================================================

# Summarize the climate data (runtime: ~6 minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
    tolower(tmp$q12),
    "AS SELECT
    exp AS exp,
    year AS year,
    icao AS icao,
    MAX(tas) AS max_tas
    FROM", tolower(dat$cli),
    "GROUP BY exp, year, icao;",
    sep = " "
  )
)

# Fetch the data
dt_q12 <- fn_sql_qry(
  statement = paste(
    "SELECT * FROM",
    tolower(tmp$q12),
    "ORDER BY exp ASC, year ASC;",
    sep = " "
  )
)

# Convert the shared socioeconomic pathway (SSP) to a factor
set(x = dt_q12, j = "exp",  value = as.factor(dt_q12[, exp]))

# Convert the year to an interval variable for continuous scaling
set(x = dt_q12, j = "year", value = as.integer(dt_q12[, year]))

# Convert the airport code to a factor
set(x = dt_q12, j = "icao", value = as.factor(dt_q12[, icao]))

# # Save the data for later reference FOR TESTING ONLY
# fwrite(
#   x = dt_q12,
#   file = paste(dir$res, "dt_q12.csv", sep = "/")
# )

# Calculate the mean maximum temperature across all airports by SSP and year
dt_q12 <- dt_q12[, avg_max_tas := mean(max_tas), by = c("exp", "year")]

# Remove the airport code and maximum temperature
dt_q12 <- dt_q12[, .SD, .SDcols = !c("icao", "max_tas")]

# Keep only the first observation by group
dt_q12 <- dt_q12[, .SD[1:1], c("exp", "year")]

# Count the observations by SSP
obs_q12 <- dt_q12[, .N, by = exp]

# Define the variables of interest
cols_q12 <- c("avg_max_tas")

# Define their labels
labs_q12 <- c(
  "Relative Change in the Global Mean Maximum Near-Surface Temperature in °C"
)

# Build a data table of the first year values by SSP for each variable
ini_q12 <- dt_q12[, .SD[1:1], by = exp][rep(1:.N, times = obs_q12$N)]

# Subtract the starting value (first year) from each variable and observation
dt_q12[, (cols_q12) := dt_q12[, cols_q12, with = FALSE] -
  ini_q12[, cols_q12, with = FALSE]]

# # Define a function to generate plots
# fn_plot_q12 <- function(var1, var2) {
#   (
#     ggplot(
#       data    = dt_q12,
#       mapping = aes(x = year, y = dt_q12[[var1]])
#     ) +
#       geom_line(size = .2) +
#       geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#       scale_x_continuous("Year") +
#       scale_y_continuous(var2) +
#       facet_wrap(~toupper(exp), ncol = 2) +
#       theme_light() +
#       theme(axis.title.y = element_blank()) # Hide y axis title
#   ) |>
#     ggsave(
#       filename = paste("9_q12_", as.name(var1), ".png", sep = ""),
#       device   = "png",
#       path     = "plots",
#       scale    = 1,
#       width    = 6,
#       height   = NA,
#       units    = "in",
#       dpi      = "print"
#     )
# }

# # Generate the plots for every variable
# mapply(
#   FUN  = fn_plot_q12,
#   var1 = cols_q12,
#   var2 = labs_q12
# )

# Generate the plots for every variable
mapply(
  FUN  = fn_plot,
  dtid = dt_q12,
  var1 = cols_q12,
  var2 = labs_q12
)

# ==============================================================================
# 1.3 Relative Change in the Global Minimum Near-Surface Air Density in °C
# ==============================================================================

# # Summarize the climate data (runtime: ~6 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#     tolower(tmp$q13),
#     "AS SELECT
#     exp AS exp,
#     year AS year,
#     icao AS icao,
#     MIN(rho) AS min_rho
#     FROM", tolower(dat$cli),
#     "GROUP BY exp, year, icao;",
#     sep = " "
#   )
# )
# 
# # Fetch the data
# dt_q13 <- fn_sql_qry(
#   statement = paste(
#     "SELECT * FROM",
#     tolower(tmp$q13),
#     "ORDER BY exp ASC, year ASC, icao ASC;",
#     sep = " "
#   )
# )
# 
# # Convert the shared socioeconomic pathway (SSP) to a factor
# set(x = dt_q13, j = "exp",  value = as.factor(dt_q13[, exp]))
# 
# # Convert the year to an interval variable for continuous scaling
# set(x = dt_q13, j = "year", value = as.integer(dt_q13[, year]))
# 
# # Convert the airport code to a factor
# set(x = dt_q13, j = "icao", value = as.factor(dt_q13[, icao]))
# 
# # Save the data for later reference
# fwrite(
#   x = dt_q13,
#   file = paste(dir$res, "dt_q13.csv", sep = "/")
# )

# ==============================================================================
# 1.3 Climate change by zone
# ==============================================================================

# # Summarize the climate data (runtime: ~12 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#     tolower(tmp$q12),
#     "AS SELECT
#     zone AS zone,
#     exp AS exp,
#     year AS year,
#     AVG(tas) AS avg_tas,
#     MAX(tas) AS max_tas,
#     AVG(rho) AS avg_rho,
#     MIN(rho) AS min_rho,
#     AVG(hdw) AS avg_hdw
#     FROM", tolower(dat$cli),
#     "GROUP BY zone, exp, year;",
#     sep = " "
#   )
# )
# 
# # Fetch the data
# dt_q12 <- fn_sql_qry(
#   statement = paste(
#     "SELECT * FROM",
#     tolower(tmp$q12),
#     "ORDER BY zone ASC, exp ASC, year ASC;",
#     sep = " "
#   )
# )
# 
# # Convert the climatic zone to a factor
# set(x = dt_q12, j = "zone",  value = as.factor(dt_q12[, zone]))
# 
# # Convert the SSP to a factor
# set(x = dt_q12, j = "exp",  value = as.factor(dt_q12[, exp]))
# 
# # Convert the year to an interval variable for continuous scaling
# set(x = dt_q12, j = "year", value = as.integer(dt_q12[, year]))
# 
# # # Count the observations by zone and SSP
# obs_q12 <- dt_q12[, .N, by = c("zone", "exp")]
# 
# # Define the variables of interest
# cols_q12 <- c("avg_tas", "max_tas", "avg_rho", "min_rho", "avg_hdw")
# 
# # Define their labels
# labs_q12 <- c(
#   "Relative Change in the Zonal Mean Near-Surface Temperature in °C",
#   "Relative Change in the Zonal Maximum Near-Surface Temperature in °C",
#   "Relative Change in the Zonal Mean Near-Surface Air Density in kg/m³",
#   "Relative Change in the Zonal Minimum Near-Surface Air Density in kg/m³",
#   "Relative Change in the Zonal Mean Near-Surface Headwind Speed in m/s"
# )
# 
# # Build a data table of the first year values by zone and SSP for each variable
# ini_q12 <- dt_q12[, .SD[1:1], by = c("zone", "exp")][rep(
#   1:.N,
#   times = obs_q12$N
# )]
# 
# # Subtract the starting value (first year) from each variable and observation
# dt_q12[, (cols_q12) := dt_q12[, cols_q12, with = FALSE] -
#   ini_q12[, cols_q12, with = FALSE]]
# 
# # Define a function to generate plots
# fn_plot_q12 <- function(var1, var2) {
#   (
#     ggplot(
#       data    = dt_q12,
#       mapping = aes(x = year, y = dt_q12[[var1]], color = zone)
#     ) +
#       geom_line(size = .2) +
#       geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#       scale_x_continuous("Year") +
#       scale_y_continuous(var2) +
#       facet_wrap(~toupper(exp), ncol = 2) +
#       labs(color = "Climate zone") +
#       theme_light() +
#       theme(axis.title.y = element_blank()) # Hide y axis title
#   ) |>
#   ggsave(
#     filename = paste("9_q12_", as.name(var1), ".png", sep = ""),
#     device   = "png",
#     path     = "plots",
#     scale    = 1,
#     width    = 6,
#     height   = NA,
#     units    = "in",
#     dpi      = "print"
#   )
# }
# 
# # Generate the plots for every variable
# mapply(
#   FUN  = fn_plot_q12,
#   var1 = cols_q12,
#   var2 = labs_q12
# )
# 
# # Return the local polynomial regression fitting values to smooth the volatility
# dt_q12[, paste("loess_", (cols_q12), sep = "") := lapply(
#     X = .SD,
#     FUN = function(x) {
#       predict(loess(formula = x ~ year, span = .75, model = TRUE))
#     }
#   ),
#   by = c("zone", "exp"),
#   .SDcols = cols_q12
# ]
# 
# # Save the data for later reference
# fwrite(
#   x = dt_q12,
#   file = paste(dir$res, "dt_q12.csv", sep = "/")
# )
# 
# # Save a summary table of the values for the final year
# dt_q12[dt_q12[, .I[year == max(year)], by = c("zone", "exp")]$V1]



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