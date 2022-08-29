# ==============================================================================
#    NAME: scripts/9_analyze.R
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
# 1. Climate change at the sample airports
# ==============================================================================

# ==============================================================================
# 1.1 Relative change in mean temperature, air density, and headwind globally
# ==============================================================================

# # Create the summary table (runtime: ~12 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#     tolower(tmp$q11),
#     "AS SELECT
#     exp AS exp,
#     year AS year,
#     AVG(tas) AS avg_tas,
#     AVG(rho) AS avg_rho,
#     AVG(hdw) AS avg_hdw
#     FROM",
#     tolower(dat$cli),
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
# cols_q11 <- c("avg_tas", "avg_rho", "avg_hdw")
# 
# # Define their labels
# labs_q11 <- c(
#   "Relative Change in the Global Mean Near-Surface Air Temperature in °C",
#   "Relative Change in the Global Mean Near-Surface Air Density in kg/m³",
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
# fn_plot_q11 <- function(cols, labs) {
#   (
#     ggplot(
#       data    = dt_q11,
#       mapping = aes(x = year, y = dt_q11[[cols]])
#     ) +
#       geom_line(size = .2) +
#       geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#       scale_x_continuous(name = "Year") +
#       scale_y_continuous(name = labs) +
#       facet_wrap(~toupper(exp), ncol = 2) +
#       theme_light() +
#       theme(axis.title.y = element_blank())
#   ) |>
#     ggsave(
#       filename = paste("9_q11_", as.name(cols), "_globally.png", sep = ""),
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
#   cols = cols_q11,
#   labs = labs_q11
# )
# 
# # Return the local polynomial regression fitting values to smooth the volatility
# dt_q11[, paste("loess_", (cols_q11), sep = "") := lapply(
#     X     = .SD,
#     FUN   = function(x) {
#       predict(loess(formula = x ~ year, span = .75, model = TRUE))
#     }
#   ),
#   by      = "exp",
#   .SDcols = cols_q11
# ]
# 
# # Save the data for later reference
# fwrite(
#   x    = dt_q11,
#   file = paste(dir$res, "dt_q11.csv", sep = "/")
# )
# 
# # Display the summary table of the values for the final year
# View(x = dt_q11[dt_q11[, .I[year == max(year)], by = "exp"]$V1], title = "Q11")

# ==============================================================================
# 1.2 Relative change in the maximum near-surface temperature globally
# ==============================================================================

# # Create the summary table (runtime: ~6 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#     tolower(tmp$q12),
#     "AS SELECT
#     exp AS exp,
#     year AS year,
#     icao AS icao,
#     MAX(tas) AS max_tas
#     FROM",
#     tolower(dat$cli),
#     "GROUP BY exp, year, icao;",
#     sep = " "
#   )
# )
# 
# # Fetch the data
# dt_q12 <- fn_sql_qry(
#   statement = paste(
#     "SELECT * FROM",
#     tolower(tmp$q12),
#     "ORDER BY exp ASC, year ASC;",
#     sep = " "
#   )
# )
# 
# # Convert the shared socioeconomic pathway (SSP) to a factor
# set(x = dt_q12, j = "exp",  value = as.factor(dt_q12[, exp]))
# 
# # Convert the year to an interval variable for continuous scaling
# set(x = dt_q12, j = "year", value = as.integer(dt_q12[, year]))
# 
# # Convert the airport code to a factor
# set(x = dt_q12, j = "icao", value = as.factor(dt_q12[, icao]))
# 
# # Calculate the mean maximum temperature across all airports by SSP and year
# dt_q12 <- dt_q12[, avg_max_tas := mean(max_tas), by = c("exp", "year")]
# 
# # Remove the airport code and maximum temperature
# dt_q12 <- dt_q12[, .SD, .SDcols = !c("icao", "max_tas")]
# 
# # Keep only the first observation by group
# dt_q12 <- dt_q12[, .SD[1:1], c("exp", "year")]
# 
# # Count the observations by SSP
# obs_q12 <- dt_q12[, .N, by = exp]
# 
# # Define the variables of interest
# cols_q12 <- c("avg_max_tas")
# 
# # Define their labels
# labs_q12 <- c(
#   "Relative Change in the Global Mean Maximum Near-Surface Temperature in °C"
# )
# 
# # Build a data table of the first year values by SSP for each variable
# ini_q12 <- dt_q12[, .SD[1:1], by = exp][rep(1:.N, times = obs_q12$N)]
# 
# # Subtract the starting value (first year) from each variable and observation
# dt_q12[, (cols_q12) := dt_q12[, cols_q12, with = FALSE] -
#   ini_q12[, cols_q12, with = FALSE]]
# 
# # Define a function to generate plots
# fn_plot_q12 <- function(cols, labs) {
#   (
#     ggplot(
#       data    = dt_q12,
#       mapping = aes(x = year, y = dt_q12[[cols]])
#     ) +
#       geom_line(size = .2) +
#       geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#       scale_x_continuous(name = "Year") +
#       scale_y_continuous(name = labs) +
#       facet_wrap(~toupper(exp), ncol = 2) +
#       theme_light() +
#       theme(axis.title.y = element_blank())
#   ) |>
#     ggsave(
#       filename = paste("9_q12_", as.name(cols), "_globally.png", sep = ""),
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
#   FUN  = fn_plot_q12,
#   cols = cols_q12,
#   labs = labs_q12
# )
# 
# # Return the local polynomial regression fitting values to smooth the volatility
# dt_q12[, paste("loess_", (cols_q12), sep = "") := lapply(
#     X     = .SD,
#     FUN   = function(x) {
#       predict(loess(formula = x ~ year, span = .75, model = TRUE))
#     }
#   ),
#   by      = "exp",
#   .SDcols = cols_q12
# ]
# 
# # Save the data for later reference
# fwrite(
#   x    = dt_q12,
#   file = paste(dir$res, "dt_q12.csv", sep = "/")
# )
# 
# # Display the summary table of the values for the final year
# View(x = dt_q12[dt_q12[, .I[year == max(year)], by = "exp"]$V1], title = "Q12")

# ==============================================================================
# 1.3 Relative change in the minimum near-surface air density globally
# ==============================================================================

# # Create the summary table (runtime: ~6 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#     tolower(tmp$q13),
#     "AS SELECT
#     exp AS exp,
#     year AS year,
#     icao AS icao,
#     MIN(rho) AS min_rho
#     FROM",
#     tolower(dat$cli),
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
#     "ORDER BY exp ASC, year ASC;",
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
# # Calculate the mean minimum air density across all airports by SSP and year
# dt_q13 <- dt_q13[, avg_min_rho := mean(min_rho), by = c("exp", "year")]
# 
# # Remove the airport code and minimum air density
# dt_q13 <- dt_q13[, .SD, .SDcols = !c("icao", "min_rho")]
# 
# # Keep only the first observation by group
# dt_q13 <- dt_q13[, .SD[1:1], c("exp", "year")]
# 
# # Count the observations by SSP
# obs_q13 <- dt_q13[, .N, by = exp]
# 
# # Define the variables of interest
# cols_q13 <- c("avg_min_rho")
# 
# # Define their labels
# labs_q13 <- c(
#   "Relative Change in the Global Minimum Near-Surface Air Density in kg/m³"
# )
# 
# # Build a data table of the first year values by SSP for each variable
# ini_q13 <- dt_q13[, .SD[1:1], by = exp][rep(1:.N, times = obs_q13$N)]
# 
# # Subtract the starting value (first year) from each variable and observation
# dt_q13[, (cols_q13) := dt_q13[, cols_q13, with = FALSE] -
#          ini_q13[, cols_q13, with = FALSE]]
# 
# # Define a function to generate plots
# fn_plot_q13 <- function(cols, labs) {
#   (
#     ggplot(
#       data    = dt_q13,
#       mapping = aes(x = year, y = dt_q13[[cols]])
#     ) +
#       geom_line(size = .2) +
#       geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#       scale_x_continuous(name = "Year") +
#       scale_y_continuous(name = labs) +
#       facet_wrap(~toupper(exp), ncol = 2) +
#       theme_light() +
#       theme(axis.title.y = element_blank())
#   ) |>
#     ggsave(
#       filename = paste("9_q13_", as.name(cols), "_globally.png", sep = ""),
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
#   FUN  = fn_plot_q13,
#   cols = cols_q13,
#   labs = labs_q13
# )
# 
# # Return the local polynomial regression fitting values to smooth the volatility
# dt_q13[, paste("loess_", (cols_q13), sep = "") := lapply(
#     X     = .SD,
#     FUN   = function(x) {
#         predict(loess(formula = x ~ year, span = .75, model = TRUE))
#     }
#   ),
#   by      = "exp",
#   .SDcols = cols_q13
# ]
# 
# # Save the data for later reference
# fwrite(
#   x    = dt_q13,
#   file = paste(dir$res, "dt_q13.csv", sep = "/")
# )
# 
# # Display the summary table of the values for the final year
# View(x = dt_q13[dt_q13[, .I[year == max(year)], by = "exp"]$V1], title = "Q13")

# ==============================================================================
# 1.4 Relative change in mean air temperature, air density, and headwind by zone
# ==============================================================================

# # Create the summary table (runtime: ~11 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#     tolower(tmp$q14),
#     "AS SELECT
#     zone as zone,
#     exp AS exp,
#     year AS year,
#     AVG(tas) AS avg_tas,
#     AVG(rho) AS avg_rho,
#     AVG(hdw) AS avg_hdw
#     FROM",
#     tolower(dat$cli),
#     "GROUP BY zone, exp, year;",
#     sep = " "
#   )
# )
# 
# # Fetch the data
# dt_q14 <- fn_sql_qry(
#   statement = paste(
#     "SELECT * FROM",
#     tolower(tmp$q14),
#     "ORDER BY zone ASC, exp ASC, year ASC;",
#     sep = " "
#   )
# )
# 
# # Convert the climatic zone to a factor
# set(x = dt_q14, j = "zone",  value = as.factor(dt_q14[, zone]))
# 
# # Convert the shared socioeconomic pathway (SSP) to a factor
# set(x = dt_q14, j = "exp",  value = as.factor(dt_q14[, exp]))
# 
# # Convert the year to an interval variable for continuous scaling
# set(x = dt_q14, j = "year", value = as.integer(dt_q14[, year]))
# 
# # Count the observations by zone and SSP
# obs_q14 <- dt_q14[, .N, by = c("zone", "exp")]
# 
# # Define the variables of interest
# cols_q14 <- c("avg_tas", "avg_rho", "avg_hdw")
# 
# # Define their labels
# labs_q14 <- c(
#   "Relative Change in the Global Mean Near-Surface Air Temperature in °C",
#   "Relative Change in the Global Mean Near-Surface Air Density in kg/m³",
#   "Relative Change in the Global Mean Near-Surface Headwind Speed in m/s"
# )
# 
# # Build a data table of the first year values by zone and SSP for each variable
# ini_q14 <- dt_q14[, .SD[1:1], by = c("zone", "exp")][rep(
#   1:.N,
#   times = obs_q14$N
# )]
# 
# # Subtract the starting value (first year) from each variable and observation
# dt_q14[, (cols_q14) := dt_q14[, cols_q14, with = FALSE] -
#   ini_q14[, cols_q14, with = FALSE]]
# 
# # Define a function to generate plots
# fn_plot_q14 <- function(cols, labs) {
#   (
#     ggplot(
#       data    = dt_q14,
#       mapping = aes(x = year, y = dt_q14[[cols]], color = zone)
#     ) +
#       geom_line(size = .2) +
#       geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#       scale_x_continuous(name = "Year") +
#       scale_y_continuous(name = labs) +
#       facet_wrap(~toupper(exp), ncol = 2) +
#       labs(color = "Climate zone") +
#       theme_light() +
#       theme(axis.title.y = element_blank())
#   ) |>
#   ggsave(
#     filename = paste("9_q14_", as.name(cols), "_by_zone.png", sep = ""),
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
#   FUN  = fn_plot_q14,
#   cols = cols_q14,
#   labs = labs_q14
# )
# 
# # Return the local polynomial regression fitting values to smooth the volatility
# dt_q14[, paste("loess_", (cols_q14), sep = "") := lapply(
#     X     = .SD,
#     FUN   = function(x) {
#       predict(loess(formula = x ~ year, span = .75, model = TRUE))
#     }
#   ),
#   by      = c("zone", "exp"),
#   .SDcols = cols_q14
# ]
# 
# # Save the data for later reference
# fwrite(
#   x    = dt_q14,
#   file = paste(dir$res, "dt_q14.csv", sep = "/")
# )
# 
# # Display the summary table of the values for the final year
# View(
#   x = dt_q14[dt_q14[, .I[year == max(year)], by = c("zone", "exp")]$V1],
#   title = "Q14"
# )

# ==============================================================================
# 1.5 Relative change in the maximum near-surface temperature by zone
# ==============================================================================

# # Create the summary table (runtime: ~14 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#     tolower(tmp$q15),
#     "AS SELECT
#     zone AS zone,
#     exp AS exp,
#     year AS year,
#     icao AS icao,
#     MAX(tas) AS max_tas
#     FROM",
#     tolower(dat$cli),
#     "GROUP BY exp, year, icao;",
#     sep = " "
#   )
# )
# 
# # Fetch the data
# dt_q15 <- fn_sql_qry(
#   statement = paste(
#     "SELECT * FROM",
#     tolower(tmp$q15),
#     "ORDER BY zone ASC, exp ASC, year ASC;",
#     sep = " "
#   )
# )
# 
# # Convert the climatic zone to a factor
# set(x = dt_q15, j = "zone",  value = as.factor(dt_q15[, zone]))
# 
# # Convert the shared socioeconomic pathway (SSP) to a factor
# set(x = dt_q15, j = "exp",  value = as.factor(dt_q15[, exp]))
# 
# # Convert the year to an interval variable for continuous scaling
# set(x = dt_q15, j = "year", value = as.integer(dt_q15[, year]))
# 
# # Convert the airport code to a factor
# set(x = dt_q15, j = "icao", value = as.factor(dt_q15[, icao]))
# 
# # Count the observations by zone and SSP
# obs_q15 <- dt_q15[, .N, by = c("zone", "exp")]
# 
# # Define the variables of interest
# cols_q15 <- c("avg_max_tas")
# 
# # Define their labels
# labs_q15 <- c(
#   "Relative Change in the Global Mean Maximum Near-Surface Temperature in °C"
# )
# 
# # Calculate the mean maximum temperature across all airports by zone, SSP, year
# dt_q15 <- dt_q15[, avg_max_tas := mean(max_tas), by = c("zone", "exp", "year")]
# 
# # Remove the airport code and maximum temperature
# dt_q15 <- dt_q15[, .SD, .SDcols = !c("icao", "max_tas")]
# 
# # Build a data table of the first year values by zone and SSP for each variable
# ini_q15 <- dt_q15[, .SD[1:1], by = c("zone", "exp")][rep(
#   1:.N,
#   times = obs_q15$N
# )]
# 
# # Subtract the starting value (first year) from each variable and observation
# dt_q15[, (cols_q15) := dt_q15[, cols_q15, with = FALSE] -
#          ini_q15[, cols_q15, with = FALSE]]
# 
# # Keep only the first observation by zone, SSP, and year
# dt_q15 <- dt_q15[, .SD[1:1], by = c("zone", "exp", "year")]
# 
# # Define a function to generate plots
# fn_plot_q15 <- function(cols, labs) {
#   (
#     ggplot(
#       data    = dt_q15,
#       mapping = aes(x = year, y = dt_q15[[cols]], color = zone)
#     ) +
#       geom_line(size = .2) +
#       geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#       scale_x_continuous(name = "Year") +
#       scale_y_continuous(name = labs) +
#       facet_wrap(~toupper(exp), ncol = 2) +
#       labs(color = "Climate zone") +
#       theme_light() +
#       theme(axis.title.y = element_blank())
#   ) |>
#   ggsave(
#     filename = paste("9_q15_", as.name(cols), "_by_zone.png", sep = ""),
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
#   FUN  = fn_plot_q15,
#   cols = cols_q15,
#   labs = labs_q15
# )
# 
# # Return the local polynomial regression fitting values to smooth the volatility
# dt_q15[, paste("loess_", (cols_q15), sep = "") := lapply(
#     X     = .SD,
#     FUN   = function(x) {
#       predict(loess(formula = x ~ year, span = .75, model = TRUE))
#     }
#   ),
#   by      = c("zone", "exp"),
#   .SDcols = cols_q15
# ]
# 
# # Save the data for later reference
# fwrite(
#   x    = dt_q15,
#   file = paste(dir$res, "dt_q15.csv", sep = "/")
# )
# 
# # Display the summary table of the values for the final year
# View(
#   x = dt_q15[dt_q15[, .I[year == max(year)], by = c("zone", "exp")]$V1],
#   title = "Q15"
# )

# ==============================================================================
# 1.6 Relative change in the minimum near-surface air density by zone
# ==============================================================================

# # Create the summary table (runtime: ~14 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#     tolower(tmp$q16),
#     "AS SELECT
#     zone AS zone,
#     exp AS exp,
#     year AS year,
#     icao AS icao,
#     MIN(rho) AS min_rho
#     FROM",
#     tolower(dat$cli),
#     "GROUP BY exp, year, icao;",
#     sep = " "
#   )
# )
# 
# # Fetch the data
# dt_q16 <- fn_sql_qry(
#   statement = paste(
#     "SELECT * FROM",
#     tolower(tmp$q16),
#     "ORDER BY zone ASC, exp ASC, year ASC;",
#     sep = " "
#   )
# )
# 
# # Convert the climatic zone to a factor
# set(x = dt_q16, j = "zone",  value = as.factor(dt_q16[, zone]))
# 
# # Convert the shared socioeconomic pathway (SSP) to a factor
# set(x = dt_q16, j = "exp",  value = as.factor(dt_q16[, exp]))
# 
# # Convert the year to an interval variable for continuous scaling
# set(x = dt_q16, j = "year", value = as.integer(dt_q16[, year]))
# 
# # Convert the airport code to a factor
# set(x = dt_q16, j = "icao", value = as.factor(dt_q16[, icao]))
# 
# # Count the observations by zone and SSP
# obs_q16 <- dt_q16[, .N, by = c("zone", "exp")]
# 
# # Define the variables of interest
# cols_q16 <- c("avg_min_rho")
# 
# # Define their labels
# labs_q16 <- c(
#   "Relative Change in the Global Minimum Near-Surface Air Density in kg/m³"
# )
# 
# # Calculate the mean minimum air density across all airports by zone, SSP, year
# dt_q16 <- dt_q16[, avg_min_rho := mean(min_rho), by = c("zone", "exp", "year")]
# 
# # Remove the airport code and minimum air density
# dt_q16 <- dt_q16[, .SD, .SDcols = !c("icao", "min_rho")]
# 
# # Build a data table of the first year values by zone and SSP for each variable
# ini_q16 <- dt_q16[, .SD[1:1], by = c("zone", "exp")][rep(
#   1:.N,
#   times = obs_q16$N
# )]
# 
# # Subtract the starting value (first year) from each variable and observation
# dt_q16[, (cols_q16) := dt_q16[, cols_q16, with = FALSE] -
#          ini_q16[, cols_q16, with = FALSE]]
# 
# # Keep only the first observation by zone, SSP, and year
# dt_q16 <- dt_q16[, .SD[1:1], by = c("zone", "exp", "year")]
# 
# # Define a function to generate plots
# fn_plot_q16 <- function(cols, labs) {
#   (
#     ggplot(
#       data    = dt_q16,
#       mapping = aes(x = year, y = dt_q16[[cols]], color = zone)
#     ) +
#       geom_line(size = .2) +
#       geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#       scale_x_continuous(name = "Year") +
#       scale_y_continuous(name = labs) +
#       facet_wrap(~toupper(exp), ncol = 2) +
#       labs(color = "Climate zone") +
#       theme_light() +
#       theme(axis.title.y = element_blank())
#   ) |>
#   ggsave(
#     filename = paste("9_q16_", as.name(cols), "_by_zone.png", sep = ""),
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
#   FUN  = fn_plot_q16,
#   cols = cols_q16,
#   labs = labs_q16
# )
# 
# # Return the local polynomial regression fitting values to smooth the volatility
# dt_q16[, paste("loess_", (cols_q16), sep = "") := lapply(
#     X     = .SD,
#     FUN   = function(x) {
#       predict(loess(formula = x ~ year, span = .75, model = TRUE))
#     }
#   ),
#   by      = c("zone", "exp"),
#   .SDcols = cols_q16
# ]
# 
# # Save the data for later reference
# fwrite(
#   x    = dt_q16,
#   file = paste(dir$res, "dt_q16.csv", sep = "/")
# )
# 
# # Display the summary table of the values for the final year
# View(
#   x = dt_q16[dt_q16[, .I[year == max(year)], by = c("zone", "exp")]$V1],
#   title = "Q16"
# )

# ==============================================================================
# 2. Takeoff performance at the sample airports
# ==============================================================================

# ==============================================================================
# 2.1 Takeoffs and iterations
# ==============================================================================

# # Create the summary table (runtime: ~33 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#     tolower(tmp$q21),
#     "(tko_cnt INT, itr_sum BIGINT, itr_avg FLOAT)",
#     "AS SELECT
#     COUNT(*),
#     SUM(itr),
#     AVG(itr)
#     FROM",
#     tolower(dat$tko),
#     ";",
#     sep = " "
#   )
# )
# 
# # Fetch the data
# dt_q21 <- fn_sql_qry(
#   statement = paste(
#     "SELECT * FROM",
#     tolower(tmp$q21),
#     sep = " "
#   )
# )
# 
# # Display the summary table of the takeoffs
# View(
#   x     = dt_q21,
#   title = "Takeoffs & iterations"
# )

# ==============================================================================
# 2.2 Takeoff outcomes
# ==============================================================================

# Create the summary table (runtime: ~70 minutes)
# a. tko_ok_thr_min refers to a successful takeoff with thrust at 75% TOGA
# b. tko_ok_thr_mid refers to a successful takeoff with thrust at 76%-99% TOGA
# c. tko_ok_thr_max refers to a successful takeoff with thrust at 100% TOGA
# d. tko_ok refers to a successful takeoff (a + b + c)
# e. tko_ko refers to an unsuccessful takeoff
# f. tko refers to any takeoff (d + e)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
    tolower(tmp$q22),
    "(year YEAR, exp CHAR(6), type CHAR(4),
    tko_ok_thr_min INT, tko_ok_thr_mid INT, tko_ok_thr_max INT,
    tko_ok INT, tko_ko INT,
    tko INT)
    AS SELECT
    YEAR(obs) AS year,
    exp,
    type,
    SUM(thr_red =", sim$thr_ini, ") AS tko_ok_thr_min,
    SUM(thr_red BETWEEN 1 AND ", sim$thr_ini - 1L, ") AS tko_ok_thr_mid,
    SUM(thr_red = 0 AND todr <= toda) AS tko_ok_thr_max,
    SUM(todr <= toda) AS tko_ok,
    SUM(todr > toda) AS tko_ko,
    COUNT(*) AS tko
    FROM",
    tolower(dat$tko),
    "GROUP BY year, exp, type",
    ";",
    sep = " "
  )
)

# Fetch the data
dt_q22 <- fn_sql_qry(
  statement = paste(
    "SELECT * FROM",
    tolower(tmp$q22),
    sep = " "
  )
)

# Convert the year to an interval variable for continuous scaling
set(x = dt_q22, j = "year", value = as.integer(dt_q22[, year]))

# Convert the shared socioeconomic pathway (SSP) to a factor
set(x = dt_q22, j = "exp",  value = as.factor(dt_q22[, exp]))

# Convert the aircraft type to a factor
set(x = dt_q22, j = "type",  value = as.factor(dt_q22[, type]))

# Combine narrowbodies and widebodies, respectively
levels(dt_q22$type) <- c(
  "A20n" = "Narrowbody",
  "B39m" = "Narrowbody",
  "A359" = "Widebody",
  "B789" = "Widebody"
)

# Define the variables of interest
cols_q31 <- c(
  "tko_ok_thr_min",
  "tko_ok_thr_mid",
  "tko_ok_thr_max",
  "tko_ok",
  "tko_ko",
  "tko"
)

# Define their labels
labs_q31 <- c(
  "75% TOGA",
  "76%-99% TOGA",
  "TOGA",
  "Succesful takeoff",
  "Unsuccessful takeoff",
  "All takeoffs"
)

# Summarize the data
dt_q22 <- dt_q22[, lapply(.SD, sum),
  by = c("year", "exp", "type"),
  .SDcols = cols_q31
]

# Convert thrust-reduced takeoffs to percentages of all takeoffs row-wise
dt_q22[, c(
    "tko_ok_thr_min_per",
    "tko_ok_thr_mid_per",
    "tko_ok_thr_max_per",
    "tko_ok_per",
    "tko_ko_per",
    "tko_per"
  ) := lapply(
    X = .SD,
    FUN = function(x) {
      x / dt_q22[, tko]
    }
  ),
  .SDcols = cols_q31
]

# Return the local polynomial regression fitting values to smooth the volatility
dt_q22[, paste(
    c("tko_ok_thr_min_per", "tko_ko_per"),
    "_loess",
    sep = ""
  ) := lapply(
    X = .SD,
    FUN = function(x) {
      predict(loess(formula = x ~ year, span = .75, model = TRUE))
    }
  ),
  by = c("exp", "type"),
  .SDcols = c("tko_ok_thr_min_per", "tko_ko_per")
]

# Display the summary table
View(
  x     = dt_q22,
  title = "Takeoff outcomes"
)

# Define a function to generate plots
fn_plot_q22 <- function(cols, labs) {
  (
    ggplot(
      data    = dt_q22,
      mapping = aes(x = year, y = dt_q22[[cols]])
    ) +
      geom_line() +
      geom_smooth(formula = y ~ x, method = "loess", size = .5) +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(
        name   = labs,
        labels = scales::percent) +
      facet_wrap(toupper(exp) ~ type, ncol = 2, scales = "free_y") +
      theme_light() +
      theme(axis.title.y = element_blank())
  ) |>
    ggsave(
      filename = paste("9_q22_", as.name(cols), ".png", sep = ""),
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
  FUN  = fn_plot_q22,
  cols = c("tko_ok_thr_min_per", "tko_ko_per"),
  labs = c("75% TOGA", "Unsuccessful takeoffs")
)

# ==============================================================================
# 3. Research question #1
# How much thrust increase will be required to lift an equivalent payload?
# ==============================================================================

# ==============================================================================
# 3.1 Average takeoff thrust
# ==============================================================================

# # Create the summary table (runtime: ~60 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#     tolower(tmp$q35),
#     "(year YEAR, exp CHAR(6), zone CHAR(11), type CHAR(4), thr_red_avg FLOAT)",
#     "AS SELECT
#     YEAR(obs) AS year,
#     exp,
#     zone,
#     type,
#     AVG(thr_red) AS thr_red_avg
#     FROM",
#     tolower(dat$tko),
#     "WHERE todr <= toda",
#     "GROUP BY year, exp, zone, type",
#     ";",
#     sep = " "
#   )
# )
# 
# # Fetch the data
# dt_q35 <- fn_sql_qry(
#   statement = paste(
#     "SELECT * FROM",
#     tolower(tmp$q35),
#     sep = " "
#   )
# )
# 
# # Convert the year to an interval variable for continuous scaling
# set(x = dt_q35, j = "year", value = as.integer(dt_q35[, year]))
# 
# # Convert the climatic zone to a factor
# set(x = dt_q35, j = "zone",  value = as.factor(dt_q35[, zone]))
# 
# # Convert the shared socioeconomic pathway (SSP) to a factor
# set(x = dt_q35, j = "exp",  value = as.factor(dt_q35[, exp]))
# 
# # Convert the aircraft type to a factor
# set(x = dt_q35, j = "type",  value = as.factor(dt_q35[, type]))
# 
# # Convert the mean thrust reduction to mean thrust as a percentage of TOGA
# set(x = dt_q35, j = "thr_red_avg",  value = (100 - dt_q35[, thr_red_avg]) / 100)
# 
# # Combine narrowbodies and widebodies, respectively
# levels(dt_q35$type) <- c(
#   "A20n" = "Narrowbody",
#   "B39m" = "Narrowbody",
#   "A359" = "Widebody",
#   "B789" = "Widebody"
# )
# 
# # FOR TESTING
# fwrite(dt_q35, file = "dt_q35.csv")
# 
# # Summarize the average thrust reduction by year, SSP, and aircraft type
# dt_q35 <- dt_q35[, lapply(.SD, mean),
#   by = c("year", "exp", "type"),
#   .SDcols = c("thr_red_avg")
# ]
# 
# # Pivot the data from wide to long for plotting
# dt_q35 <- dcast(data = dt_q35, id.vars = "year")
# 
# # Keep only one aircraft type
# # dt_q35 <- dt_q35[type == "Widebody"]
# 
# # Display the summary table
# View(
#   x     = dt_q35,
#   title = "Average thrust reduction"
# )
# 
# # Create a plot
# (
#   ggplot(
#     data    = dt_q35,
#     mapping = aes(x = year, y = thr_red_avg)
#   ) +
#     geom_line() +
#     geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#     scale_x_continuous(name = "Year") +
#     facet_wrap(~toupper(exp), ncol = 2) +
#     theme_light() +
#     theme(axis.title.y = element_blank())
# )
# |>
# ggsave(
#   filename = paste("9_q31_thr_red.png", sep = ""),
#   device   = "png",
#   path     = "plots",
#   scale    = 1,
#   width    = 6,
#   height   = NA,
#   units    = "in",
#   dpi      = "print"
# )

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