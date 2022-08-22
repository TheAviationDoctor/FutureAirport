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
# 1.1 Relative change in mean temperature, air density, and headwind globally
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
    AVG(rho) AS avg_rho,
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
obs_q11 <- dt_q11[, .N, by = exp]

# Define the variables of interest
cols_q11 <- c("avg_tas", "avg_rho", "avg_hdw")

# Define their labels
labs_q11 <- c(
  "Relative Change in the Global Mean Near-Surface Air Temperature in °C",
  "Relative Change in the Global Mean Near-Surface Air Density in kg/m³",
  "Relative Change in the Global Mean Near-Surface Headwind Speed in m/s"
)

# Build a data table of the first year values by SSP for each variable
ini_q11 <- dt_q11[, .SD[1:1], by = exp][rep(1:.N, times = obs_q11$N)]

# Subtract the starting value (first year) from each variable and observation
dt_q11[, (cols_q11) := dt_q11[, cols_q11, with = FALSE] -
  ini_q11[, cols_q11, with = FALSE]]

# Define a function to generate plots
fn_plot_q11 <- function(cols, labs) {
  (
    ggplot(
      data    = dt_q11,
      mapping = aes(x = year, y = dt_q11[[cols]])
    ) +
      geom_line(size = .2) +
      geom_smooth(formula = y ~ x, method = "loess", size = .5) +
      scale_x_continuous("Year") +
      scale_y_continuous(labs) +
      facet_wrap(~toupper(exp), ncol = 2) +
      theme_light() +
      theme(axis.title.y = element_blank()) # Hide y axis title
  ) |>
    ggsave(
      filename = paste("9_q11_", as.name(cols), "_globally.png", sep = ""),
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
  FUN  = fn_plot_q11,
  cols = cols_q11,
  labs = labs_q11
)

# Return the local polynomial regression fitting values to smooth the volatility
dt_q11[, paste("loess_", (cols_q11), sep = "") := lapply(
    X     = .SD,
    FUN   = function(x) {
      predict(loess(formula = x ~ year, span = .75, model = TRUE))
    }
  ),
  by      = "exp",
  .SDcols = cols_q11
]

# Save the data for later reference
fwrite(
  x    = dt_q11,
  file = paste(dir$res, "dt_q11.csv", sep = "/")
)

# Display a summary table of the values for the final year
View(x = dt_q11[dt_q11[, .I[year == max(year)], by = "exp"]$V1], title = "Q11")

# ==============================================================================
# 1.2 Relative change in the maximum near-surface temperature globally
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

# Define a function to generate plots
fn_plot_q12 <- function(cols, labs) {
  (
    ggplot(
      data    = dt_q12,
      mapping = aes(x = year, y = dt_q12[[cols]])
    ) +
      geom_line(size = .2) +
      geom_smooth(formula = y ~ x, method = "loess", size = .5) +
      scale_x_continuous("Year") +
      scale_y_continuous(labs) +
      facet_wrap(~toupper(exp), ncol = 2) +
      theme_light() +
      theme(axis.title.y = element_blank()) # Hide y axis title
  ) |>
    ggsave(
      filename = paste("9_q12_", as.name(cols), "_globally.png", sep = ""),
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
  FUN  = fn_plot_q12,
  cols = cols_q12,
  labs = labs_q12
)

# Return the local polynomial regression fitting values to smooth the volatility
dt_q12[, paste("loess_", (cols_q12), sep = "") := lapply(
    X     = .SD,
    FUN   = function(x) {
      predict(loess(formula = x ~ year, span = .75, model = TRUE))
    }
  ),
  by      = "exp",
  .SDcols = cols_q12
]

# Save the data for later reference
fwrite(
  x    = dt_q12,
  file = paste(dir$res, "dt_q12.csv", sep = "/")
)

# Display a summary table of the values for the final year
View(x = dt_q12[dt_q12[, .I[year == max(year)], by = "exp"]$V1], title = "Q12")

# ==============================================================================
# 1.3 Relative change in the minimum near-surface air density globally
# ==============================================================================

# Summarize the climate data (runtime: ~6 minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
    tolower(tmp$q13),
    "AS SELECT
    exp AS exp,
    year AS year,
    icao AS icao,
    MIN(rho) AS min_rho
    FROM", tolower(dat$cli),
    "GROUP BY exp, year, icao;",
    sep = " "
  )
)

# Fetch the data
dt_q13 <- fn_sql_qry(
  statement = paste(
    "SELECT * FROM",
    tolower(tmp$q13),
    "ORDER BY exp ASC, year ASC;",
    sep = " "
  )
)

# Convert the shared socioeconomic pathway (SSP) to a factor
set(x = dt_q13, j = "exp",  value = as.factor(dt_q13[, exp]))

# Convert the year to an interval variable for continuous scaling
set(x = dt_q13, j = "year", value = as.integer(dt_q13[, year]))

# Convert the airport code to a factor
set(x = dt_q13, j = "icao", value = as.factor(dt_q13[, icao]))

# Calculate the mean minimum air density across all airports by SSP and year
dt_q13 <- dt_q13[, avg_min_rho := mean(min_rho), by = c("exp", "year")]

# Remove the airport code and minimum air density
dt_q13 <- dt_q13[, .SD, .SDcols = !c("icao", "min_rho")]

# Keep only the first observation by group
dt_q13 <- dt_q13[, .SD[1:1], c("exp", "year")]

# Count the observations by SSP
obs_q13 <- dt_q13[, .N, by = exp]

# Define the variables of interest
cols_q13 <- c("avg_min_rho")

# Define their labels
labs_q13 <- c(
  "Relative Change in the Global Minimum Near-Surface Air Density in kg/m³"
)

# Build a data table of the first year values by SSP for each variable
ini_q13 <- dt_q13[, .SD[1:1], by = exp][rep(1:.N, times = obs_q13$N)]

# Subtract the starting value (first year) from each variable and observation
dt_q13[, (cols_q13) := dt_q13[, cols_q13, with = FALSE] -
         ini_q13[, cols_q13, with = FALSE]]

# Define a function to generate plots
fn_plot_q13 <- function(cols, labs) {
  (
    ggplot(
      data    = dt_q13,
      mapping = aes(x = year, y = dt_q13[[cols]])
    ) +
      geom_line(size = .2) +
      geom_smooth(formula = y ~ x, method = "loess", size = .5) +
      scale_x_continuous("Year") +
      scale_y_continuous(labs) +
      facet_wrap(~toupper(exp), ncol = 2) +
      theme_light() +
      theme(axis.title.y = element_blank()) # Hide y axis title
  ) |>
    ggsave(
      filename = paste("9_q13_", as.name(cols), "_globally.png", sep = ""),
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
  FUN  = fn_plot_q13,
  cols = cols_q13,
  labs = labs_q13
)

# Return the local polynomial regression fitting values to smooth the volatility
dt_q13[, paste("loess_", (cols_q13), sep = "") := lapply(
    X     = .SD,
    FUN   = function(x) {
        predict(loess(formula = x ~ year, span = .75, model = TRUE))
    }
  ),
  by      = "exp",
  .SDcols = cols_q13
]

# Save the data for later reference
fwrite(
  x    = dt_q13,
  file = paste(dir$res, "dt_q13.csv", sep = "/")
)

# Display a summary table of the values for the final year
View(x = dt_q13[dt_q13[, .I[year == max(year)], by = "exp"]$V1], title = "Q13")

# ==============================================================================
# 1.4 Relative change in mean air temperature, air density, and headwind by zone
# ==============================================================================

# Summarize the climate data (runtime: ~11 minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
    tolower(tmp$q14),
    "AS SELECT
    zone as zone,
    exp AS exp,
    year AS year,
    AVG(tas) AS avg_tas,
    AVG(rho) AS avg_rho,
    AVG(hdw) AS avg_hdw
    FROM", tolower(dat$cli),
    "GROUP BY zone, exp, year;",
    sep = " "
  )
)

# Fetch the data
dt_q14 <- fn_sql_qry(
  statement = paste(
    "SELECT * FROM",
    tolower(tmp$q14),
    "ORDER BY zone ASC, exp ASC, year ASC;",
    sep = " "
  )
)

# Convert the climatic zone to a factor
set(x = dt_q14, j = "zone",  value = as.factor(dt_q14[, zone]))

# Convert the shared socioeconomic pathway (SSP) to a factor
set(x = dt_q14, j = "exp",  value = as.factor(dt_q14[, exp]))

# Convert the year to an interval variable for continuous scaling
set(x = dt_q14, j = "year", value = as.integer(dt_q14[, year]))

# Count the observations by zone and SSP
obs_q14 <- dt_q14[, .N, by = c("zone", "exp")]

# Define the variables of interest
cols_q14 <- c("avg_tas", "avg_rho", "avg_hdw")

# Define their labels
labs_q14 <- c(
  "Relative Change in the Global Mean Near-Surface Air Temperature in °C",
  "Relative Change in the Global Mean Near-Surface Air Density in kg/m³",
  "Relative Change in the Global Mean Near-Surface Headwind Speed in m/s"
)

# Build a data table of the first year values by zone and SSP for each variable
ini_q14 <- dt_q14[, .SD[1:1], by = c("zone", "exp")][rep(
  1:.N,
  times = obs_q14$N
)]

# Subtract the starting value (first year) from each variable and observation
dt_q14[, (cols_q14) := dt_q14[, cols_q14, with = FALSE] -
  ini_q14[, cols_q14, with = FALSE]]

# Define a function to generate plots
fn_plot_q14 <- function(cols, labs) {
  (
    ggplot(
      data    = dt_q14,
      mapping = aes(x = year, y = dt_q14[[cols]], color = zone)
    ) +
      geom_line(size = .2) +
      geom_smooth(formula = y ~ x, method = "loess", size = .5) +
      scale_x_continuous("Year") +
      scale_y_continuous(labs) +
      facet_wrap(~toupper(exp), ncol = 2) +
      labs(color = "Climate zone") +
      theme_light() +
      theme(axis.title.y = element_blank()) # Hide y axis title
  ) |>
  ggsave(
    filename = paste("9_q14_", as.name(cols), "_by_zone.png", sep = ""),
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
  FUN  = fn_plot_q14,
  cols = cols_q14,
  labs = labs_q14
)

# Return the local polynomial regression fitting values to smooth the volatility
dt_q14[, paste("loess_", (cols_q14), sep = "") := lapply(
    X     = .SD,
    FUN   = function(x) {
      predict(loess(formula = x ~ year, span = .75, model = TRUE))
    }
  ),
  by      = c("zone", "exp"),
  .SDcols = cols_q14
]

# Save the data for later reference
fwrite(
  x    = dt_q14,
  file = paste(dir$res, "dt_q14.csv", sep = "/")
)

# Display a summary table of the values for the final year
View(
  x = dt_q14[dt_q14[, .I[year == max(year)], by = c("zone", "exp")]$V1],
  title = "Q14"
)

# ==============================================================================
# 1.5 Relative change in the maximum near-surface temperature by zone
# ==============================================================================

# Summarize the climate data (runtime: ~14 minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
    tolower(tmp$q15),
    "AS SELECT
    zone AS zone,
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
dt_q15 <- fn_sql_qry(
  statement = paste(
    "SELECT * FROM",
    tolower(tmp$q15),
    "ORDER BY zone ASC, exp ASC, year ASC;",
    sep = " "
  )
)

# Convert the climatic zone to a factor
set(x = dt_q15, j = "zone",  value = as.factor(dt_q15[, zone]))

# Convert the shared socioeconomic pathway (SSP) to a factor
set(x = dt_q15, j = "exp",  value = as.factor(dt_q15[, exp]))

# Convert the year to an interval variable for continuous scaling
set(x = dt_q15, j = "year", value = as.integer(dt_q15[, year]))

# Convert the airport code to a factor
set(x = dt_q15, j = "icao", value = as.factor(dt_q15[, icao]))

# Count the observations by zone and SSP
obs_q15 <- dt_q15[, .N, by = c("zone", "exp")]

# Define the variables of interest
cols_q15 <- c("avg_max_tas")

# Define their labels
labs_q15 <- c(
  "Relative Change in the Global Mean Maximum Near-Surface Temperature in °C"
)

# Calculate the mean maximum temperature across all airports by zone, SSP, year
dt_q15 <- dt_q15[, avg_max_tas := mean(max_tas), by = c("zone", "exp", "year")]

# Remove the airport code and maximum temperature
dt_q15 <- dt_q15[, .SD, .SDcols = !c("icao", "max_tas")]

# Build a data table of the first year values by zone and SSP for each variable
ini_q15 <- dt_q15[, .SD[1:1], by = c("zone", "exp")][rep(
  1:.N,
  times = obs_q15$N
)]

# Subtract the starting value (first year) from each variable and observation
dt_q15[, (cols_q15) := dt_q15[, cols_q15, with = FALSE] -
         ini_q15[, cols_q15, with = FALSE]]

# Keep only the first observation by zone, SSP, and year
dt_q15 <- dt_q15[, .SD[1:1], by = c("zone", "exp", "year")]

# Define a function to generate plots
fn_plot_q15 <- function(cols, labs) {
  (
    ggplot(
      data    = dt_q15,
      mapping = aes(x = year, y = dt_q15[[cols]], color = zone)
    ) +
      geom_line(size = .2) +
      geom_smooth(formula = y ~ x, method = "loess", size = .5) +
      scale_x_continuous("Year") +
      scale_y_continuous(labs) +
      facet_wrap(~toupper(exp), ncol = 2) +
      labs(color = "Climate zone") +
      theme_light() +
      theme(axis.title.y = element_blank()) # Hide y axis title
  ) |>
  ggsave(
    filename = paste("9_q15_", as.name(cols), "_by_zone.png", sep = ""),
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
  FUN  = fn_plot_q15,
  cols = cols_q15,
  labs = labs_q15
)

# Return the local polynomial regression fitting values to smooth the volatility
dt_q15[, paste("loess_", (cols_q15), sep = "") := lapply(
    X     = .SD,
    FUN   = function(x) {
      predict(loess(formula = x ~ year, span = .75, model = TRUE))
    }
  ),
  by      = c("zone", "exp"),
  .SDcols = cols_q15
]

# Save the data for later reference
fwrite(
  x    = dt_q15,
  file = paste(dir$res, "dt_q15.csv", sep = "/")
)

# Display a summary table of the values for the final year
View(
  x = dt_q15[dt_q15[, .I[year == max(year)], by = c("zone", "exp")]$V1],
  title = "Q15"
)

# ==============================================================================
# 1.6 Relative change in the minimum near-surface air density by zone
# ==============================================================================

# Summarize the climate data (runtime: ~14 minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
    tolower(tmp$q16),
    "AS SELECT
    zone AS zone,
    exp AS exp,
    year AS year,
    icao AS icao,
    MIN(rho) AS min_rho
    FROM", tolower(dat$cli),
    "GROUP BY exp, year, icao;",
    sep = " "
  )
)

# Fetch the data
dt_q16 <- fn_sql_qry(
  statement = paste(
    "SELECT * FROM",
    tolower(tmp$q16),
    "ORDER BY zone ASC, exp ASC, year ASC;",
    sep = " "
  )
)

# Convert the climatic zone to a factor
set(x = dt_q16, j = "zone",  value = as.factor(dt_q16[, zone]))

# Convert the shared socioeconomic pathway (SSP) to a factor
set(x = dt_q16, j = "exp",  value = as.factor(dt_q16[, exp]))

# Convert the year to an interval variable for continuous scaling
set(x = dt_q16, j = "year", value = as.integer(dt_q16[, year]))

# Convert the airport code to a factor
set(x = dt_q16, j = "icao", value = as.factor(dt_q16[, icao]))

# Count the observations by zone and SSP
obs_q16 <- dt_q16[, .N, by = c("zone", "exp")]

# Define the variables of interest
cols_q16 <- c("avg_min_rho")

# Define their labels
labs_q16 <- c(
  "Relative Change in the Global Minimum Near-Surface Air Density in kg/m³"
)

# Calculate the mean minimum air density across all airports by zone, SSP, year
dt_q16 <- dt_q16[, avg_min_rho := mean(min_rho), by = c("zone", "exp", "year")]

# Remove the airport code and minimum air density
dt_q16 <- dt_q16[, .SD, .SDcols = !c("icao", "min_rho")]

# Build a data table of the first year values by zone and SSP for each variable
ini_q16 <- dt_q16[, .SD[1:1], by = c("zone", "exp")][rep(
  1:.N,
  times = obs_q16$N
)]

# Subtract the starting value (first year) from each variable and observation
dt_q16[, (cols_q16) := dt_q16[, cols_q16, with = FALSE] -
         ini_q16[, cols_q16, with = FALSE]]

# Keep only the first observation by zone, SSP, and year
dt_q16 <- dt_q16[, .SD[1:1], by = c("zone", "exp", "year")]

# Define a function to generate plots
fn_plot_q16 <- function(cols, labs) {
  (
    ggplot(
      data    = dt_q16,
      mapping = aes(x = year, y = dt_q16[[cols]], color = zone)
    ) +
      geom_line(size = .2) +
      geom_smooth(formula = y ~ x, method = "loess", size = .5) +
      scale_x_continuous("Year") +
      scale_y_continuous(labs) +
      facet_wrap(~toupper(exp), ncol = 2) +
      labs(color = "Climate zone") +
      theme_light() +
      theme(axis.title.y = element_blank()) # Hide y axis title
  ) |>
  ggsave(
    filename = paste("9_q16_", as.name(cols), "_by_zone.png", sep = ""),
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
  FUN  = fn_plot_q16,
  cols = cols_q16,
  labs = labs_q16
)

# Return the local polynomial regression fitting values to smooth the volatility
dt_q16[, paste("loess_", (cols_q16), sep = "") := lapply(
    X     = .SD,
    FUN   = function(x) {
      predict(loess(formula = x ~ year, span = .75, model = TRUE))
    }
  ),
  by      = c("zone", "exp"),
  .SDcols = cols_q16
]

# Save the data for later reference
fwrite(
  x    = dt_q16,
  file = paste(dir$res, "dt_q16.csv", sep = "/")
)

# Display a summary table of the values for the final year
View(
  x = dt_q16[dt_q16[, .I[year == max(year)], by = c("zone", "exp")]$V1],
  title = "Q16"
)

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