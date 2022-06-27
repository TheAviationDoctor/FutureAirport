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
# 1.1 Climate change globally by SSPs
# ==============================================================================

# Summarize the climate data globally by year and SSP (runtime: ~22 minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TEMPORARY TABLE IF NOT EXISTS",
    tolower(tmp$q11),
    "AS SELECT
    year AS year,
    zone AS zone,
    AVG(tas) AS avg_tas, MAX(tas) AS max_tas,
    AVG(rho) AS avg_rho, MIN(rho) AS min_rho,
    AVG(hdw) AS avg_hdw,
    exp AS exp
    FROM", tolower(dat$cli),
    "GROUP BY year, exp;",
    sep = " "
  )
)

# Fetch the summary data
dt_q11 <- fn_sql_qry(
  statement = paste(
    "SELECT * FROM",
    tolower(tmp$q11),
    "ORDER BY year ASC, exp ASC;",
    sep = " "
  )
)

# Count number of SSPs
exp_cnt <- length(unique(dt_q11[, exp]))

# Normalize observations based on first year values to show changes over time
set(
  x     = dt_q11,
  j     = "avg_tas",
  value = dt_q11[, avg_tas] - dt_q11[1:exp_cnt, avg_tas]
)
set(
  x     = dt_q11,
  j     = "max_tas",
  value = dt_q11[, max_tas] - dt_q11[1:exp_cnt, max_tas]
)
set(
  x     = dt_q11,
  j     = "avg_rho",
  value = dt_q11[, avg_rho] - dt_q11[1:exp_cnt, avg_rho]
)
set(
  x     = dt_q11,
  j     = "min_rho",
  value = dt_q11[, min_rho] - dt_q11[1:exp_cnt, min_rho]
)
set(
  x     = dt_q11,
  j     = "avg_hdw",
  value = dt_q11[, avg_hdw] - dt_q11[1:exp_cnt, avg_hdw]
)

# Define a function to generate plots
fn_plot <- function(var1, var2) {
  (
    ggplot(
      data    = dt_out,
      mapping = aes(x = year, y = dt_out[[var1]], color = toupper(exp))
    ) +
      geom_line(size = .3) +
      geom_smooth(linetype = "dashed", size = .4) +
      scale_x_continuous("Year") +
      scale_y_continuous(var2) +
      facet_wrap(~ toupper(exp), ncol = 2) +
      guides(color = "none") +
      theme_light()
  ) |>
    ggsave(
      filename = paste("9_line_", as.name(var1), ".png", sep = ""),
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
  var1 = c("avg_tas", "max_tas", "avg_rho", "min_rho", "avg_hdw"),
  var2 = c(
    "Mean near-surface temperature increase in °C",
    "Max. near-surface temperature increase in °C",
    "Mean near-surface air density in kg/m3",
    "Min. near-surface air density in kg/m3",
    "Mean near-surface headwind speed in m/s"
  )
)

# ==============================================================================
# 1.2 Climate change by zone by SSPs
# ==============================================================================

# Summarize the climate data globally by year and SSP (runtime: ~22 minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TEMPORARY TABLE IF NOT EXISTS",
    tolower(tmp$q12),
    "AS SELECT
    year AS year,
    zone AS zone,
    AVG(tas) AS avg_tas, MAX(tas) AS max_tas,
    AVG(rho) AS avg_rho, MIN(rho) AS min_rho,
    AVG(hdw) AS avg_hdw,
    exp AS exp
    FROM", tolower(dat$cli),
    "GROUP BY year, exp;",
    sep = " "
  )
)

# Fetch the summary data
dt_q12 <- fn_sql_qry(
  statement = paste(
    "SELECT * FROM",
    tolower(tmp$q12),
    "ORDER BY year ASC, exp ASC;",
    sep = " "
  )
)

# Normalize observations based on first year values to show changes over time
set(
  x     = dt_q12,
  j     = "avg_tas",
  value = dt_q12[, avg_tas] - dt_q12[1:exp_cnt, avg_tas]
)
set(
  x     = dt_q12,
  j     = "max_tas",
  value = dt_q12[, max_tas] - dt_q12[1:exp_cnt, max_tas]
)
set(
  x     = dt_q12,
  j     = "avg_rho",
  value = dt_q12[, avg_rho] - dt_q12[1:exp_cnt, avg_rho]
)
set(
  x     = dt_q12,
  j     = "min_rho",
  value = dt_q12[, min_rho] - dt_q12[1:exp_cnt, min_rho]
)
set(
  x     = dt_q12,
  j     = "avg_hdw",
  value = dt_q12[, avg_hdw] - dt_q12[1:exp_cnt, avg_hdw]
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