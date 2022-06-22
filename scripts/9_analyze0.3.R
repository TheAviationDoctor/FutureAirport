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

# IMPORTANT NOTE
# Some of the queries below are memory-intensive for the MySQL engine.
# If you run into the following error:
# "The total number of locks exceeds the lock table size",
# log into MySQL Workbench with admin rights to increase the InnoDB buffer size:
# 'SET GLOBAL innodb_buffer_pool_size = 64 * 1024 * 1024 * 1024;' [this is 64GB]

# ==============================================================================
# 1. Research question #1: How much change to near-surface air temperature,
# air density, and headwind will airports experience in the 21st century?
# ==============================================================================

# ==============================================================================
# 1.1 Summarize the climate data in the data in the database
# Runtime: ~22 minutes
# ==============================================================================

fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
    tolower(vis$cli_glb),
    "AS SELECT
    YEAR(obs) AS year,
    AVG(tas) AS avg_tas, MAX(tas) AS max_tas,
    AVG(rho) AS avg_rho, MIN(rho) AS min_rho,
    AVG(hdw) AS avg_hdw,
    exp AS exp
    FROM", tolower(dat$cli),
    "GROUP BY year, exp;",
    sep = " "
  )
)

# ==============================================================================
# 1.2 Visualize the data from the summary table
# ==============================================================================

dt_out <- fn_sql_qry(
  statement = paste(
    "SELECT * FROM",
    tolower(vis$cli_glb),
    "GROUP BY year, exp;",
    sep = " "
  )
)

# Count the number of SSPs
exp_cnt <- length(unique(dt_cla[, exp]))

# Normalize observations based on first year values to show changes over time
set(
  x     = dt_cla,
  j     = "avg_tas",
  value = dt_cla[, avg_tas] - dt_cla[1:exp_cnt, avg_tas]
)
set(
  x     = dt_cla,
  j     = "max_tas",
  value = dt_cla[, max_tas] - dt_cla[1:exp_cnt, max_tas]
)
set(
  x     = dt_cla,
  j     = "avg_rho",
  value = dt_cla[, avg_rho] - dt_cla[1:exp_cnt, avg_rho]
)
set(
  x     = dt_cla,
  j     = "min_rho",
  value = dt_cla[, min_rho] - dt_cla[1:exp_cnt, min_rho]
)
set(
  x     = dt_cla,
  j     = "avg_hdw",
  value = dt_cla[, avg_hdw] - dt_cla[1:exp_cnt, avg_hdw]
)

# Define a function to generate plots
fn_plot <- function(var1, var2) {
  (
    ggplot(
      data    = dt_cla,
      mapping = aes(x = year, y = dt_cla[[var1]], color = toupper(exp))
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

# Display the script execution time
Sys.time() - start_time

# EOF