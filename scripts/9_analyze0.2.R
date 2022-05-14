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

# ==============================================================================
# 1 Import the takeoff performance simulation data
# ==============================================================================

# ==============================================================================
# 1.1 Unique takeoffs
# Takes 4.7 minutes
# ==============================================================================

# Fetch the data
dt_uni <- fn_dat_sql(
  select = "COUNT(*) AS count",
  from   = dat$tko,
  where  = NULL,
  group  = NULL,
  order  = NULL
)

# Output results to the console
print(paste("Count of unique takeoffs:", format(dt_uni, big.mark = ",")))

# ==============================================================================
# 1.2 Takeoff iterations
# ==============================================================================

# Fetch the data
dt_itr <- fn_dat_sql(
  select = "SUM(itr) AS sum, AVG(itr) as avg",
  from   = dat$tst,
  where  = NULL,
  group  = NULL,
  order  = NULL
)

# Output results to the console
print(paste("Sum:", format(dt_itr[, sum], big.mark = ",")))
print(paste("Average:", format(dt_itr[, avg], big.mark = ",")))

# ==============================================================================
# 1.3 Takeoff outcomes
# ==============================================================================

# Fetch the data
dt_uns <- fn_dat_sql(
  select = "YEAR(obs) AS year, exp AS exp, COUNT(*) AS count",
  from   = dat$tst,
  where  = "todr > toda",
  group  = "year, exp",
  order  = NULL
)

# Output results to the console
print(paste("Unsuccessful:", format(length(dt_uns), big.mark = ",")))
print(paste("Successful:", format(dt_uni - length(dt_uns), big.mark = ",")))

# Set y-axis limits
lim <- c(1130E3, 1195E3)

# Create and save the plot
(ggplot(data = dt_uns) +
    geom_line(mapping = aes(x = year, y = count), color = "black", size = 1) +
    scale_x_continuous("Year") +
    scale_y_continuous("Count", labels = scales::comma, limits = lim) +
    facet_wrap(~toupper(exp), ncol = 2) +
    theme_light()) %>%
  ggsave(
    filename = "9_line_uns_to_cnt.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# Add a column for percentage
set(x = dt_uns, j = "per", value = dt_uns[, count] / dt_uni * 100)

# Create and save the plot
(ggplot(data = dt_uns) +
    geom_line(mapping = aes(x = year, y = per), color = "black", size = 1) +
    scale_x_continuous("Year") +
    scale_y_continuous("Count", labels = scales::percent) +
    facet_wrap(~toupper(exp), ncol = 2) +
    theme_light()) %>%
  ggsave(
    filename = "9_line_uns_to_per.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# ==============================================================================
# 1.4 Takeoff thrust reduction
# ==============================================================================

# Fetch the data
dt_thr <- fn_dat_sql(
  select = "YEAR(obs) AS year, exp AS exp, AVG(thr_red) AS avg",
  from   = dat$tst,
  where  = NULL,
  group  = "year, exp",
  order  = NULL
)

# Output results to the console
print(paste("Thrust reduction:", format(dt_thr[, avg], big.mark = ",")))

# Create and save the plot
(ggplot(data = dt_uns) +
    geom_line(mapping = aes(x = year, y = count), color = "black", size = 1) +
    scale_x_continuous("Year") +
    scale_y_continuous("Percent", labels = scales::percent) +
    facet_wrap(~toupper(exp), ncol = 2) +
    theme_light()) %>%
  ggsave(
    filename = "9_line_thr_red.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )
# ==============================================================================
# 1.5 Takeoff payload removal
# ==============================================================================

# Fetch the data
dt_pax <- fn_dat_sql(
  select = "YEAR(obs) AS year, exp AS exp, SUM(tom_red) AS sum, AVG(tom_red) AS avg",
  from   = dat$tst,
  where  = NULL,
  group  = "year, exp",
  order  = NULL
)

# Output results to the console
print(paste("Sum of payload removal:", format(dt_pax[, sum], big.mark = ",")))
print(paste("Average payload removal:", format(dt_pax[, avg], big.mark = ",")))

# Create and save the plot
(ggplot(data = dt_uns) +
    geom_line(mapping = aes(x = year, y = count), color = "black", size = 1) +
    scale_x_continuous("Year") +
    scale_y_continuous("Count", labels = scales::percent) +
    facet_wrap(~toupper(exp), ncol = 2) +
    theme_light()) %>%
  ggsave(
    filename = "9_line_tom_red.png",
    device = "png",
    path = "plots",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print"
  )

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Display the script execution time
Sys.time() - start_time

# EOF