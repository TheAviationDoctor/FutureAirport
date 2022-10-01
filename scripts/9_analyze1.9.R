# ==============================================================================
#    NAME: scripts/9_analyze.R
#   INPUT: Climate and takeoff data output by earlier scripts
# ACTIONS: Create summary tables in MySQL and associated plots
#  OUTPUT: Plot and summary data files saved to disk
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

# # Create the summary table (runtime: ~12 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#       tolower(dat$an1),
#     "(
#       year    YEAR,
#       exp     CHAR(6),
#       zone    CHAR(11),
#       icao    CHAR(4),
#       avg_tas FLOAT,
#       avg_rho FLOAT,
#       avg_hdw FLOAT,
#       max_tas FLOAT,
#       min_rho FLOAT
#     )
#     AS SELECT
#       year,
#       exp,
#       zone,
#       icao,
#       AVG(tas) AS avg_tas,
#       AVG(rho) AS avg_rho,
#       AVG(hdw) AS avg_hdw,
#       MAX(tas) AS max_tas,
#       MIN(rho) AS min_rho
#     FROM",
#       tolower(dat$cli),
#     "GROUP BY
#       year,
#       exp,
#       icao
#     ;",
#     sep = " "
#   )
# )
# 
# # Fetch the data
# dt_an1 <- fn_sql_qry(
#   statement = paste(
#     "SELECT
#       *
#     FROM",
#       tolower(dat$an1),
#     ";",
#     sep = " "
#   )
# )
# 
# # Recast column types
# set(x = dt_an1, j = "year", value = as.integer(dt_an1[, year]))
# set(x = dt_an1, j = "zone", value = as.factor(dt_an1[, zone]))
# set(x = dt_an1, j = "exp",  value = as.factor(dt_an1[, exp]))
# set(x = dt_an1, j = "icao", value = as.factor(dt_an1[, icao]))
# 
# # Convert near-surface temperatures from °K to °C for display
# dt_an1[, avg_tas := avg_tas - sim$k_to_c]
# dt_an1[, max_tas := max_tas - sim$k_to_c]
# 
# # Declare dependent variables of interest
# cols <- c("avg_tas", "avg_rho", "avg_hdw", "max_tas", "min_rho")
# 
# # Declare grouping of interest
# grp <- c("year", "exp", "zone")
# 
# # Summarize and combine the data by group
# dt_an1 <- rbind(
#   # Zonal summary
#   cbind(
#     dt_an1[, lapply(X = .SD, FUN = mean),
#       by = grp,
#       .SDcols = c("avg_tas", "avg_rho", "avg_hdw")
#     ],
#     dt_an1[, lapply(X = .SD, FUN = max),
#       by = grp,
#       .SDcols = c("max_tas")
#     ][, "max_tas"],
#     dt_an1[, lapply(X = .SD, FUN = min),
#       by = grp,
#       .SDcols = c("min_rho")
#     ][, "min_rho"]
#   ),
#   # Global summary
#   cbind(
#     dt_an1[, zone := "Global"][, lapply(X = .SD, FUN = mean),
#       by = grp,
#       .SDcols = c("avg_tas", "avg_rho", "avg_hdw")
#     ],
#     dt_an1[, zone := "Global"][, lapply(X = .SD, FUN = max),
#       by = grp,
#       .SDcols = c("max_tas")
#     ][, "max_tas"],
#     dt_an1[, zone := "Global"][, lapply(X = .SD, FUN = min),
#       by = grp,
#       .SDcols = c("min_rho")
#     ][, "min_rho"]
#   )
# )
# 
# # Update grouping of interest
# grp <- c("exp", "zone")
# 
# # Perform local polynomial regression fitting to dampen the volatility
# dt_an1[,
#   paste(cols, "loess", sep = "_") := lapply(
#     X = .SD,
#     FUN = function(x) {
#       predict(loess(formula = x ~ year, span = .75, model = TRUE))
#     }
#   ),
#   by = grp,
#   .SDcols = cols
# ]
# 
# # Add the first-year values by group
# dt_an1 <- dt_an1[dt_an1[, .SD[1:1], by = grp], on = grp][,
#   c(
#     "i.year",
#     "i.avg_tas",
#     "i.avg_rho",
#     "i.avg_hdw",
#     "i.max_tas",
#     "i.min_rho"
#   ) := NULL
# ]
# 
# # Calculate relative changes in dependent variables and remove initial values
# lapply(
#   X = cols,
#   FUN = function(x) {
#     dt_an1[, paste(x, "per", sep = "_") :=
#       (
#         get(paste(x, "loess", sep = "_")) -
#         get(paste("i.", x, "_loess", sep = ""))
#       ) /
#         get(paste("i.", x, "_loess", sep = ""))
#     ][, paste("i.", x, "_loess", sep = "") := NULL]
#   }
# )
# 
# # Create a function to plot results
# fn_plot <- function(cols) {
#   (
#     ggplot(
#       data    = dt_an1,
#       mapping = aes(
#         x     = year,
#         y     = dt_an1[[as.character(cols)]],
#         color = zone
#       )
#     ) +
#     geom_line(size = .2) +
#     geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#     # Starting value labels
#     geom_text(
#       data       = dt_an1[, .SD[which.min(year)], by = grp],
#       aes(
#         x        = year,
#         y        = dt_an1[, .SD[which.min(year)], by = grp]
#         [[paste(as.character(cols), "loess", sep = "_")]],
#         color    = zone,
#         label    = round(
#           x      = dt_an1[, .SD[which.min(year)], by = grp]
#           [[paste(as.character(cols), "loess", sep = "_")]],
#           digits = 2L
#         )
#       ),
#       size  = 2L,
#       vjust = -1L
#     ) +
#     # Ending value labels
#     geom_text(
#       data       = dt_an1[, .SD[which.max(year)], by = grp],
#       aes(
#         x        = year,
#         y        = dt_an1[, .SD[which.max(year)], by = grp]
#         [[paste(as.character(cols), "loess", sep = "_")]],
#         color    = zone,
#         label    = round(
#           x      = dt_an1[, .SD[which.max(year)], by = grp]
#           [[paste(as.character(cols), "loess", sep = "_")]],
#           digits = 2L
#         )
#       ),
#       size  = 2L,
#       vjust = -1L
#     ) +
#     scale_x_continuous(name = "Year") +
#     scale_y_continuous(name = "Value") +
#     facet_wrap(~toupper(exp), ncol = 2L) +
#     theme_light() +
#     theme(axis.title.y = element_blank())
#   ) |>
#   ggsave(
#     filename = tolower(paste("9_", cols, ".png", sep = "")),
#     device   = "png",
#     path     = "plots",
#     scale    = 1L,
#     width    = 6L,
#     height   = NA,
#     units    = "in",
#     dpi      = "print"
#   )
# }
# 
# # Generate the plots
# mapply(
#   FUN  = fn_plot,
#   cols = cols
# )
# 
# # Save the data to disk
# fwrite(
#   x    = dt_an1,
#   file = paste(dir$res, "dt_an1.csv", sep = "/")
# )

# ==============================================================================
# 2. Takeoff outcomes summary
# ==============================================================================

# Create the summary table (runtime: ~X minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
      tolower(dat$an2),
    "(
      exp            CHAR(6),
      zone           CHAR(11),
      icao           CHAR(4),
      type           CHAR(4),
      itr_avg        FLOAT,
      itr_sum        INT,
      tko_ok_thr_min MEDIUMINT,
      tko_ok_thr_mid MEDIUMINT,
      tko_ok_thr_max MEDIUMINT,
      tko_ok         MEDIUMINT,
      tko_ko         MEDIUMINT,
      tko            MEDIUMINT
    )
    AS SELECT
      exp,
      zone,
      icao,
      type,
      AVG(itr)                                         AS itr_avg,
      SUM(itr)                                         AS itr_sum,
      SUM(thr_red =", sim$thr_ini, ")                  AS tko_ok_thr_min,
      SUM(thr_red BETWEEN 1 AND", sim$thr_ini - 1L, ") AS tko_ok_thr_mid,
      SUM(thr_red = 0 AND todr <= toda)                AS tko_ok_thr_max,
      SUM(todr <= toda)                                AS tko_ok,
      SUM(todr > toda)                                 AS tko_ko,
      COUNT(*)                                         AS tko
    FROM",
      tolower(dat$tko),
    "GROUP BY
      exp,
      icao,
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
set(x = dt_an2, j = "zone",    value = as.factor(dt_an2[, zone]))
set(x = dt_an2, j = "exp",     value = as.factor(dt_an2[, exp]))
set(x = dt_an2, j = "icao",    value = as.factor(dt_an2[, icao]))
set(x = dt_an2, j = "type",    value = as.factor(dt_an2[, type]))
set(x = dt_an2, j = "itr_sum", value = as.numeric(dt_an2[, itr_sum]))

# Combine the aircraft types to narrow/widebody
levels(dt_an2$type) <- bod

# Declare dependent variables of interest
cols <- c(
  "itr_avg",
  "itr_sum",
  "tko_ok_thr_min",
  "tko_ok_thr_mid",
  "tko_ok_thr_max",
  "tko_ok",
  "tko_ko",
  "tko"
)

# Declare grouping of interest
grp <- c("exp", "zone", "type")

# Summarize and combine the data by group
dt_an2 <- rbind(
  # Zonal summary
  cbind(
    dt_an2[, lapply(X = .SD, FUN = sum),
      by = grp,
      .SDcols = c(
        "itr_sum",
        "tko_ok_thr_min",
        "tko_ok_thr_mid",
        "tko_ok_thr_max",
        "tko_ok",
        "tko_ko",
        "tko"
      )
    ],
    dt_an2[, lapply(X = .SD, FUN = mean),
      by = grp,
      .SDcols = c("itr_avg")
    ][, "itr_avg"]
  ),
  # Global summary
  cbind(
    dt_an2[, zone := "Global"][, lapply(X = .SD, FUN = sum),
      by = grp,
      .SDcols = c(
        "itr_sum",
        "tko_ok_thr_min",
        "tko_ok_thr_mid",
        "tko_ok_thr_max",
        "tko_ok",
        "tko_ko",
        "tko"
      )
    ],
    dt_an2[, zone := "Global"][, lapply(X = .SD, FUN = mean),
      by = grp,
      .SDcols = c("itr_avg")
    ][, "itr_avg"]
  )
)



# Save the data to disk
fwrite(
  x    = dt_an2,
  file = paste(dir$res, "dt_an2.csv", sep = "/")
)

# # ==============================================================================
# # 3. Research questions summary
# # ==============================================================================
# 
# # Create the summary table (runtime: ~60 minutes)
# fn_sql_qry(
#   statement = paste(
#     "CREATE TABLE IF NOT EXISTS",
#       tolower(dat$an3),
#     "(
#       year        YEAR,
#       exp         CHAR(6),
#       zone        CHAR(11),
#       icao        CHAR(4),
#       type        CHAR(4),
#       avg_todr    FLOAT,
#       avg_thr_red FLOAT,
#       avg_tom_red FLOAT
#     )
#     AS SELECT
#       year,
#       exp,
#       zone,
#       icao,
#       type,
#       AVG(todr)    AS avg_todr,
#       AVG(thr_red) AS avg_thr_red,
#       AVG(tom_red) AS avg_tom_red
#     FROM",
#       tolower(dat$tko),
#     "WHERE
#       todr <= toda
#     GROUP BY
#       year,
#       exp,
#       icao,
#       type
#     ;",
#     sep = " "
#   )
# )
# 
# # Fetch the data
# dt_an3 <- fn_sql_qry(
#   statement = paste(
#     "SELECT
#       *
#     FROM",
#       tolower(dat$an3),
#     ";",
#     sep = " "
#   )
# )
# 
# # Recast column types
# set(x = dt_an3, j = "year", value = as.integer(dt_an3[, year]))
# set(x = dt_an3, j = "zone", value = as.factor(dt_an3[, zone]))
# set(x = dt_an3, j = "exp",  value = as.factor(dt_an3[, exp]))
# set(x = dt_an3, j = "icao", value = as.factor(dt_an3[, icao]))
# set(x = dt_an3, j = "type", value = as.factor(dt_an3[, type]))
# 
# # Combine the aircraft types to narrow/widebody
# levels(dt_an3$type) <- bod
# 
# # Convert thrust reduction to thrust
# dt_an3[, avg_thr := (100L - avg_thr_red) / 100L][, avg_thr_red := NULL]
# 
# # Convert payload removal to passengers
# dt_an3[, avg_pax := avg_tom_red / sim$pax_avg][, avg_tom_red := NULL]
# 
# # Declare dependent variables of interest
# cols <- c("avg_todr", "avg_thr", "avg_pax")
# 
# # Declare grouping of interest
# grp <- c("year", "exp", "zone", "type")
# 
# # Summarize and combine the data by group
# dt_an3 <- rbind(
#   # Zonal summary
#   dt_an3[, lapply(X = .SD, FUN = mean),
#     by = grp,
#     .SDcols = cols
#   ],
#   # Global summary
#   dt_an3[, zone := "Global"][, lapply(X = .SD, FUN = mean),
#     by = grp,
#     .SDcols = cols
#   ]
# )
# 
# # Update grouping of interest
# grp <- c("exp", "zone", "type")
# 
# # Perform local polynomial regression fitting to dampen the volatility
# dt_an3[,
#   paste(cols, "loess", sep = "_") := lapply(
#     X = .SD,
#     FUN = function(x) {
#       predict(loess(formula = x ~ year, span = .75, model = TRUE))
#     }
#   ),
#   by = grp,
#   .SDcols = cols
# ]
# 
# # Add the first-year values by group
# dt_an3 <- dt_an3[dt_an3[, .SD[1:1], by = grp],
#  on = grp][, c("i.year", "i.avg_todr", "i.avg_thr", "i.avg_pax") := NULL]
# 
# # Calculate relative changes in dependent variables and remove initial values
# lapply(
#   X = cols,
#   FUN = function(x) {
#     dt_an3[, paste(x, "per", sep = "_") :=
#       (
#         get(paste(x, "loess", sep = "_")) -
#         get(paste("i.", x, "_loess", sep = ""))
#       ) /
#         get(paste("i.", x, "_loess", sep = ""))
#     ][, paste("i.", x, "_loess", sep = "") := NULL]
#   }
# )
# 
# # Update grouping of interest
# grp <- c("exp", "zone")
# 
# # Create a function to plot results
# fn_plot <- function(body, cols) {
#   (
#     ggplot(
#       data    = dt_an3[type == body],
#       mapping = aes(
#         x     = year,
#         y     = dt_an3[type == body][[as.character(cols)]],
#         color = zone
#       )
#     ) +
#     geom_line(size = .2) +
#     geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#     # Starting value labels
#     geom_text(
#       data       = dt_an3[type == body][, .SD[which.min(year)], by = grp],
#       aes(
#         x        = year,
#         y        = dt_an3[type == body][, .SD[which.min(year)], by = grp]
#                     [[paste(as.character(cols), "loess", sep = "_")]],
#         color    = zone,
#         label    = round(
#           x      = dt_an3[type == body]
#                     [, .SD[which.min(year)], by = grp]
#                     [[paste(as.character(cols), "loess", sep = "_")]],
#           digits = 2L
#         )
#       ),
#       size  = 2L,
#       vjust = -1L
#     ) +
#     # Ending value labels
#     geom_text(
#       data       = dt_an3[type == body][, .SD[which.max(year)], by = grp],
#       aes(
#         x        = year,
#         y        = dt_an3[type == body][, .SD[which.max(year)], by = grp]
#                     [[paste(as.character(cols), "loess", sep = "_")]],
#         color    = zone,
#         label    = round(
#           x      = dt_an3[type == body]
#                     [, .SD[which.max(year)], by = grp]
#                     [[paste(as.character(cols), "loess", sep = "_")]],
#           digits = 2L
#         )
#       ),
#       size  = 2L,
#       vjust = -1L
#     ) +
#     scale_x_continuous(name = "Year") +
#     scale_y_continuous(name = "Value") +
#     facet_wrap(~toupper(exp), ncol = 2L) +
#     theme_light() +
#     theme(axis.title.y = element_blank())
#   ) |>
#   ggsave(
#     filename = tolower(paste("9_", body, "_", cols, ".png", sep = "")),
#     device   = "png",
#     path     = "plots",
#     scale    = 1L,
#     width    = 6L,
#     height   = NA,
#     units    = "in",
#     dpi      = "print"
#   )
# }
# 
# # Combine the aircraft bodies and dependent variables to be plotted
# mix <- expand.grid(
#   body = names(bod),
#   cols = cols
# )
# 
# # Generate the plots
# mapply(
#   FUN  = fn_plot,
#   body = mix$body,
#   cols = mix$cols
# )
# 
# # Save the data to disk
# fwrite(
#   x    = dt_an3,
#   file = paste(dir$res, "dt_an3.csv", sep = "/")
# )

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Stop the script timer
Sys.time() - start_time

# EOF