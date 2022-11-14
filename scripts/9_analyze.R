# ==============================================================================
#    NAME: scripts/9_analyze.R
#   INPUT: Climate and takeoff data output by earlier scripts
# ACTIONS: Create summary tables in MySQL and associated plots
#  OUTPUT: Plot and summary data files saved to disk
# RUNTIME: ~160 minutes if the summary tables do not yet exist in the database.
#          < 30 seconds otherwise. (3.8 GHz CPU / 128 GB DDR4 RAM / SSD)
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
library(viridis)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# ==============================================================================
# 1. Climate change summary
# ==============================================================================

# ==============================================================================
# 1.1 Create, fetch, and cleanse the data. Variables:
# avg_tas  = Mean air temperature in °C
# avg_hurs = Mean relative humidity in %
# avg_ps   = Mean air pressure in Pa
# avg_rho  = Mean air density in kg/m³
# avg_hdw  = Mean headwind in m/s
# max_tas  = Maximum air temperature in °C
# min_rho  = Minimum air density in kg/m³
# ==============================================================================

# Create the summary table (runtime: ~13 minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
    tolower(dat$an1),
    "(
      year     YEAR,
      ssp      CHAR(6),
      zone     CHAR(11),
      icao     CHAR(4),
      lat      FLOAT,
      lon      FLOAT,
      avg_tas  FLOAT,
      avg_hurs FLOAT,
      avg_ps   FLOAT,
      avg_rho  FLOAT,
      avg_hdw  FLOAT,
      max_tas  FLOAT,
      min_rho  FLOAT
    )
    AS SELECT
      year,
      ssp,
      zone,
      icao,
      lat,
      lon,
      AVG(tas)  AS avg_tas,
      AVG(hurs) AS avg_hurs,
      AVG(ps)   AS avg_ps,
      AVG(rho2) AS avg_rho,
      AVG(hdw)  AS avg_hdw,
      MAX(tas)  AS max_tas,
      MIN(rho2) AS min_rho
    FROM",
    tolower(dat$cli),
    "GROUP BY
      year,
      ssp,
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
set(x = dt_an1, j = "zone", value = as.factor(dt_an1[, zone]))
set(x = dt_an1, j = "ssp",  value = as.factor(dt_an1[, ssp]))
set(x = dt_an1, j = "icao", value = as.factor(dt_an1[, icao]))

# ==============================================================================
# 1.2 Treat output variables of interest
# ==============================================================================

# Convert near-surface air temperature from °K to °C
dt_an1[, avg_tas := avg_tas - sim$k_to_c]
dt_an1[, max_tas := max_tas - sim$k_to_c]

# Convert near-surface air pressure from Pa to hPa
dt_an1[, avg_ps := avg_ps / 100L]

# ==============================================================================
# 1.3 Summarize the data
# ==============================================================================

# Declare output variables for averaging
cols_mean <- c("avg_tas", "avg_hurs", "avg_ps", "avg_rho", "avg_hdw")

# Declare input variables for grouping
grp <- c("year", "ssp", "zone")

# Summarize the data and combine them by group
dt_an1a <- rbind(
  # Zonal summary by group
  cbind(
    dt_an1[, lapply(X = .SD, FUN = mean),
      by = grp,
      .SDcols = cols_mean
    ],
    dt_an1[, lapply(X = .SD, FUN = max),
      by = grp,
      .SDcols = c("max_tas")
    ][, "max_tas"],
    dt_an1[, lapply(X = .SD, FUN = min),
      by = grp,
      .SDcols = c("min_rho")
    ][, "min_rho"]
  ),
  # Global summary by group
  cbind(
    dt_an1[, zone := "Global"][, lapply(X = .SD, FUN = mean),
      by = grp,
      .SDcols = cols_mean
    ],
    dt_an1[, zone := "Global"][, lapply(X = .SD, FUN = max),
      by = grp,
      .SDcols = c("max_tas")
    ][, "max_tas"],
    dt_an1[, zone := "Global"][, lapply(X = .SD, FUN = min),
      by = grp,
      .SDcols = c("min_rho")
    ][, "min_rho"]
  )
)

# ==============================================================================
# 1.4 Add local polynomial regression fitting (LOESS) to the output variables
# ==============================================================================

# Declare output variables for regression fitting
cols <- c(
  "avg_tas",
  "avg_hurs",
  "avg_ps",
  "avg_rho",
  "avg_hdw",
  "max_tas",
  "min_rho"
)

# Declare input variables for grouping
grp <- c("ssp", "zone")

# Perform regression fitting by group
dt_an1a[,
  paste(cols, "loess", sep = "_") := lapply(
    X = .SD,
    FUN = function(x) {
      predict(loess(formula = x ~ year, span = .75, model = TRUE))
    }
  ),
  by = grp,
  .SDcols = cols
]

# ==============================================================================
# 1.5 Save the data to disk
# ==============================================================================

fwrite(
  x    = dt_an1a,
  file = paste(dir$res, "dt_an1a.csv", sep = "/")
)

# # ==============================================================================
# # 1.6 Plot the results
# # ==============================================================================
# 
# # Create a function to plot results
# fn_plot <- function(cols) {
#   (
#     ggplot(
#       data = dt_an1a,
#       mapping = aes(
#         x     = year,
#         y     = dt_an1a[[as.character(cols)]]
#       )
#     ) +
#       geom_line(size = .2) +
#       geom_smooth(formula = y ~ x, method = "loess", size = .5) +
#       # Starting value labels
#       geom_label(
#         data = dt_an1a[, .SD[which.min(year)], by = grp],
#         aes(
#           x = year,
#           y = dt_an1a[, .SD[which.min(year)], by = grp]
#           [[paste(as.character(cols), "loess", sep = "_")]],
#           label = round(
#             x = dt_an1a[, .SD[which.min(year)], by = grp]
#             [[paste(as.character(cols), "loess", sep = "_")]],
#             digits = 2L
#           )
#         ),
#         alpha      = .5,
#         fill       = "white",
#         label.r    = unit(0L, "lines"),
#         label.size = 0L,
#         nudge_x    = 4L,
#         size       = 2L
#       ) +
#       # Ending value labels
#       geom_label(
#         data = dt_an1a[, .SD[which.max(year)], by = grp],
#         aes(
#           x = year,
#           y = dt_an1a[, .SD[which.max(year)], by = grp]
#           [[paste(as.character(cols), "loess", sep = "_")]],
#           label = round(
#             x = dt_an1a[, .SD[which.max(year)], by = grp]
#             [[paste(as.character(cols), "loess", sep = "_")]],
#             digits = 2L
#           )
#         ),
#         alpha      = .5,
#         fill       = "white",
#         label.r    = unit(0L, "lines"),
#         label.size = 0L,
#         nudge_x    = -4L,
#         size       = 2L
#       ) +
#       scale_x_continuous(name = "Year", n.breaks = 5L) +
#       scale_y_continuous(name = "Value", labels = label_comma(accuracy = .01)) +
#       facet_grid(zone ~ toupper(ssp), scales = "free_y") +
#       theme_light() +
#       theme(
#         axis.title.y = element_blank(),
#         text = element_text(size = 8)
#       )
#   ) |>
#     ggsave(
#       filename = tolower(paste("9_", cols, ".png", sep = "")),
#       device   = "png",
#       path     = "plots",
#       scale    = 1L,
#       width    = 6L,
#       height   = NA,
#       units    = "in",
#       dpi      = "print"
#     )
# }
# 
# # Generate the plots
# mapply(
#   FUN  = fn_plot,
#   cols = cols
# )



dt_an1b <- dt_an1[year %in% c(2015, 2100)]

# Pivot the dataset from long to wide format
dt_an1b <- dcast.data.table(
  data      = dt_an1b,
  formula   = icao + zone + ssp ~ year,
  value.var = cols
)

# Express output variables as a percentage of all takeoffs
lapply(
  X = cols,
  FUN = function(x) {
    dt_an1b[, paste(x) := get(paste(x, "2100", sep = "_")) - get(paste(x, "2015", sep = "_"))]
  }
)

View(dt_an1b)
stop()

# Define the world object from the Natural Earth package
world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

# Build a world map and plot the airports
(ggplot() +
    geom_sf(data = world, color = NA, fill = "gray", alpha = .5) +
    scale_y_continuous(breaks = unique(unlist(geo, use.names = FALSE))) +
    coord_sf(expand = FALSE) +
    scale_color_viridis() +
    geom_point(
      data     = dt_an1b[!duplicated(dt_an1b$icao), ],
      mapping  = aes(x = lon, y = lat, color = avg_rho),
      # color    = "black",
      shape    = 20L,
      size     = 1.5
    ) +
    # geom_hline(
    #   aes(yintercept = dt_pop$lat[which.max(dt_pop$lat)]),
    #   color    = "black"
    # ) +
    # geom_hline(
    #   aes(yintercept = df_smp$lat[which.max(df_smp$lat)]),
    #   color    = "purple"
    # ) +
    # geom_hline(
    #   aes(yintercept = dt_pop$lat[which.min(dt_pop$lat)]),
    #   color    = "black"
    # ) +
    # geom_hline(
    #   aes(yintercept = df_smp$lat[which.min(df_smp$lat)]),
    #   color    = "purple"
    # ) +
    # geom_hline(
    #   aes(yintercept = mean(dt_pop[!duplicated(dt_pop$icao), ]$lat)),
    #   color    = "black"
    # ) +
    # geom_hline(
    #   aes(yintercept = mean(df_smp[!duplicated(df_smp$icao), ]$lat)),
    #   color    = "purple"
    # ) +
    # geom_hline(
    #   aes(yintercept = median(dt_pop[!duplicated(dt_pop$icao), ]$lat)),
    #   color    = "black"
    # ) +
    # geom_hline(
    #   aes(yintercept = median(df_smp[!duplicated(df_smp$icao), ]$lat)),
    #   color    = "purple"
    # ) +
    # geom_hline(
    #   aes(yintercept = 66.5635),
    #   color    = "gray",
    #   linetype = "dashed"
    # ) +
    # geom_hline(
    #   aes(yintercept = 30),
    #   color    = "gray",
    #   linetype = "dashed"
    # ) +
    # geom_hline(
    #   aes(yintercept = 23.4365),
    #   color    = "gray",
    #   linetype = "dashed"
    # ) +
    # geom_hline(
    #   aes(yintercept = -23.4365),
    #   color    = "gray",
    #   linetype = "dashed"
    # ) +
    # geom_hline(
    #   aes(yintercept = -30),
    #   color    = "gray",
    #   linetype = "dashed"
    # ) +
    # geom_hline(
    #   aes(yintercept = -66.5635),
    #   color    = "gray",
    #   linetype = "dashed"
    # ) +
    theme_light() +
    theme(axis.title = element_blank(), axis.text.x = element_blank())) |>
  ggsave(
    filename = "9_world.png",
    device   = "png",
    path     = "plots",
    scale    = 1L,
    width    = 8.15,
    height   = 3.69,
    units    = "in",
    dpi      = "print"
  )

stop()

# ==============================================================================
# 2. Takeoff outcomes summary
# ==============================================================================

# ==============================================================================
# 2.1 Create, fetch, and cleanse the data. Variables:
# itr_avg              = Average count of iterations per takeoff
# itr_sum              = Count of all iterations performed
# tko_ok_thr_min       = Count of takeoffs performed using 75% TOGA
# tko_ok_thr_mid       = Count of takeoffs performed using ]75%-100%[ TOGA
# tko_ok_thr_max_no_rm = Count of takeoffs performed using 100% TOGA and no
#                         payload removal
# tko_ok_thr_max_rm    = Count of takeoffs performed using 100% TOGA and
#                         payload removal not exceeding the BELF
# tko_ok_thr_max       = Count of all takeoffs performed using 100% TOGA
#                         (with or without payload removal)
# tko_ok               = Count of all successful takeoffs
#                         (regardless of thrust and payload removal)
# tko_ok               = Count of all unsuccessful takeoffs
#                         (despite 100% TOGA and payload removal down to BELF)
# tko                  = Count of all takeoffs (whether successful or not)
# ==============================================================================

# Create the summary table (runtime: ~90 minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
    tolower(dat$an2),
    "(
      year                 YEAR,
      ssp                  CHAR(6),
      zone                 CHAR(11),
      icao                 CHAR(4),
      type                 CHAR(4),
      itr_avg              FLOAT,
      itr_sum              INT,
      tko_ok_thr_min       MEDIUMINT,
      tko_ok_thr_mid       MEDIUMINT,
      tko_ok_thr_max_no_rm MEDIUMINT,
      tko_ok_thr_max_rm    MEDIUMINT,
      tko_ok_thr_max       MEDIUMINT,
      tko_ok               MEDIUMINT,
      tko_ko               MEDIUMINT,
      tko                  MEDIUMINT
    )
    AS SELECT
      year,
      ssp,
      zone,
      icao,
      type,
      AVG(itr)                                          AS itr_avg,
      SUM(itr)                                          AS itr_sum,
      SUM(thr_red =", sim$thr_ini, ")                   AS tko_ok_thr_min,
      SUM(thr_red BETWEEN 1 AND", sim$thr_ini - 1L, ")  AS tko_ok_thr_mid,
      SUM(thr_red = 0 AND todr <= toda AND tom_rem = 0) AS tko_ok_thr_max_no_rm,
      SUM(thr_red = 0 AND todr <= toda AND tom_rem > 0) AS tko_ok_thr_max_rm,
      SUM(thr_red = 0 AND todr <= toda)                 AS tko_ok_thr_max,
      SUM(todr <= toda)                                 AS tko_ok,
      SUM(todr > toda)                                  AS tko_ko,
      COUNT(*)                                          AS tko
    FROM",
    tolower(dat$tko),
    "GROUP BY
      year,
      ssp,
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
set(x = dt_an2, j = "year",    value = as.integer(dt_an2[, year]))
set(x = dt_an2, j = "zone",    value = as.factor(dt_an2[, zone]))
set(x = dt_an2, j = "ssp",     value = as.factor(dt_an2[, ssp]))
set(x = dt_an2, j = "icao",    value = as.factor(dt_an2[, icao]))
set(x = dt_an2, j = "type",    value = as.factor(dt_an2[, type]))
set(x = dt_an2, j = "itr_sum", value = as.numeric(dt_an2[, itr_sum]))

# Combine the aircraft types to narrow/widebody
levels(dt_an2$type) <- bod

# ==============================================================================
# 2.2 Treat output variables of interest
# ==============================================================================

# Declare output variables for conversion from absolute to relative
cols <- c(
  "tko_ok_thr_min",
  "tko_ok_thr_mid",
  "tko_ok_thr_max_no_rm",
  "tko_ok_thr_max_rm",
  "tko_ok_thr_max",
  "tko_ok",
  "tko_ko",
  "tko"
)

# Express output variables as a percentage of all takeoffs
lapply(
  X = cols,
  FUN = function(x) {
    dt_an2[, paste(x, "rel", sep = "_") := get(x) / tko]
  }
)

# ==============================================================================
# 2.3 Summarize the data
# ==============================================================================

# Declare output variables for summation
cols_sum <- c(
  "itr_sum",
  "tko_ok_thr_min",
  "tko_ok_thr_mid",
  "tko_ok_thr_max_no_rm",
  "tko_ok_thr_max_rm",
  "tko_ok_thr_max",
  "tko_ok",
  "tko_ko",
  "tko"
)

# Declare output variables for averaging
cols_mean <- c(
  "itr_avg",
  "tko_ok_thr_min_rel",
  "tko_ok_thr_mid_rel",
  "tko_ok_thr_max_no_rm_rel",
  "tko_ok_thr_max_rm_rel",
  "tko_ok_thr_max_rel",
  "tko_ok_rel",
  "tko_ko_rel",
  "tko_rel"
)

# Declare input variables for grouping
grp <- c("year", "ssp", "zone", "type")

# Summarize the data and combine them by group
dt_an2 <- rbind(
  # Zonal summary by group
  cbind(
    dt_an2[, lapply(X = .SD, FUN = sum),
      by = grp,
      .SDcols = cols_sum
    ],
    dt_an2[, lapply(X = .SD, FUN = mean),
      by = grp,
      .SDcols = cols_mean
    ][, ..cols_mean]
  ),
  # Global summary by group
  cbind(
    dt_an2[, zone := "Global"][, lapply(X = .SD, FUN = sum),
      by = grp,
      .SDcols = cols_sum
    ],
    dt_an2[, zone := "Global"][, lapply(X = .SD, FUN = mean),
      by = grp,
      .SDcols = cols_mean
    ][, ..cols_mean]
  )
)

# ==============================================================================
# 2.4 Add local polynomial regression fitting (LOESS) to the output variables
# ==============================================================================

# Declare output variables for regression fitting
cols <- c(
  "tko_ok_thr_min_rel",
  "tko_ok_thr_mid_rel",
  "tko_ok_thr_max_no_rm_rel",
  "tko_ok_thr_max_rm_rel",
  "tko_ok_thr_max_rel",
  "tko_ok_rel",
  "tko_ko_rel",
  "tko_rel"
)

# Declare input variables for grouping
grp <- c("ssp", "zone", "type")

# Perform regression fitting by group
dt_an2[,
  paste(cols, "loess", sep = "_") := lapply(
    X = .SD,
    FUN = function(x) {
      predict(loess(formula = x ~ year, span = .75, model = TRUE))
    }
  ),
  by = grp,
  .SDcols = cols
]

# ==============================================================================
# 2.5 Save the data to disk
# ==============================================================================

fwrite(
  x    = dt_an2,
  file = paste(dir$res, "dt_an2.csv", sep = "/")
)

# ==============================================================================
# 2.6 Plot the results
# ==============================================================================

# Update output variables for plotting
cols <- c(
  "tko_ok_thr_min_rel",
  "tko_ko_rel"
)

# Update input variables for grouping
grp <- c("ssp", "zone")

# Create a function to plot results
fn_plot <- function(body, cols) {
  (
    ggplot(
      data = dt_an2[type == body],
      mapping = aes(
        x     = year,
        y     = dt_an2[type == body][[as.character(cols)]]
      )
    ) +
      geom_line(size = .2) +
      geom_smooth(formula = y ~ x, method = "loess", size = .5) +
      # Starting value labels
      geom_label(
        data = dt_an2[type == body][, .SD[which.min(year)], by = grp],
        aes(
          x = year,
          y = dt_an2[type == body][, .SD[which.min(year)], by = grp]
          [[paste(as.character(cols), "loess", sep = "_")]],
          label = sprintf(fmt = "%1.1f%%", dt_an2[type == body]
          [, .SD[which.min(year)], by = grp]
          [[paste(as.character(cols), "loess", sep = "_")]] * 100L)
        ),
        alpha      = .5,
        fill       = "white",
        label.r    = unit(0L, "lines"),
        label.size = 0L,
        nudge_x    = 4L,
        size       = 2L
      ) +
      # Ending value labels
      geom_label(
        data = dt_an2[type == body][, .SD[which.max(year)], by = grp],
        aes(
          x = year,
          y = dt_an2[type == body][, .SD[which.max(year)], by = grp]
          [[paste(as.character(cols), "loess", sep = "_")]],
          label = sprintf(fmt = "%1.1f%%", dt_an2[type == body]
          [, .SD[which.max(year)], by = grp]
          [[paste(as.character(cols), "loess", sep = "_")]] * 100L)
        ),
        alpha      = .5,
        fill       = "white",
        label.r    = unit(0L, "lines"),
        label.size = 0L,
        nudge_x    = -4L,
        size       = 2L
      ) +
      scale_x_continuous(name = "Year", n.breaks = 3L) +
      scale_y_continuous(name = "Value", labels = scales::percent) +
      facet_grid(zone ~ toupper(ssp), scales = "free_y") +
      theme_light() +
      theme(
        axis.title.y = element_blank(),
        text = element_text(size = 8)
      )
  ) |>
    ggsave(
      filename = tolower(paste("9_", body, "_", cols, ".png", sep = "")),
      device   = "png",
      path     = "plots",
      scale    = 1L,
      width    = 6L,
      height   = NA,
      units    = "in",
      dpi      = "print"
    )
}

# Combine the aircraft bodies and output variables to be plotted
mix <- expand.grid(
  body = names(bod),
  cols = cols
)

# Generate the plots
mapply(
  FUN  = fn_plot,
  body = mix$body,
  cols = mix$cols
)

# ==============================================================================
# 3. Research questions summary
# ==============================================================================

# ==============================================================================
# 3.1 Create, fetch, and cleanse the data. Variables:
# avg_todr    = Mean takeoff distance required in m
# avg_thr_red = Mean thrust reduction in percentage points of TOGA
# avg_tom_rem = Mean takeoff mass reduction in kg
# ==============================================================================

# Create the summary table (runtime: ~60 minutes)
fn_sql_qry(
  statement = paste(
    "CREATE TABLE IF NOT EXISTS",
    tolower(dat$an3),
    "(
      year        YEAR,
      ssp         CHAR(6),
      zone        CHAR(11),
      icao        CHAR(4),
      type        CHAR(4),
      avg_todr    FLOAT,
      avg_thr_red FLOAT,
      avg_tom_rem FLOAT
    )
    AS SELECT
      year,
      ssp,
      zone,
      icao,
      type,
      AVG(todr)    AS avg_todr,
      AVG(thr_red) AS avg_thr_red,
      AVG(tom_rem) AS avg_tom_rem
    FROM",
    tolower(dat$tko),
    "WHERE
      todr <= toda
    GROUP BY
      year,
      ssp,
      icao,
      type
    ;",
    sep = " "
  )
)

# Fetch the data
dt_an3 <- fn_sql_qry(
  statement = paste(
    "SELECT
      *
    FROM",
    tolower(dat$an3),
    ";",
    sep = " "
  )
)

# Recast column types
set(x = dt_an3, j = "year", value = as.integer(dt_an3[, year]))
set(x = dt_an3, j = "zone", value = as.factor(dt_an3[, zone]))
set(x = dt_an3, j = "ssp",  value = as.factor(dt_an3[, ssp]))
set(x = dt_an3, j = "icao", value = as.factor(dt_an3[, icao]))
set(x = dt_an3, j = "type", value = as.factor(dt_an3[, type]))

# Combine the aircraft types to narrow/widebody
levels(dt_an3$type) <- bod

# ==============================================================================
# 3.2 Treat output variables of interest
# ==============================================================================

# Convert thrust reduction below TOGA to thrust as a percentage of TOGA
dt_an3[, avg_thr := (100L - avg_thr_red) / 100L][, avg_thr_red := NULL]

# Convert payload removal in kg to passengers based on standard assumptions
dt_an3[, avg_pax_rem := avg_tom_rem / sim$pax_avg][, avg_tom_rem := NULL]

# ==============================================================================
# 3.3 Summarize the data
# ==============================================================================

# Declare output variables for averaging
cols <- c("avg_todr", "avg_thr", "avg_pax_rem")

# Declare input variables for grouping
grp <- c("year", "ssp", "zone", "type")

# Summarize the data and combine them by group
dt_an3 <- rbind(
  # Zonal summary by group
  dt_an3[, lapply(X = .SD, FUN = mean),
    by = grp,
    .SDcols = cols
  ],
  # Global summary by group
  dt_an3[, zone := "Global"][, lapply(X = .SD, FUN = mean),
    by = grp,
    .SDcols = cols
  ]
)

# ==============================================================================
# 3.4 Add local polynomial regression fitting (LOESS) to the output variables
# ==============================================================================

# Declare input variables for grouping
grp <- c("ssp", "zone", "type")

# Perform regression fitting by group
dt_an3[,
  paste(cols, "loess", sep = "_") := lapply(
    X = .SD,
    FUN = function(x) {
      predict(loess(formula = x ~ year, span = .75, model = TRUE))
    }
  ),
  by = grp,
  .SDcols = cols
]

# ==============================================================================
# 3.5 # Save the data to disk
# ==============================================================================

fwrite(
  x    = dt_an3,
  file = paste(dir$res, "dt_an3.csv", sep = "/")
)

# ==============================================================================
# 3.6 Plot the results
# ==============================================================================

# Declare input variables for grouping
grp <- c("ssp", "zone")

# Create a function to plot results
fn_plot <- function(body, cols) {
  (
    ggplot(
      data = dt_an3[type == body],
      mapping = aes(
        x     = year,
        y     = dt_an3[type == body][[as.character(cols)]]
      )
    ) +
      geom_line(size = .2) +
      geom_smooth(formula = y ~ x, method = "loess", size = .5) +
      # Starting value labels
      geom_label(
        data = dt_an3[type == body][, .SD[which.min(year)], by = grp],
        aes(
          x = year,
          y = dt_an3[type == body][, .SD[which.min(year)], by = grp]
          [[paste(as.character(cols), "loess", sep = "_")]],
          label = round(
            x = dt_an3[type == body]
            [, .SD[which.min(year)], by = grp]
            [[paste(as.character(cols), "loess", sep = "_")]],
            digits = 2L
          )
        ),
        alpha      = .5,
        fill       = "white",
        label.r    = unit(0L, "lines"),
        label.size = 0L,
        nudge_x    = 6L,
        size       = 2L
      ) +
      # Ending value labels
      geom_label(
        data = dt_an3[type == body][, .SD[which.max(year)], by = grp],
        aes(
          x = year,
          y = dt_an3[type == body][, .SD[which.max(year)], by = grp]
          [[paste(as.character(cols), "loess", sep = "_")]],
          label = round(
            x = dt_an3[type == body]
            [, .SD[which.max(year)], by = grp]
            [[paste(as.character(cols), "loess", sep = "_")]],
            digits = 2L
          )
        ),
        alpha      = .5,
        fill       = "white",
        label.r    = unit(0L, "lines"),
        label.size = 0L,
        nudge_x    = -6L,
        size       = 2L
      ) +
      facet_grid(zone ~ toupper(ssp), scales = "free_y") +
      theme_light() +
      theme(
        axis.title.y = element_blank(),
        text = element_text(size = 8)
      )
  ) |>
    ggsave(
      filename = tolower(paste("9_", body, "_", cols, ".png", sep = "")),
      device   = "png",
      path     = "plots",
      scale    = 1L,
      width    = 6L,
      height   = NA,
      units    = "in",
      dpi      = "print"
    )
}

# Combine the aircraft bodies and output variables to be plotted
mix <- expand.grid(
  body = names(bod),
  cols = cols
)

# Generate the plots
mapply(
  FUN  = fn_plot,
  body = mix$body,
  cols = mix$cols
)

# ==============================================================================
# 6 Housekeeping
# ==============================================================================

# Stop the script timer
Sys.time() - start_time

# EOF