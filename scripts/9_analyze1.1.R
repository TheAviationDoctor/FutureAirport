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
# 1.1 Create, fetch, and cleanse the climate data. Variables:
# avg_tas  = Mean air temperature in °C
# avg_hurs = Mean relative humidity in %
# avg_ps   = Mean air pressure in Pa
# avg_rho  = Mean air density in kg/m³
# avg_hdw  = Mean headwind in m/s
# max_tas  = Maximum air temperature in °C
# min_rho  = Minimum air density in kg/m³
# ==============================================================================

# Create the summary table (runtime: ~15 minutes)
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
      max_tas  FLOAT,
      max_hurs FLOAT,
      max_ps   FLOAT,
      max_rho  FLOAT,
      max_hdw  FLOAT,
      avg_tas  FLOAT,
      avg_hurs FLOAT,
      avg_ps   FLOAT,
      avg_rho  FLOAT,
      avg_hdw  FLOAT,
      min_tas  FLOAT,
      min_hurs FLOAT,
      min_ps   FLOAT,
      min_rho  FLOAT,
      min_hdw  FLOAT
    )
    AS SELECT
      year,
      ssp,
      zone,
      icao,
      lat,
      lon,
      MAX(tas)      AS max_tas,
      MAX(hurs_cap) AS max_hurs,
      MAX(ps)       AS max_ps,
      MAX(rho2)     AS max_rho,
      MAX(hdw)      AS max_hdw,
      AVG(tas)      AS avg_tas,
      AVG(hurs_cap) AS avg_hurs,
      AVG(ps)       AS avg_ps,
      AVG(rho2)     AS avg_rho,
      AVG(hdw)      AS avg_hdw,
      MIN(tas)      AS min_tas,
      MIN(hurs_cap) AS min_hurs,
      MIN(ps)       AS min_ps,
      MIN(rho2)     AS min_rho,
      MIN(hdw)      AS min_hdw
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
dt_cli <- fn_sql_qry(
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
set(x = dt_cli, j = "year", value = as.integer(dt_cli[, year]))
set(x = dt_cli, j = "zone", value = as.factor(dt_cli[, zone]))
set(x = dt_cli, j = "ssp",  value = as.factor(dt_cli[, ssp]))
set(x = dt_cli, j = "icao", value = as.factor(dt_cli[, icao]))

# Convert temperatures from °K to °C
cols <- c("max_tas", "avg_tas", "min_tas")
dt_cli[, (cols) := lapply(X = .SD, FUN = "-", sim$k_to_c), .SDcols = cols]

# Convert near-surface air pressure from Pa to hPa
cols <- c("max_ps", "avg_ps", "min_ps")
dt_cli[, (cols) := lapply(X = .SD, FUN = "/", 10^2), .SDcols = cols]

# Convert near-surface air density from kg/m³ to g/m³
cols <- c("max_rho", "avg_rho", "min_rho")
dt_cli[, (cols) := lapply(X = .SD, FUN = "*", 10^3), .SDcols = cols]

# Recode frigid airports to temperate
dt_cli[zone == "Frigid", zone := "Temperate"]

# Save the data to disk
fwrite(
  x    = dt_cli,
  file = paste(dir$res, "dt_cli_unsummarized.csv", sep = "/")
)

# ==============================================================================
# 1.2 Summarize climate change by zone
# ==============================================================================

# ==============================================================================
# 1.2.1 Summarize base values
# ==============================================================================

# Declare independent variables for grouping
grp <- c("year", "ssp", "zone")

# Declare columns
cols_max <- c("max_tas", "max_ps", "max_hurs", "max_rho", "max_hdw")
cols_avg <- c("avg_tas", "avg_ps", "avg_hurs", "avg_rho", "avg_hdw")
cols_min <- c("min_tas", "min_ps", "min_hurs", "min_rho", "min_hdw")
cols_all <- c(cols_max, cols_avg, cols_min)

# Summarize the data by group
dt_cli <- rbind(
  # Zonal summary
  cbind(
    # Maxima
    dt_cli[, lapply(X = .SD, FUN = max),
      by      = grp,
      .SDcols = cols_max
    ],
    # Means
    dt_cli[, lapply(X = .SD, FUN = mean),
      by      = grp,
      .SDcols = cols_avg
    ][, ..cols_avg],
    # Minima
    dt_cli[, lapply(X = .SD, FUN = min),
      by      = grp,
      .SDcols = cols_min
    ][, ..cols_min]
  ),
  # Global summary
  cbind(
    # Maxima
    dt_cli[, zone := "Global"][, lapply(X = .SD, FUN = max),
      by      = grp,
      .SDcols = cols_max
    ],
    # Means
    dt_cli[, zone := "Global"][, lapply(X = .SD, FUN = mean),
      by      = grp,
      .SDcols = cols_avg
    ][, ..cols_avg],
    # Minima
    dt_cli[, zone := "Global"][, lapply(X = .SD, FUN = min),
      by      = grp,
      .SDcols = cols_min
    ][, ..cols_min]
  )
)

# ==============================================================================
# 1.2.2 Calculate absolute changes in base values
# ==============================================================================

# Declare columns
cols_max_abs <- paste(cols_max, "abs", sep = "_")
cols_avg_abs <- paste(cols_avg, "abs", sep = "_")
cols_min_abs <- paste(cols_min, "abs", sep = "_")
cols_all_abs <- paste(cols_all, "abs", sep = "_")

# Calculate absolute changes in base values
dt_cli[,
  (cols_all_abs) := lapply(X = .SD, FUN = function(x) { (x - x[1:1]) }),
  by      = c("ssp", "zone"),
  .SDcols = cols_all
]

# ==============================================================================
# 1.2.3 Calculate relative changes in base values
# ==============================================================================

# Declare columns
cols_max_rel <- paste(cols_max, "rel", sep = "_")
cols_avg_rel <- paste(cols_avg, "rel", sep = "_")
cols_min_rel <- paste(cols_min, "rel", sep = "_")
cols_all_rel <- paste(cols_all, "rel", sep = "_")

# Calculate relative changes in base values
dt_cli[,
  (cols_all_rel) := lapply(X = .SD, FUN = function(x) {
    (x - x[1:1]) / x[1:1]
  }),
  by      = c("ssp", "zone"),
  .SDcols = cols_all
]

# ==============================================================================
# 1.2.2 Smooth out base values using LOESS
# ==============================================================================

# Declare columns
cols_max_loe <- paste(cols_max, "loe", sep = "_")
cols_avg_loe <- paste(cols_avg, "loe", sep = "_")
cols_min_loe <- paste(cols_min, "loe", sep = "_")
cols_all_loe <- paste(cols_all, "loe", sep = "_")

# Calculate LOESS values
dt_cli[,
  (cols_all_loe) := lapply(
    X   = .SD,
    FUN = function(x) {
      predict(loess(formula = x ~ year, span = .75, model = TRUE))
    }
  ),
  by      = c("ssp", "zone"),
  .SDcols = cols_all
]

# ==============================================================================
# 1.2.3 Calculate absolute changes in LOESS values
# ==============================================================================

# Declare columns
cols_max_loe_abs <- paste(cols_max_loe, "abs", sep = "_")
cols_avg_loe_abs <- paste(cols_avg_loe, "abs", sep = "_")
cols_min_loe_abs <- paste(cols_min_loe, "abs", sep = "_")
cols_all_loe_abs <- paste(cols_all_loe, "abs", sep = "_")

# Calculate absolute changes in LOESS values
dt_cli[,
  (cols_all_loe_abs) := lapply(X = .SD, FUN = function(x) { (x - x[1:1]) }),
  by      = c("ssp", "zone"),
  .SDcols = cols_all_loe
]

# ==============================================================================
# 1.2.4 Calculate relative changes in LOESS values
# ==============================================================================

# Declare columns
cols_max_loe_rel <- paste(cols_max_loe, "rel", sep = "_")
cols_avg_loe_rel <- paste(cols_avg_loe, "rel", sep = "_")
cols_min_loe_rel <- paste(cols_min_loe, "rel", sep = "_")
cols_all_loe_rel <- paste(cols_all_loe, "rel", sep = "_")

# Calculate relative changes in LOESS values
dt_cli[,
  (cols_all_loe_rel) := lapply(X = .SD, FUN = function(x) {
    (x - x[1:1]) / x[1:1]
  }),
  by = c("ssp", "zone"),
  .SDcols = cols_all_loe
]

# Save the data to disk
fwrite(
  x    = dt_cli,
  file = paste(dir$res, "dt_cli_summarized_by_zone.csv", sep = "/")
)

# ==============================================================================
# 1.2.5 Tabulate the absolute and relative changes in LOESS values
# ==============================================================================

# # Visualize the absolute and relative changes in the console
# dt_out <- cbind(
#   dt_cli
#   [year == dt_cli[which.max(year), year], c("zone", "ssp", ..cols_all_loe_abs)
#   ][, (cols_all_loe_abs) := round(.SD, 1L), .SDcols = cols_all_loe_abs
#   ][, melt(.SD, id.vars = c("zone", "ssp"))
#   ][, dcast(.SD, formula = zone + variable ~ ssp)
#   ][, zone := factor(zone, levels = c("Global", "Temperate", "Tropical"))
#   ][, variable := gsub("_loe_abs", "", variable)
#   ][, variable := factor(variable, c(rbind(cols_max, cols_avg, cols_min)))
#   ][order(variable, zone)
#   ][, setnames(.SD, 3:6, paste(colnames(.SD)[3:6], "abs", sep = "_"))
#   ][, setcolorder(.SD, c(2, 1, 3, 4, 5))
#   ],
#   dt_cli
#   [year == dt_cli[which.max(year), year], c("zone", "ssp", ..cols_all_loe_rel)
#   ][, (cols_all_loe_rel) := round(.SD * 100L, 1L), .SDcols = cols_all_loe_rel
#   ][, melt(.SD, id.vars = c("zone", "ssp"))
#   ][, dcast(.SD, formula = zone + variable ~ ssp)
#   ][, zone := factor(zone, levels = c("Global", "Temperate", "Tropical"))
#   ][, variable := gsub("_loe_rel", "", variable)
#   ][, variable := factor(variable, c(rbind(cols_max, cols_avg, cols_min)))
#   ][order(variable, zone)
#   ][, setnames(.SD, 3:6, paste(colnames(.SD)[3:6], "rel", sep = "_"))
#   ][, setcolorder(.SD, c(2, 1, 3, 4, 5))
#   ][, c("zone", "variable") := NULL
#   ]
# )
# 
# # Save the data to disk
# fwrite(
#   x    = dt_out,
#   file = paste(dir$res, "dt_cli_loess_changes_by_2100.csv", sep = "/")
# )

# ==============================================================================
# 1.2.6 Plot the base and LOESS values
# ==============================================================================

# # Declare independent variables for grouping
# grp <- c("ssp", "zone")
# 
# # Order the zones so the facets display in alphabetical order
# dt_cli[, zone := factor(zone, levels = c("Global", "Temperate", "Tropical"))]
# 
# # Create a function to plot results
# fn_plot <- function(col) {
#   print(paste("Plotting", as.character(col), sep = " "))
#   (
#     ggplot(
#       data = dt_cli,
#       mapping = aes(
#         x     = year,
#         y     = dt_cli[[as.character(col)]]
#       )
#     ) +
#     geom_line(linewidth = .2) +
#     geom_smooth(formula = y ~ x, method = "loess", linewidth = .5) +
#     # Starting value labels
#     geom_label(
#       data = dt_cli[, .SD[which.min(year)], by  = grp],
#       aes(
#         x = year,
#         y = dt_cli[, .SD[which.min(year)], by   = grp]
#         [[paste(as.character(col), "loe", sep   = "_")]],
#         label = sprintf(fmt = "%.2f",
#           x = dt_cli[, .SD[which.min(year)], by = grp]
#           [[paste(as.character(col), "loe", sep = "_")]]
#         )
#       ),
#       alpha      = .5,
#       fill       = "white",
#       label.r    = unit(0L, "lines"),
#       label.size = 0L,
#       nudge_x    = 5L,
#       size       = 1.5
#     ) +
#     # Ending value labels
#     geom_label(
#       data = dt_cli[, .SD[which.max(year)], by  = grp],
#       aes(
#         x = year,
#         y = dt_cli[, .SD[which.max(year)], by   = grp]
#         [[paste(as.character(col), "loe", sep   = "_")]],
#         label = sprintf(fmt = "%.2f",
#           x = dt_cli[, .SD[which.max(year)], by = grp]
#           [[paste(as.character(col), "loe", sep = "_")]]
#         )
#       ),
#       alpha      = .5,
#       fill       = "white",
#       label.r    = unit(0L, "lines"),
#       label.size = 0L,
#       nudge_x    = -5L,
#       size       = 1.5
#     ) +
#     scale_x_continuous(name = "Year",  n.breaks = 5L) +
#     scale_y_continuous(name = "Value", labels = label_comma(accuracy = .01)) +
#     facet_grid(zone ~ toupper(ssp), scales = "free_y") +
#     theme_light() +
#     theme(
#       axis.title.y = element_blank(),
#       text         = element_text(size = 6)
#     )
#   ) |>
#     ggsave(
#       filename = tolower(paste("9_", col, ".png", sep = "")),
#       device   = "png",
#       path     = "plots",
#       scale    = 1L,
#       width    = 9L,
#       height   = 5.2,
#       units    = "in",
#       dpi      = "retina"
#     )
# }
# 
# # Generate the plots
# mapply(
#   FUN = fn_plot,
#   col = cols_all
# )

# ==============================================================================
# 1.2.7 Plot max, min, and mean of each variable
# ==============================================================================

# Declare independent variables for grouping
grp <- c("ssp", "zone")

# Transform the data
dt_plt <- dt_cli[zone == "Global", c("year", "ssp", ..cols_avg)
][, melt(.SD, id.vars = c("year", "ssp"))
][, variable := gsub("avg_", "", variable)
][, variable := factor(variable, gsub("avg_", "", cols_avg))
]

# Load the label data separately
labs_start <- dt_cli[
  zone == "Global", .SD[which.min(year)],
  by = "ssp", .SDcols = c("year", cols_avg_loe)
][, melt(.SD, id.vars = c("year", "ssp"))
][, variable := gsub("avg_", "", variable)
][, variable := gsub("_loe", "", variable)
]

# Load the label data separately
labs_end <- dt_cli[
  zone == "Global", .SD[which.max(year)],
  by = "ssp", .SDcols = c("year", cols_avg_loe)
][, melt(.SD, id.vars = c("year", "ssp"))
][, variable := gsub("avg_", "", variable)
][, variable := gsub("_loe", "", variable)
]

# Create the plot
ggplot(data   = dt_plt) +
  geom_line(
    linewidth = .2,
    mapping   = aes(
      x       = year,
      y       = value
    )
  ) +
  geom_smooth(
    formula   = y ~ x,
    method    = "loess",
    linewidth = .5,
    mapping   = aes(
      x       = year,
      y       = value
      )
    ) +
  # STILL NEED TO FIX THE LABELS - DISPLAYING BASE VALUE BUT NOT LOESS RIGHT NOW
  # Starting value labels
  geom_label(
    data = labs_start,
    aes(
      x     = year,
      y     = value,
      label = sprintf(fmt = "%.1f", value)
    ),
    alpha      = .5,
    fill       = "white",
    label.r    = unit(0L, "lines"),
    label.size = 0L,
    nudge_x    = 1L,
    size       = 1.5
  ) +
  # Ending value labels
  geom_label(
    data = labs_end,
    aes(
      x     = year,
      y     = value,
      label = sprintf(fmt = "%.1f", value)
    ),
    alpha      = .5,
    fill       = "white",
    label.r    = unit(0L, "lines"),
    label.size = 0L,
    nudge_x    = 4L,
    size       = 1.5
  ) +
  scale_x_continuous(name = "Year",  n.breaks = 5L) +
  scale_y_continuous(name = "Value") +
  facet_grid(variable ~ toupper(ssp), scales = "free_y") +
  theme_light() +
  theme(
    axis.title.y = element_blank(),
    text         = element_text(size = 6)
  )

ggsave(
  filename = tolower(paste("9_", "global", ".png", sep = "")),
  plot     = last_plot(),
  device   = "png",
  path     = "plots",
  scale    = 1L,
  width    = 9L,
  height   = 5.2,
  units    = "in",
  dpi      = "retina"
)

stop()

# ==============================================================================
# 1.7 Plot the climate variables on world maps
# ==============================================================================

# WIP START

# Select only the first and last years
# dt_cli <- dt_cli[year %in% c(2015, 2100)]

# Pivot the dataset from long to wide format
# dt_cli <- dcast.data.table(
#   data      = dt_cli,
#   formula   = zone + ssp ~ year,
#   value.var = cols
# )

# # Express output variables as a percentage of all takeoffs
# lapply(
#   X = cols,
#   FUN = function(x) {
#     dt_cli[, paste(x) := get(paste(x, "2100", sep = "_")) - get(paste(x, "2015", sep = "_"))]
#   }
# )

# # Define the world object from the Natural Earth package
# world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
# 
# # Build a world map and plot the airports
# (ggplot() +
#     geom_sf(data = world, color = NA, fill = "gray", alpha = .5) +
#     scale_y_continuous(breaks = unique(unlist(geo, use.names = FALSE))) +
#     coord_sf(expand = FALSE) +
#     scale_color_viridis() +
#     geom_point(
#       data     = dt_clib[!duplicated(dt_clib$icao), ],
#       mapping  = aes(x = lon, y = lat, color = avg_rho),
#       # color    = "black",
#       shape    = 20L,
#       size     = 1.5
#     ) +
#     # geom_hline(
#     #   aes(yintercept = dt_pop$lat[which.max(dt_pop$lat)]),
#     #   color    = "black"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = df_smp$lat[which.max(df_smp$lat)]),
#     #   color    = "purple"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = dt_pop$lat[which.min(dt_pop$lat)]),
#     #   color    = "black"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = df_smp$lat[which.min(df_smp$lat)]),
#     #   color    = "purple"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = mean(dt_pop[!duplicated(dt_pop$icao), ]$lat)),
#     #   color    = "black"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = mean(df_smp[!duplicated(df_smp$icao), ]$lat)),
#     #   color    = "purple"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = median(dt_pop[!duplicated(dt_pop$icao), ]$lat)),
#     #   color    = "black"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = median(df_smp[!duplicated(df_smp$icao), ]$lat)),
#     #   color    = "purple"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = 66.5635),
#     #   color    = "gray",
#     #   linetype = "dashed"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = 30),
#     #   color    = "gray",
#     #   linetype = "dashed"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = 23.4365),
#     #   color    = "gray",
#     #   linetype = "dashed"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = -23.4365),
#     #   color    = "gray",
#     #   linetype = "dashed"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = -30),
#     #   color    = "gray",
#     #   linetype = "dashed"
#     # ) +
#     # geom_hline(
#     #   aes(yintercept = -66.5635),
#     #   color    = "gray",
#     #   linetype = "dashed"
#     # ) +
#     theme_light() +
#     theme(axis.title = element_blank(), axis.text.x = element_blank())) |>
#   ggsave(
#     filename = "9_world.png",
#     device   = "png",
#     path     = "plots",
#     scale    = 1L,
#     width    = 8.15,
#     height   = 3.69,
#     units    = "in",
#     dpi      = "print"
#   )
# 
# # WIP END

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
dt_tko <- fn_sql_qry(
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
set(x = dt_tko, j = "year",    value = as.integer(dt_tko[, year]))
set(x = dt_tko, j = "zone",    value = as.factor(dt_tko[, zone]))
set(x = dt_tko, j = "ssp",     value = as.factor(dt_tko[, ssp]))
set(x = dt_tko, j = "icao",    value = as.factor(dt_tko[, icao]))
set(x = dt_tko, j = "type",    value = as.factor(dt_tko[, type]))
set(x = dt_tko, j = "itr_sum", value = as.numeric(dt_tko[, itr_sum]))

# Recode frigid airports to temperate
dt_tko[zone == "Frigid", zone := "Temperate"]

# Combine the aircraft types to narrow/widebody
levels(dt_tko$type) <- bod

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
    dt_tko[, paste(x, "rel", sep = "_") := get(x) / tko]
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

# Declare independent variables for grouping
grp <- c("year", "ssp", "zone", "type")

# Summarize the data and combine them by group
dt_tko <- rbind(
  # Zonal summary by group
  cbind(
    dt_tko[, lapply(X = .SD, FUN = sum),
      by = grp,
      .SDcols = cols_sum
    ],
    dt_tko[, lapply(X = .SD, FUN = mean),
      by = grp,
      .SDcols = cols_mean
    ][, ..cols_mean]
  ),
  # Global summary by group
  cbind(
    dt_tko[, zone := "Global"][, lapply(X = .SD, FUN = sum),
      by = grp,
      .SDcols = cols_sum
    ],
    dt_tko[, zone := "Global"][, lapply(X = .SD, FUN = mean),
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

# Declare independent variables for grouping
grp <- c("ssp", "zone", "type")

# Perform regression fitting by group
dt_tko[,
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
  x    = dt_tko,
  file = paste(dir$res, "dt_tko.csv", sep = "/")
)

# ==============================================================================
# 2.6 Plot the results
# ==============================================================================

# Update output variables for plotting
cols <- c(
  "tko_ok_thr_min_rel",
  "tko_ko_rel"
)

# Declare independent variables for grouping
grp <- c("ssp", "zone")

# Create a function to plot results
fn_plot <- function(body, cols) {
  (
    ggplot(
      data = dt_tko[type == body],
      mapping = aes(
        x     = year,
        y     = dt_tko[type == body][[as.character(cols)]]
      )
    ) +
      geom_line(linewidth = .2) +
      geom_smooth(formula = y ~ x, method = "loess", linewidth = .5) +
      # Starting value labels
      geom_label(
        data = dt_tko[type == body][, .SD[which.min(year)], by = grp],
        aes(
          x = year,
          y = dt_tko[type == body][, .SD[which.min(year)], by = grp]
          [[paste(as.character(cols), "loess", sep = "_")]],
          label = sprintf(fmt = "%1.1f%%", dt_tko[type == body]
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
        data = dt_tko[type == body][, .SD[which.max(year)], by = grp],
        aes(
          x = year,
          y = dt_tko[type == body][, .SD[which.max(year)], by = grp]
          [[paste(as.character(cols), "loess", sep = "_")]],
          label = sprintf(fmt = "%1.1f%%", dt_tko[type == body]
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
dt_res <- fn_sql_qry(
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
set(x = dt_res, j = "year", value = as.integer(dt_res[, year]))
set(x = dt_res, j = "zone", value = as.factor(dt_res[, zone]))
set(x = dt_res, j = "ssp",  value = as.factor(dt_res[, ssp]))
set(x = dt_res, j = "icao", value = as.factor(dt_res[, icao]))
set(x = dt_res, j = "type", value = as.factor(dt_res[, type]))

# Recode frigid airports to temperate
dt_res[zone == "Frigid", zone := "Temperate"]

# Combine the aircraft types to narrow/widebody
levels(dt_res$type) <- bod

# ==============================================================================
# 3.2 Treat output variables of interest
# ==============================================================================

# Convert thrust reduction below TOGA to thrust as a percentage of TOGA
dt_res[, avg_thr := (100L - avg_thr_red) / 100L][, avg_thr_red := NULL]

# Convert payload removal in kg to passengers based on standard assumptions
dt_res[, avg_pax_rem := avg_tom_rem / sim$pax_avg][, avg_tom_rem := NULL]

# ==============================================================================
# 3.3 Summarize the data
# ==============================================================================

# Declare output variables for averaging
cols <- c("avg_todr", "avg_thr", "avg_pax_rem")

# Declare independent variables for grouping
grp <- c("year", "ssp", "zone", "type")

# Summarize the data and combine them by group
dt_res <- rbind(
  # Zonal summary by group
  dt_res[, lapply(X = .SD, FUN = mean),
    by = grp,
    .SDcols = cols
  ],
  # Global summary by group
  dt_res[, zone := "Global"][, lapply(X = .SD, FUN = mean),
    by = grp,
    .SDcols = cols
  ]
)

# ==============================================================================
# 3.4 Add local polynomial regression fitting (LOESS) to the output variables
# ==============================================================================

# Declare independent variables for grouping
grp <- c("ssp", "zone", "type")

# Perform regression fitting by group
dt_res[,
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
  x    = dt_res,
  file = paste(dir$res, "dt_res.csv", sep = "/")
)

# ==============================================================================
# 3.6 Plot the results
# ==============================================================================

# Declare independent variables for grouping
grp <- c("ssp", "zone")

# Create a function to plot results
fn_plot <- function(body, cols) {
  (
    ggplot(
      data = dt_res[type == body],
      mapping = aes(
        x     = year,
        y     = dt_res[type == body][[as.character(cols)]]
      )
    ) +
      geom_line(linewidth = .2) +
      geom_smooth(formula = y ~ x, method = "loess", linewidth = .5) +
      # Starting value labels
      geom_label(
        data = dt_res[type == body][, .SD[which.min(year)], by = grp],
        aes(
          x = year,
          y = dt_res[type == body][, .SD[which.min(year)], by = grp]
          [[paste(as.character(cols), "loess", sep = "_")]],
          label = round(
            x = dt_res[type == body]
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
        data = dt_res[type == body][, .SD[which.max(year)], by = grp],
        aes(
          x = year,
          y = dt_res[type == body][, .SD[which.max(year)], by = grp]
          [[paste(as.character(cols), "loess", sep = "_")]],
          label = round(
            x = dt_res[type == body]
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