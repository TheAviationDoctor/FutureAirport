################################################################################
# /scripts/csv_avg_parse.R                                                     #
# Parses the per-airport csv files to describe climate statistics              #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(ggplot2)
library(R.utils)

# Clear the console
cat("\014")

################################################################################
# List csv files to be processed                                               #
################################################################################

csv_in_path <- "data/climate/5_csv_averages"
csv_in_list <- list.files(path = csv_in_path, pattern = "\\.csv.gz$", full.names = FALSE, recursive = TRUE)

################################################################################
# Read and process each file sequentially                                      #
################################################################################

# for(i in 1:1) {
for(i in 1:length(csv_in_list)) {
    
  # Output progress to the console
  print(paste("   Processing file ", i, " of ", length(csv_in_list), "...", sep = ""))
  
  # Load the current file into a data.table
  file <- fread(      
    file = file.path(csv_in_path, csv_in_list[i], fsep = "/"),
    header = TRUE,
    colClasses = list(POSIXct = 1:1, factor = 2:2, numeric = 3:ncol(file)),
    data.table = TRUE,
    key = c("nc.time", "nc.exp")
  )

time    <- file[, nc.time]
exp     <- file[, nc.exp]
rho     <- file[, global]

ggplot(data = file, mapping = aes(x = time, y = rho)) +
  geom_line() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Time", y = expression(rho)) +
  facet_grid(exp) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

ggsave(
    paste("rho.png"),
    plot = last_plot(),
    device = "png",
    path = "plots/",
    scale = 1,
    width = 6,
    height = NA,
    units = "in",
    dpi = "print",
    limitsize = TRUE,
    bg = NULL
  )

}
