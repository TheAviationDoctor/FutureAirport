################################################################################
# /scripts/csv_long_to_wide.R                                                  #
# Pivot the per-airport csv files from long to wide                            #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(R.utils)

# Clear the console
cat("\014")

################################################################################
# List csv files to be processed                                               #
################################################################################

csv_in_path <- "data/climate/3_csv_long"
csv_in_list <- list.files(path = csv_in_path, pattern = "\\.csv.gz$", full.names = FALSE, recursive = TRUE)

################################################################################
# Read and process each file sequentially                                      #
################################################################################

for(i in 1:length(csv_in_list)) {
  
  # Output progress to the console
  print(paste("   Processing file ", i, " of ", length(csv_in_list), "...", sep = ""))
  
  # Load the current file into a data.table
  file <- fread(      
    file = file.path(csv_in_path, csv_in_list[i], fsep = "/"),
    header = TRUE,
    colClasses = c("POSIXct", "factor", "factor", "numeric"),
    col.names = c("nc.time", "nc.exp", "nc.var", "nc.val"),
    data.table = TRUE,
    key = c("nc.var", "nc.exp", "nc.time")
  )

  # Convert the current file from long to wide
  file <- dcast.data.table(
    data = file,
    formula = nc.time + nc.exp ~ nc.var,
    value.var = "nc.val"
  )

  # The last value of 'hurs' would be empty, so we remove it
  file <- na.omit(object = file, cols = "hurs")

  ################################################################################
  # Calculate the air density of moist air                                       #
  ################################################################################
  
  # Inputs
  hurs <- file[, hurs] / 100   # Near-surface relative humidity in %
  ps   <- file[, ps] / 100     # Near-surface air pressure in hPa
  tas  <- file[, tas] - 273.15 # Near-surface air temperature in °C
  pol  <- 0.99999683 +         # Polynomial approximation for the saturation vapor pressure over liquid water at 0°C as per function ESW(T) at https://icoads.noaa.gov/software/other/profs. Requires tas in °C
          tas * (-0.90826951E-02 +
            tas * (0.78736169E-04 +
              tas * (-0.61117958E-06 +
                tas * (0.43884187E-08 +
                  tas * (-0.29883885E-10 +
                    tas * (0.21874425E-12 +
                      tas * (-0.17892321E-14 +
                        tas * (0.11112018E-16 +
                          tas * (-0.30994571E-19)))))))))
  es0  <- 6.1078  # Saturation vapor pressure at 0°C (constant)
  Rd   <- 287.058 # Specific gas constant for dry air, in J/(kg·K)
  Rv   <- 461.495 # Specific gas constant for water vapor, in J/(kg·K)
  
  # Outputs
  esw <- es0 / pol^8 # Saturation vapor pressure at tas, in Pa
  pv  <- esw * hurs  # Partial pressure of water vapor, in Pa
  pd  <- ps - pv     # Partial pressure of dry air, in Pa
  rho <- ((pd / (Rd * (tas + 273.15))) + (pv / (Rv * (tas + 273.15)))) * 100 # Total air density. Requires tas in K
  
  # Save the air density to a new column
  file[, rho := rho]
  
  ################################################################################
  # Save each file                                                               #
  ################################################################################
  
  # Write the results to a compressed CSV file
  csv_out_path <- "data/climate/4_csv_wide"
  csv_out_name <- csv_in_list[i]
  fwrite(x = file, file = file.path(csv_out_path, csv_out_name, fsep = "/"), append = FALSE, na = NA, compress = "gzip")
  
}

# EOF