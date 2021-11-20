################################################################################
#    NAME: scripts/6_calibration.R                                             #
#   INPUT: Published aircraft geometric and engine characteristics             #
#          Published takeoff mass/distance values empirically determined       #
# ACTIONS: Assume a starting value for the unknown lift coefficient cL         #
#          Simulate takeoffs at varying mass values using that assumed cL      #
#          Compare the resulting TODR with that published by the OEMs          #
#          Vary cL values until the residual error (RSS) is minimized          #
#  OUTPUT: Optimized values for cL and cD (which is derived from cL)           #
# RUNTIME: ~X hours on the researcher's config (https://bit.ly/3ChCBAP)        #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(ggplot2)

# Import the common settings
source("scripts/0_common.R")

# Import the simulation functions
source("scripts/6.0_functions.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

# Import the aircraft data from the CSV file
dt_act <- fn_imp_act()

# Import the calibration data from the CSV file
dt_cal <- fn_imp_cal()

################################################################################
# Calculate the regulatory takeoff distance required (TODR) in m based on:     #
#  cL: assumed lift coefficient to calibrate for optimal fitness to OEM data   #
################################################################################

fn_sim_cal <- function(cL) {

  # For each calibrated takeoff mass/distance pair
  for (i in 1:nrow(dt_cal)) {
    
    ##############################################################################
    # Set the simulation inputs                                                  #
    ##############################################################################
    
    # Calibration assumptions
    cD   <- fn_sim_cd(cL)                           # Mean drag coefficient in takeoff configuration from the estimated cL
    
    # Known aircraft data
    S        <- dt_act[type %in% dt_cal[i, type], S]    # Total wing surface area in takeoff configuration in m²
    n        <- dt_act[type %in% dt_cal[i, type], n]    # Engine count in units
    bpr      <- dt_act[type %in% dt_cal[i, type], bpr]  # Engine bypass ratio
    slst     <- dt_act[type %in% dt_cal[i, type], slst] # Engine sea-level static maximum thrust in N (per engine)
    
    # Known calibration data
    cal_m    <- dt_cal[i, cal_m]                        # Calibrated takeoff mass in kg
    cal_todr <- dt_cal[i, cal_todr]                     # Calibrated TODR in m
    
    ############################################################################
    # Perform the simulation and save the results                              #
    ############################################################################
    
    # Run the takeoff simulation using sea-level ISA environmental conditions and return the simulated TODR
    todr <- fn_sim_tko(cal_m, S, cL, cD, n, bpr, slst, sim$hdw, sim$ps, sim$rho, sim$tas)
    
    ############################################################################
    # Save the results                                                         #
    ############################################################################
    
    # Save the simulated TODR to the data table
    dt_cal[i, sim_todr := todr]
    
    # Save the percentage of difference between calibrated and simulated TODR
    dt_cal[i, diff := round(abs(cal_todr - sim_todr) / cal_todr * 100, 2)]
    
  } # End of the for loop
  

  # Calculate the residual sum of squares between calibrated and simulated TODR values
  rss <- sum((dt_cal[, sim_todr] - dt_cal[, cal_todr])^2)

  print(paste("cL =", cL, "|", "rss =", rss, sep = " "))

  # Return the residual sum of squares
  return(rss)

} # End of the fn_sim_cal function
  
################################################################################
# Optimize the simulation by finding the cL that minimizes the RSS             #
################################################################################

# Run the optimizer
opt <- optimize(f = fn_sim_cal, interval = c(0, 3))

# Extract the optimal cL value from the optimizer
opt_cL <- opt$minimum

# Run the takeoff simulation again with the optimal cL value
fn_sim_cal(opt_cL)

##########################################################################
# Plot the results                                                       #
##########################################################################

# Rename the columsn for display in the legend
setnames(dt_cal, old = c("cal_todr", "sim_todr"), new = c("Calibrated", "Simulated"))

# Pivot the data table from wide to long for plotting
dt_cal <- melt(dt_cal, id.vars = c("type", "cal_m"), measure.vars = c("Calibrated", "Simulated"), variable.name = "TODR", value.name = "value")

# Build a plot of calibrated and simulated TODR over calibrated mass
cal_plot <- ggplot() +
  geom_line(data = dt_cal, mapping = aes(x = value, y = cal_m, color = TODR)) +
  scale_color_manual(values = c("turquoise", "magenta")) +
  scale_x_continuous("Regulatory takeoff distance required in m", labels = scales::comma) +
  scale_y_continuous("Takeoff mass in kg", labels = scales::comma) +
  annotate(geom = "text", x = mean(dt_cal[, value]), y = mean(dt_cal[, cal_m]), label = paste("cL =", round(opt_cL, digits = 3), " | ", "cD =", round(fn_sim_cd(opt_cL), digits = 3), sep = " ")) +
  facet_wrap(~ type, ncol = 2, scales = "free") +
  theme_light()

# Display the plot
print(cal_plot)

# Save the plot to a file
ggsave(
  paste("cal_plot.png"),
  plot = cal_plot,
  device = "png",
  path = path_plt,
  scale = 1,
  width = 6,
  height = NA,
  units = "in",
  dpi = "print",
  limitsize = TRUE,
  bg = NULL
)

# EOF