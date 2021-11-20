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
  
  ##############################################################################
  # Set the simulation inputs                                                  #
  ##############################################################################
  
  # Known aircraft data
  lambda_f <- dt_act[type %in% dt_cal[i, type], lambda_f] # 
  cfc      <- dt_act[type %in% dt_cal[i, type], cfc]      # 
  SfS      <- dt_act[type %in% dt_cal[i, type], SfS]      # 
  cD0      <- dt_act[type %in% dt_cal[i, type], cD0]      # 
  k        <- dt_act[type %in% dt_cal[i, type], k]        # 
  span     <- dt_act[type %in% dt_cal[i, type], span]     # 
  S        <- dt_act[type %in% dt_cal[i, type], S]        # Total wing surface area in takeoff configuration in m²
  n        <- dt_act[type %in% dt_cal[i, type], n]        # Engine count in units
  bpr      <- dt_act[type %in% dt_cal[i, type], bpr]      # Engine bypass ratio
  slst     <- dt_act[type %in% dt_cal[i, type], slst]     # Engine sea-level static maximum thrust in N (per engine)
  
  # Known calibration data
  cal_m    <- dt_cal[i, cal_m]                            # Calibrated takeoff mass in kg
  cal_todr <- dt_cal[i, cal_todr]                         # Calibrated TODR in m
  
  ############################################################################
  # Perform the simulation and save the results                              #
  ############################################################################
  
  # Calculate the drag coefficient in takeoff configuration from the estimated cL
  cD <- fn_sim_cd(lambda_f, cfc, SfS, cD0, k, span, cal_m, S, cL)
  
  # Run the takeoff simulation using sea-level ISA environmental conditions and return the simulated TODR
  todr <- fn_sim_tko(cal_m, S, cL, cD, n, bpr, slst, sim$hdw, sim$ps, sim$rho, sim$tas)
  
  ############################################################################
  # Save the results                                                         #
  ############################################################################
  
  # Save the drag coefficient to the data table
  dt_cal[i, opt_cD := cD]
  
  # Save the  simulated TODR to the data table
  dt_cal[i, sim_todr := todr]
  
  # Save the percentage of difference between the calibrated and simulated TODR
  dt_cal[i, diff := round(abs(cal_todr - sim_todr) / cal_todr * 100, 3)]
  
  # Calculate the residual sum of squares between calibrated and simulated TODR values
  dt_cal[i, rss := sum((dt_cal[i, sim_todr] - dt_cal[i, cal_todr])^2)]
  
  # Return the residual sum of squares
  return(dt_cal[i, rss])
  
} # End of the fn_sim_cal function

################################################################################
# Optimize the model by finding the cL that minimizes the RSS                  #
#  for each takeoff mass/distance pair                                         #
################################################################################

# For each calibrated takeoff mass/distance pair
for (i in 1:nrow(dt_cal)) {
  
  # Run the optimizer
  opt <- optimize(f = fn_sim_cal, interval = c(0.1, 3))

  # Save the optimal cL  
  dt_cal[i, opt_cL := opt$minimum]

  # Run the takeoff simulation again with the optimal cL value
  # fn_sim_cal(opt_cL)
  
} # End of the for loop

View(dt_cal)

##########################################################################
# Plot the results                                                       #
##########################################################################

# Rename the columns for better plotting display
setnames(dt_cal, old = c("cal_todr", "sim_todr", "opt_cL", "opt_cD"), new = c("Calibrated", "Simulated", "cL", "cD"))

# Pivot the data table from wide to long for plotting
dt_cal <- melt(dt_cal, id.vars = c("type", "cal_m", "cL", "cD"), measure.vars = c("Calibrated", "Simulated"), variable.name = "TODR", value.name = "value")

# Build a faceted plot of calibrated and simulated TODR (x-axis) over calibrated mass (y-axis)
cal_plot <- ggplot() +
  geom_line(data = dt_cal, mapping = aes(x = value, y = cal_m, color = type)) +
  scale_color_manual(values = c("turquoise", "magenta")) +
  scale_x_continuous("Regulatory takeoff distance required in m", labels = scales::comma) +
  scale_y_continuous("Takeoff mass in kg", labels = scales::comma) +
  scale_color_discrete(name = "Type") +
  facet_wrap(~ TODR, ncol = 2, scales = "free") +
  theme_light()

# Save the plot to a file
# ggsave(
#   paste("cal_plot.png"),
#   plot = cal_plot,
#   device = "png",
#   path = path_plt,
#   scale = 1,
#   width = 6,
#   height = NA,
#   units = "in",
#   dpi = "print",
#   limitsize = TRUE,
#   bg = NULL
# )

# Display the plot
print(cal_plot)

# Build a plot of calibrated mass (x-axis) over optimal lift coefficient (y-axis)
cLm_plot <- ggplot() +
  geom_line(data = dt_cal, mapping = aes(x = cal_m, y = cL, color = type)) +
  scale_x_continuous("Takeoff mass in kg", labels = scales::comma) +
  scale_y_continuous("Dimensionless lift coefficient", labels = scales::comma) +
  scale_color_discrete(name = "Type") +
  theme_light()

# Save the plot to a file
# ggsave(
#   paste("cLm_plot.png"),
#   plot = cLm_plot,
#   device = "png",
#   path = path_plt,
#   scale = 1,
#   width = 6,
#   height = NA,
#   units = "in",
#   dpi = "print",
#   limitsize = TRUE,
#   bg = NULL
# )

# Display the plot
print(cLm_plot)

# ##########################################################################
# # Build a linear model to predict cL based on takeoff mass at Vlof       #
# ##########################################################################
# 
# # Build a linear model of lift coefficient against calibrated mass
# lm.cL <- lm(formula = cL ~ cal_m, data = dt_cal)
# 
# # Display the coefficients
# summary(lm.cL)
# 
# # Save by how much cL will change for every 1 kg change in mass
# dt_cal <- dt_cal[, coef := coef(summary(lm.cL))["cal_m", "Estimate"]]
# 
# ##########################################################################
# # Store the values to the database for reuse in the simulation           #
# ##########################################################################
# 
# # Remove the simulated values which are no longer needed
# dt_cal <- dt_cal[TODR == "Calibrated"]
# 
# # Keep only the maximum calibrated mass value
# dt_cal <- dt_cal[cal_m == max(cal_m)]
# 
# # Choose which columns to keep
# cols <- c("type", "cal_m", "cL", "coef")
# 
# # Keep only the columns of interest
# dt_cal <- dt_cal[, ..cols]
# 
# # Merge the values into the original aircraft data table
# dt_act <- dt_act[dt_cal, on = "type"]

################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF