################################################################################
#    NAME: scripts/6_calibration.R                                             #
#   INPUT:                                                                     #
# ACTIONS:                                                                     #
#  OUTPUT:                                                                     #
# RUNTIME: ~X hours on the researcher's config (https://bit.ly/3ChCBAP)        #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(data.table)
library(DBI)
library(ggplot2)
library(parallel)

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

##########################################################################
# Calibrate the aircraft data                                            #
##########################################################################

# Calculate the drag coefficient component attributable to flaps/slats in takeoff configuration
delta_cD_flaps <- dt_act[, lambda_f] * dt_act[, cfc]^1.38 * dt_act[, SfS] * sin(sim$flap_angle * pi / 180)^2

# Calculate the drag coefficient component attributable to the landing gear
delta_cD_gear <- dt_act[, m] * sim$g / dt_act[, S] * 3.16E-5 * dt_act[, m]^-.215

# Calculate the total drag coefficient in non-clean configuration
cD0_total <- dt_act[, cD0] + delta_cD_flaps + delta_cD_gear

# Calculate the Oswald efficiency factor for the selected flap deflection (for wing-mounted engines)
delta_e_flaps <- .0026 * sim$flap_angle

# Calculate the aspect ratio
ar <- dt_act[, span]^2 / dt_act[, S]

# Calculate the lift-induced coefficient k in non-clean configuration
k_total <- 1 / (1 / dt_act[, k] + pi * ar * delta_e_flaps)

# TEST THE SENSITIVITY OF THE RESULTS TO A NEW VLOF
# dt_act[, Vlof := Vlof * 1.35]

# TRY TO CALCULATE VSTALL
# dt_act[, Vstall := sqrt( ( 2 * m * sim$g ) / ( sim$rho * S * 1.2 ) )]

# TEST OVERRIDE VLOF AS A FUNCTION OF VSTALL
# dt_act[, Vlof := 1.1 * Vstall]

# Calculate the dynamic pressure at the most common Vlof (based on ADS-B data from OpenAP)
q <- .5 * sim$rho * dt_act[, Vlof]^2

# Calculate the lift coefficient at MTOM in non-clean configuration
cL <- dt_act[, m] * sim$g / ( q * dt_act[, S] )

# TEST THE SENSITIVITY OF THE RESULTS TO A NEW CL
cL <- cL * .52

# Save the lift coefficient to the table
dt_act[, cL := cL]

# Calculate the drag coefficient in non-clean configuration
cD <- cD0_total + k_total * cL^2

# Save the drag coefficient to the table
dt_act[, cD := cD]

# Check that (cD - mu * cL) approximates .08 with a mu value of .0165 as per Blake (2009)
dt_act[, blake := cD - .0165 * cL]





##########################################################################
# Calculate the regulatory takeoff distance required (TODR) in m         #
##########################################################################

# For each takeoff performance calibration data observation
for (i in 1:nrow(dt_cal)) {

  ##########################################################################
  # Calculate the weight force in N                                        #
  ##########################################################################
  
  W <- fn_sim_wgt(dt_cal[i, cal_m])
  
  ##########################################################################
  # Calculate the speed in m/s at which lift L equals weight W             #
  ##########################################################################
  
  Vlof <- fn_sim_vlo(W, dt_act[type %in% dt_cal[i, type], S], dt_act[type %in% dt_cal[i, type], cL], sim$rho)
  
  ##########################################################################
  # Calculate the takeoff speeds V in m/s                                  #
  ##########################################################################
  
  V <- fn_sim_spd(sim$hdw, Vlof)
  
  ##########################################################################
  # Calculate the dynamic pressure q in Pa                                 #
  ##########################################################################
  
  q <- fn_sim_dyn(sim$rho, V$tas)

  ##########################################################################
  # Calculate the lift force in N                                          #
  ##########################################################################

  L <- fn_sim_lft(q, dt_act[type %in% dt_cal[i, type], S], dt_act[type %in% dt_cal[i, type], cL])

  ##########################################################################
  # Calculate the drag force in N                                          #
  ##########################################################################

  D <- fn_sim_drg(q, dt_act[type %in% dt_cal[i, type], S], dt_act[type %in% dt_cal[i, type], cD])

  ##########################################################################
  # Calculate the propulsive force F in N                                  #
  ##########################################################################

  F <- fn_sim_thr(dt_act[type %in% dt_cal[i, type], n], dt_act[type %in% dt_cal[i, type], bpr], dt_act[type %in% dt_cal[i, type], slst], sim$ps, sim$tas, V$tas, sim$reg_rto)

  ##########################################################################
  # Calculate the acceleration a in m/s² up to liftoff                     #
  ##########################################################################

  a <- fn_sim_acc(W, F$rto, dt_act[type %in% dt_cal[i, type], cD], dt_act[type %in% dt_cal[i, type], cL], q, dt_act[type %in% dt_cal[i, type], S])
  
  ##########################################################################
  # Calculate the horizontal takeoff distance in m                         #
  ##########################################################################

  todr <- fn_sim_dis(a, V$gnd)
  
  ##########################################################################
  # Save the results                                                       #
  ##########################################################################
  
  # Save the  simulated TODR to the data table
  dt_cal[i, sim_todr := todr]
  
  # Save the percentage of difference between calibrated and simulated TODR
  dt_cal[i, diff := round(abs(cal_todr - sim_todr) / cal_todr * 100, 2)]
  
} # End of the for loop

# Calculate the root mean square error between calibrated and simulated TODR values
rmse <- sqrt(mean((dt_cal[, sim_todr] - dt_cal[, cal_todr])^2))
rmse

##########################################################################
# Plot the results                                                       #
##########################################################################

# Build a plot of TODR over mass
cal_plot <- ggplot() +
  geom_line(data = dt_cal, mapping = aes(x = cal_todr, y = cal_m, color = "magenta")) +
  geom_line(data = dt_cal, mapping = aes(x = sim_todr, y = cal_m, color = "blue")) +
  scale_color_discrete(name = "TODR", labels = c("Simulated", "Calibrated")) +
  scale_x_continuous("Regulatory takeoff distance required in m", labels = scales::comma) +
  scale_y_continuous("Takeoff mass in kg", labels = scales::comma) +
  guides(color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ type, ncol = 2, scales = "free") +
  theme_light()

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

# Display the plot
cal_plot

# View the results
# View(dt_act)
# View(dt_cal)

# EOF