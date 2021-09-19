################################################################################
# /scripts/headwind.R                                                          #
# Calculates the headwind variable from sfcWind, uas, and vas                  #
#  Took ~2 hours to run on the researchers' config (https://bit.ly/3ChCBAP)    #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(parallel)

# Clear the console
cat("\014")

################################################################################
# Housekeeping                                                                 #
################################################################################

rwy     <- 06

spd_wnd <- 8       # Wind speed in kt
hdg_wnd <- 160     # Wind heading in deg (direction from which the wind is blowing)
hdg_rwy <- 06 * 10 # 

alpha <- abs(hdg_rwy - hdg_wnd)

alpha

# Calculate headwind speed
spd_hdw <- spd_wnd * cos(alpha * pi / 180)

# Calculate crosswind speed
spd_hdw <- spd_wnd * sin(alpha * pi / 180)