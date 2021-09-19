################################################################################
# thrust.R                                                                     #
# Models the thrust decrease over the takeoff                                  #
################################################################################

# Load common constants
source("scripts/constants.R")

# Clear the console
cat("\014")

# Velocity (true airspeed) in knots
V <- 146

# Intake area in square meters
A <- pi * (72.3 * .0254 / 2)^2
A

# Mass flow rate (i.e. air mass per unit of time) in kg/sec
m_dot <- rho["m"] * V * A

m_dot

# For the CFM56-5C, the mass flow rate is 1,065 pounds / 483 kg (http://www.iasg.co.uk/pdfs/articles/engine_services/Pages%20from%20JULY%20CVR.pdf)

1065 * (100 - 40)

474 * (100 - 40)

1350 / (rho["m"] * (pi * (3.124 / 2)^2))

rho["m"]

802 / (rho["m"] * (pi * (2.362 / 2)^2))

802 / (1.1 * (pi * (2.362 / 2)^2))

bpr <- 6.4
mdotc <- 162.5
mdotfan <- bpr * mdotc
mdot <- mdotfan + mdotc
mdot