###############################################################################
# takeoff.R                                                                   #
# Models the aircraft takeoff performance                                     #
###############################################################################

# Load common script for housekeeping
#source("scripts/common.R")

library(deSolve)

# Airport-specific constants
mu <- .2 # Dimensionless coefficient of runway friction
theta <- # Runway slope
rho <- 1.225 # Air density in kg/m³ at sea level under ISA

# Universal constants
g <- 9.80665 # Gravitational acceleration constant in m/s²

# Aircraft-specific variables
m <- 80000 # Typical mass in kg
V <- 150 # Airspeed
S <- 1 # Wing surface in m²
CL <- 1 # Lift coefficient
CD <- 1 # Drag coefficient

# Force calculations
L <- .5 * rho * V^2 * S * CL
D <- .5 * rho * V^2 * S * CD

W <- m * g # Weight in N
W

F <- mu * (W * cos(theta) - L)
F

T <- 200000

a <- (T - D + mu * L - m * g * (mu * cos(theta) - sin(theta))) / m
a