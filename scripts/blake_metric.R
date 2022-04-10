################################################################################
# blake.R                                                                      #
# Replicates the takeoff performance model from Blake (2009), p. 18-12         #
################################################################################

# library(ggplot2)

# Clear the console
cat("\014")

################################################################################
# Constants definition                                                         #
################################################################################

g <- 9.806665                                                                     # Gravitational acceleration constant in slugs per cubic foot, assuming a non-oblate, non-rotating Earth

################################################################################
# Variables definition - Atmosphere                                            #
################################################################################

rho <- 1.225                                                                  # Air density in slugs per cubic foot

w   <- 0                                                                        # Starting headwind in knots

################################################################################
# Variable definition - Runway                                                 #
################################################################################

mu    <- .0165                                                                  # Dimensionless coefficient of rolling friction  used in the Blake (2009) example, p. 18-11

theta <- 0                                                                      # Runway slope in degrees. Assumed to be zero as a positive slope in one runway heading would cancel out the negative slope in the reciprocal heading

Hp  <- 0                                                                        # Geopotential pressure altitude in feet

################################################################################
# Variables definition - Aircraft                                              #
################################################################################

W <- 240000 / 2.205                                                                    # Aircraft weight in pounds

S     <- 1951 / 10.764                                                                   # Reference wing area in square feet

CL    <- 1.6087                                                                 # Dimensionless lift coefficient

CD    <- .08 + mu * CL                                                          # Dimensionless drag coefficient (adjusted so that CD - mu * CL = .08 as per Blake's (2009) example)

################################################################################
# Variables definition - Speeds                                                #
################################################################################

GS      <- c(0, 20, 40, 60, 80, 100, 120, 140, 150, 160) / 1.944                        # Groundspeed increments (in knots) at which the acceleration is calculated

TAS     <- GS + w                                                               # True airspeed (groundspeed + wind)

################################################################################
# Variables definition - Engines                                               #
################################################################################

T <- c("0" = 35532, "20" = 34653, "40" = 33775, "60" = 32896, "80" = 32014, "100" = 31139, "120" = 30260, "140" = 29381, "150" = 28942, "160" = 28503) * 4.448 # Thrust of each engine in pounds for a given true airspeed

################################################################################
# Estimate the decrease in thrust from an airspeed increase in Table 18-2      #
################################################################################

# Values for thrust T of each engine (in pounds) for increments in true airspeed TAS
# T <- c(35532, 34653, 33775, 32896, 32014, 31139, 30260, 29381, 28942, 28503)    # Thrust
# TAS <- c(0, 20, 40, 60, 80, 100, 120, 140, 150, 160)                            # True airspeed
# d <- data.frame(T, TAS)

################################################################################
# Acceleration calculation                                                     #
################################################################################

q     <- (rho * TAS^2) / 2                                          # Dynamic pressure. The speed is converted to feet per second

a     <- g / W * (T * 2 - (mu * W) - (CD - (mu * CL)) * (q * S) - (W * sin(theta))) # Acceleration in feet per second per second

################################################################################
# Display results in a table as per page 18-12 of Blake (2009)                 #
################################################################################

d <- cbind("GS - m/s" = GS, "TAS - m/s" = TAS, "Dynamic pressure" = round(q, digits = 2), "F - thrust" = T, "F - slope" = theta, "μW" = mu * W, "(CD-μCL)qS" = round((CD - (mu * CL)) * (q * S), digits = 0), "accel ft/sec/sec" = round(a, digits = 1), "accel knots/sec" = round(a / 1.68781, digits = 1))

rownames(d) <- NULL                                                             # Remove row names from the d matrix

d                                                                               # Output results to the console
