################################################################################
# takeoff.R                                                                    #
# Models the aircraft takeoff performance                                      #
################################################################################

# Import the constants
source("scripts/0_common.R")

# Clear the console
cat("\014")

################################################################################
# Variables definition - Aircraft                                              #
################################################################################

W <- c(                                                                         # Aircraft weight in pounds
  "i" = 240000,                                                                 # In pounds
  "m" = 108862.169                                                              # In kilograms
)


S     <- c(                                                                     # Reference wing area
  "i" = 1951,                                                                   # In square feet
  "m" = 91.09                                                                   # In square meters
)

CL    <- c(                                                                     # Dimensionless lift coefficient as per BADA 3 (p. 20 of User Manual v3.15)
  "j2m"   = 1.6087,                                                             # Medium twin jet
  "blake" = 1.9805065                                                           # Used to reach (CD - (mu * CL)) = .08 in Blake's (2009) example
)

CD    <- sum(                                                                   # Drag coefficient at takeoff as per BADA 3 (p. 20 of User Manual v3.15)
  .031000,                                                                      # CD0 for takeoff configuration (5° flaps)
  .045000,                                                                      # CD2 for takeoff configuration (5° flaps)
  .022800 * CL["j2m"]                                                           # Drag component imputable to the landing gear
)

################################################################################
# Variables definition - Speeds                                                #
################################################################################

V      <- c(0, 20, 40, 60, 80, 100, 120, 140, 150, 160)                         # True airspeed increments (in knots) at which the acceleration is calculated
VSTALL <- 125                                                                   # Stalling speed in takeoff configuration (5° flaps) as per BADA 3

################################################################################
# Variables definition - Engines                                               #
################################################################################

T     <- rbind(                                                                 # Thrust of each engine
  "i" = c(35532, 34653, 33775, 32896, 32014, 31139, 30260, 29381, 28942, 28503),# In pounds
  "m" = c(16117.03, 15718.32, 15320.07, 14921.36, 14521.29, 14124.40, 13725.69, 13326.99, 13127.86, 12928.73) # In kilograms
)

CT1 <- .13899E+06                                                               # First coefficient of max climb / takeoff thrust as per BADA 3
CT2 <- .45045E+05                                                               # Second coefficient of max climb / takeoff thrust as per BADA 3
CT3 <- .10941E-09                                                               # Third coefficient of max climb / takeoff thrust as per BADA 3

################################################################################
# Constants definition                                                         #
################################################################################

g <- 32.174                                                                     # Gravitational acceleration constant in slugs per cubic foot, assuming a non-oblate, non-rotating Earth

################################################################################
# Variables definition - Atmosphere                                            #
################################################################################

rho <- .002377                                                                  # Air density in slugs per cubic foot

w   <- 0                                                                        # Starting headwind in knots

################################################################################
# Variable definition - Runway                                                 #
################################################################################

mu    <- .0165                                                                  # Dimensionless coefficient of rolling friction  used in the Blake (2009) example, p. 18-11

theta <- 0                                                                      # Runway slope in degrees. Assumed to be zero as a positive slope in one runway heading would cancel out the negative slope in the reciprocal heading

Hp  <- 0                                                                        # Geopotential pressure altitude in feet

################################################################################
# Calculate the acceleration at each TAS increment                             #
################################################################################

acc <- function(x) {
  q <- (rho["i"] * (x * 1.68781)^2) / 2
  g["i"] / W["i"] * (T["i",] * 2 - (mu["blake"] * W["i"]) - (CD - (mu["blake"] * CL["blake"])) * (q * S["i"]) - (W["i"] * sin(theta)) )
}

acc(V)



Tmax <- CT1 * ( 1 - ( Hp / CT2 ) + ( CT3 * Hp^2 ) ) * .224809 # Last term is to convert newtons to pounds

Tmax

thrust <- function(CT1, CT2, CT3, Hp) {
  CT1 * ( 1 - ( Hp / CT2 ) + ( CT3 * Hp^2 ) ) * .224809
}

thrust(CT1, CT2, CT3, Hp)







