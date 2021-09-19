################################################################################
# 0_constants.R                                                                #
# Values for various constants used across scripts                             #
################################################################################

################################################################################
# Gravitational acceleration                                                   #
################################################################################

g     <- c(                                                                     # Assuming a non-oblate, non-rotating Earth (Blake, 2009; Daidzic, 2016)
  "i" = 32.174,                                                                 # In slugs per cubic foot
  "m" = 9.806665                                                                # In meters per second per second
)

################################################################################
# Atmosphere                                                                   #
################################################################################

rho   <- c(                                                                     # Air density
  "i" = 0.0765,                                                                 # In pounds per cubic foot
  "m" = 1.225                                                                   # In kilograms per cubic meter
)

