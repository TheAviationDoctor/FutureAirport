# Clear the console
cat("\014")


const <- list("g" = 9.806665)

fn_sim_tst <- function(cD0, k, lambda_f, cfc, SfS, flap_angle, m, S, span, rho) {
  
  # Calculate the drag coefficient component attributable to flaps
  delta_cD_flaps <- lambda_f * cfc^1.38 * SfS * sin(flap_angle * pi / 180)^2
  
  # Calculate the drag coefficient component attributable to the landing gear
  delta_cD_gear <- m * const$g / S * 3.16E-5 * m^-.215
  
  # Calculate the total drag coefficient in non-clean configuration
  cD0_total <- cD0 + delta_cD_flaps + delta_cD_gear
  
  # Calculate the Oswald efficiency factor for the selected flap deflection (for wing-mounted engines)
  delta_e_flaps <- .0026 * flap_angle
  
  # Calculate the aspect ratio
  ar <- span^2 / S
  
  # Calculate the lift-induced coefficient k in non-clean configuration
  k_total <- 1 / (1 / k + pi * ar * delta_e_flaps)
  
  # Set the speed
  # Vtas <- seq(from = 0, to = 98.6867, length = 10)
  Vtas <- 85.29 # mean
  # Vtas <- 95.6867 # 1 SD
  
  # Calculate the dynamic pressure
  q <- .5 * rho * Vtas^2
  
  # Calculate the lift at lift-off (L = W)
  L <- m * const$g
  # L <- seq(from = 0, to = m * const$g, length = 10)
  
  # Calculate the coefficient of lift in non-clean configuration
  cL <- L / ( q * S )
  
  # Calculate the coefficient of drag in non-clean configuration
  cD <- cD0_total + k_total * cL^2
  
  # Calculate the drag
  D <- cD * q * S
  
  # Assemble the results
  c <- list("D" = cD, "L" = cL)
  
  mu <- .0165
  
  # Return the results
  print(cL)
  print(cD)
  print(cD - mu * cL)
  print(L)
  print(m * const$g)
  
}

fn_sim_tst(cD0 = .017, k =.038, lambda_f = .9, cfc = .176, SfS = .17, flap_angle = 15, m = 79000, S = 124, span = 35.8, rho = 1.225)