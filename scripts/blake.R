################################################################################
# blake.R                                                                      #
# Replicates the takeoff performance model from Blake (2009), p. 18-12         #
################################################################################

library(ggplot2)

# Clear the console
cat("\014")

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
# Variables definition - Aircraft                                              #
################################################################################

W <- 240000                                                                     # Aircraft weight in pounds

S     <- 1951                                                                   # Reference wing area in square feet

CL    <- 1.6087                                                                 # Dimensionless lift coefficient

CD    <- .08 + mu * CL                                                          # Dimensionless drag coefficient (adjusted so that CD - mu * CL = .08 as per Blake's (2009) example)

################################################################################
# Variables definition - Speeds                                                #
################################################################################

GS      <- c(0, 20, 40, 60, 80, 100, 120, 140, 150, 160)                        # Groundspeed increments (in knots) at which the acceleration is calculated
TAS     <- GS + w                                                               # True airspeed (groundspeed + wind)

################################################################################
# Variables definition - Engines                                               #
################################################################################

# T   <- 35532
T     <- c("0" = 35532, "20" = 34653, "40" = 33775, "60" = 32896, "80" = 32014, "100" = 31139, "120" = 30260, "140" = 29381, "150" = 28942, "160" = 28503) # Thrust of each engine in pounds for a given true airspeed

################################################################################
# Estimate the decrease in thrust from an airspeed increase in Table 18-2      #
################################################################################

# Values for thrust T of each engine (in pounds) for increments in true airspeed TAS
T <- c(35532, 34653, 33775, 32896, 32014, 31139, 30260, 29381, 28942, 28503)    # Thrust
TAS <- c(0, 20, 40, 60, 80, 100, 120, 140, 150, 160)                            # True airspeed
d <- data.frame(T, TAS)

# Build the model
model <- lm(d) # Linear regression model to estimate by how much the thrust in pounds decreases for every one extra knot in airspeed
coef(model)[2] # For every knot of change in TAS, the thrust T changes by this much

# Use the model to predict thrust at every TAS
T_hat <- vector(length = length(T)) # Initialize the estimate vector, of the same length as T
T_hat[1] <- T[1]  # Starting thrust at zero TAS
for (i in 2:10) {
  T_hat[i] <- T_hat[i-1] + (TAS[i] - TAS[i-1]) * coef(model)[2] # Estimate thrust at each TAS
  T_hat[i] <- round(T_hat[i], digits = 0)                       # Round off the thrust value
}

# Correlation coefficient of actual and estimated thrusts
cor(T, T_hat) # The near-perfect correlation is almost too good to be true for the Blake values of T to be empirical, and suggests that Blake assumed a fixed value equal coef(model)[2] to model thrust decrease over TAS

# Display the results in tabular form
d_hat <- data.frame(TAS, T, T_hat, res = T - T_hat) # Original and estimated values side-by-side with residual error in units of pounds
d_hat

# Display the results in chart form
ggplot(data = d_hat) +
  geom_col(aes(x = TAS, y = T)) +       # Bar chart of Blake's values
  geom_line(aes(x = TAS, y = T_hat)) +  # Line chart of model predictions
  geom_point(aes(x = TAS, y = T_hat)) + # Points of model predictions
  xlab("true airspeed - knots") +
  ylab("thrust - pounds")

################################################################################
# Acceleration calculation                                                     #
################################################################################

q     <- (rho * (TAS * 1.68781)^2) / 2                                          # Dynamic pressure. The speed is converted to feet per second

a     <- g / W * (T * 2 - (mu * W) - (CD - (mu * CL)) * (q * S) - (W * sin(theta))) # Acceleration in feet per second per second

################################################################################
# Display results in a table as per page 18-12 of Blake (2009)                 #
################################################################################

d <- cbind("GS - knots" = GS, "TAS - knots" = TAS, "Dynamic pressure" = round(q, digits = 2), "F - thrust" = T, "F - slope" = theta, "μW" = mu * W, "(CD-μCL)qS" = round((CD - (mu * CL)) * (q * S), digits = 0), "accel ft/sec/sec" = round(a, digits = 1), "accel knots/sec" = round(a / 1.68781, digits = 1))

rownames(d) <- NULL                                                             # Remove row names from the d matrix

d                                                                               # Output results to the console

d_plot <- data.frame(TAS, a)                                                    # Prepare a data frame for plotting

ggplot(data = d_plot, aes(x = TAS, y = a)) +                                    # Replicate table 18-1 of Blake (2009)
  geom_line() +
  geom_point() +
  xlab("true airspeed - knots") +
  ylab("acceleration - ft/sec/sec")

################################################################################
# Step integration of distance as per Table 18-2 of Blake (2019)               #
################################################################################

cum <- 0                                                                        # Initialize the cumulative takeoff distance

out1 <- array(                                                                   # Initialize an array to display the results later
  dim = c(0, 5),
  dimnames = list(NULL, c("GS - knots", "TAS - knots", "accel ft/sec/sec", "s - feet", "Σs - feet"))
)

for (i in 1:length(GS)) {                                                       # For each groundspeed increment in the Blake (2009) example in Table 18-2
  avg <- ifelse(GS[i] == 0, GS[i], mean(c(GS[i-1], GS[i])))                     # Calculate the average speed (i.e., midpoint speed), skipping the zero speed row
  inc <- ifelse(GS[i] == 0, GS[i], GS[i] - GS[i-1])                             # Calculate the speed increment (i.e., new speed minus previous speed), skipping the zero speed row
  acc <- mean(c(a[i-1], a[i]))                                                  # Calculate the average acceleration (i.e., midpoint acceleration)
  dis <- avg * inc * 1.6878^2 / acc                                             # Calculate the distance covered in that increment (converting speed from knots to feet/s)
  cum <- cum + dis                                                              # Increment the cumulative (total) distance by that incremental distance
  out1 <- rbind(out1, c(GS[i], TAS[i], round(a[i], digits = 1), round(dis, digits = 0), round(cum, digits = 0))) # Save incremental results for later display to the console
}

out1                                                                             # Output results to the console

################################################################################
# Add wind effects as per Table 18-10a of Blake (2019)                         #
################################################################################

# Blake (2009) is not consistent in the speed increments across examples, so we have to redefine the key values used in the takeoff distance calculations

w   <- 20                                                                       # Now assume a positive headwind in knots
GS  <- c(0, 20, 40, 60, 80, 100, 120, 130)                                      # Use new groundspeed increments (in knots) at which the acceleration is calculated
TAS <- GS + w                                                                   # Add the headwind to the airspeed
# T   <- 35532
T   <- c(34653, 33775, 32896, 32017, 31139, 30260, 29381, 28942)                # Impute new thrust values given by Blake (2009) for the new airspeed
q   <- (rho * (TAS * 1.68781)^2) / 2                                            # Update the dynamic pressure. The speed is converted to feet per second
a   <- g / W * (T * 2 - (mu * W) - (CD - (mu * CL)) * (q * S) - (W * sin(theta))) # Acceleration in feet per second per second

d <- cbind("GS - knots" = GS, "TAS - knots" = TAS, "Dynamic pressure" = round(q, digits = 2), "F - thrust" = T, "F - slope" = theta, "μW" = mu * W, "(CD-μCL)qS" = round((CD - (mu * CL)) * (q * S), digits = 0), "accel ft/sec/sec" = round(a, digits = 1), "accel knots/sec" = round(a / 1.68781, digits = 1))

rownames(d) <- NULL                                                             # Remove row names from the d matrix

d                                                                               # Output results to the console

# Now we calculate the effect of headwind on distance as per Table 18-10b of Blake (2009)                                                                        # Initialize the cumulative takeoff distance

cum2 <- 0                                                                        # Initialize the cumulative takeoff distance

out2 <- array(                                                                   # Initialize an array to display the results later
  dim = c(0, 5),
  dimnames = list(NULL, c("GS - knots", "TAS - knots", "accel ft/sec/sec", "s - feet", "Σs - feet"))
)

for (i in 1:length(GS)) {                                                       # For each true speed increment in the Blake (2009) example in Table 18-2
  avg  <- ifelse(GS[i] == 0, 0, mean(c(GS[i-1], GS[i])))                        # Calculate the average speed (i.e., midpoint speed), skipping the zero speed row
  inc  <- ifelse(GS[i] == 0, 0, GS[i] - GS[i-1])                                # Calculate the speed increment (i.e., new speed minus previous speed), skipping the zero speed row
  acc  <- mean(c(a[i-1], a[i]))                                                 # Calculate the average acceleration (i.e., midpoint acceleration)
  dis  <- avg * inc * 1.6878^2 / acc                                            # Calculate the distance covered in that increment (converting speed from knots to feet/s)
  cum2 <- cum2 + dis                                                            # Increment the cumulative (total) distance by that incremental distance
  out2  <- rbind(out2, c(GS[i], TAS[i], round(a[i], digits = 1), round(dis, digits = 0), round(cum2, digits = 0))) # Save incremental results for later display to the console
}

out2                                                                            # Output results to the console

s1 <- out1[which(out1[,"TAS - knots"] == 150),"Σs - feet"]                              # Pull distance covered at 150 knots airspeed with no headwind
s2 <- out2[which(out2[,"TAS - knots"] == 150),"Σs - feet"]                            # Pull distance covered at 150 knots airspeed with headwind

s1 - s2                                                                         # Takeoff distance avoidance thanks to headwind

s1
s2

# s1 = 4630 feet, no wind, decreasing thrust
# s2 = 3546 feet, 20 knot headwind, decreasing thrust
# 
# s1 = 3921 feet, no wind, constant thrust
# s2 = 2971 feet, 20 knot headwind, constant thrust
# 
# s1 = 709 feet longer with decreasing thrust (18.08% longer than constant thrust)
# s2 = 575 feet longer with decreasing thrust (19.35% longer than constant thrust)























