################################################################################
#    NAME: scripts/6.1_blake.R                                                 #
#   INPUT: Hardcoded values from Blake (2009)                                  #
# ACTIONS: Replicates the takeoff performance model from Blake (2009), p. 18-12#
#          Models the thrust decrease vs. airspeed increase                    #
#  OUTPUT: Coefficient of thrust decreased for every airspeed increase         #
# RUNTIME: <1 second on the researcher's config (https://bit.ly/3ChCBAP)       #
################################################################################

################################################################################
# Housekeeping                                                                 #
################################################################################

# Load required libraries
library(ggplot2)

# Import the common settings
source("scripts/0_common.R")

# Start a script timer
start_time <- Sys.time()

# Clear the console
cat("\014")

################################################################################
# Calculate model coefficient                                                  #
################################################################################

# Set Blake's values for true airspeed increments (in knots) at takeoff
V_Blake <- c(0, 20, 40, 60, 80, 100, 120, 140, 150, 160)

# Set Blake's values for each engine's decrement (in pounds) at takeoff
T_Blake <- c(35532, 34653, 33775, 32896, 32014, 31139, 30260, 29381, 28942, 28503)

# Assemble the data frame
d <- data.frame(T_Blake, V_Blake)

# Build a linear regression model to estimate by how much the thrust decreases for every increment in airspeed
model <- lm(d)

# Display the change in thrust for every knot of change in V
coef(model)[2]

################################################################################
# Confirm that the model predicts the decrease in T for every increase in V    #
################################################################################

# Initialize the estimate vector, of the same length as T
T_hat <- vector(length = length(T_Blake))

# Set the starting thrust at zero V
T_hat[1] <- T_Blake[1]

# For each airspeed increment above zero, estimate the thrust based on the model's coefficient, and round off each result
for (i in 2:length(V_Blake)){
  T_hat[i] <- T_hat[i-1] + (V_Blake[i] - V_Blake[i-1]) * coef(model)[2]
  T_hat[i] <- round(T_hat[i], digits = 0)
}

# Correlation coefficient of actual and estimated thrusts
cor(T_Blake, T_hat)

# Store the original and estimated values side-by-side with residual error in units of pounds
d_hat <- data.frame(V_Blake, T_Blake, T_hat, res = T_Blake - T_hat)

# Display the results in tabular form
d_hat

# Compare the original thrust values from Blake (2009) in a bar chart and the model's predictions in a line chart
ggplot(data = d_hat) +
  geom_col(aes(x = V_Blake, y = T_Blake)) +
  geom_line(aes(x = V_Blake, y = T_hat)) +
  geom_point(aes(x = V_Blake, y = T_hat)) +
  xlab("true airspeed - knots") +
  ylab("thrust - pounds")

################################################################################
# Housekeeping                                                                 #
################################################################################

# Display the script execution time
Sys.time() - start_time

# EOF