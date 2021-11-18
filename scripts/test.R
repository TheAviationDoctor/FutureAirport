# Set air density under ISA
rho <- 1.225

# Set iterations
int <- 10

# Set headwind
hdw <- 0

# Set mass
m <- 78000

# Set wing surface area
S <- 121

# Set gravitational constant
g <- 9.806665

# Set a speed range
Vtas <- seq(from = 0 + hdw, to = 100, length = const$int)

# Calculate the dynamic pressure
q <- .5 * rho * Vtas^2

q

# Calculate the coefficient of lift in non-clean configuration
cL <- m * g / ( q * S )

cL

# Calculate lift for each speed
L <- q * S * cL

L

# Calculate takeoff speed
# Vtko <- sqrt( (m * g) / (.5 * S * rho * cL) )

# Vtko

q * cL

plot(Vtas, q)



