################################################################################
# scripts/0_constants.R                                                        #
# Sets values of various constants used across the scripts                     #
################################################################################

################################################################################
# Population and sample setting                                                #
################################################################################

# Input file locations
pop_geo <- "data/population/geolocation.csv" # Geolocated airports
pop_rwy <- "data/population/runways.csv"     # Runway data
pop_tra <- "data/population/traffic.csv"     # Traffic by airport

# Traffic threshold for the sample (in passengers per annum)
pop_thr <- 10^6

################################################################################
# Climate settings                                                             #
################################################################################

# Choice of CMIP6 experiments (Shared Socioeconomic Pathways, or SSPs)
nc_exps <- c("ssp126", "ssp245", "ssp370", "ssp585")

# Output of the ESGF search for NetCDF files
nc_esgf <- "data/climate/esgf/esgf.csv"

# Path to the NetCDF files
nc_path <- "data/climate/netcdf"

# Latitudinal boundaries for the Earth's climate zones
nc_lats <- list(
  list(name = "Tropical",   lower = 0,       upper = 23.4365),
  list(name = "Subropical", lower = 23.4365, upper = 30), # Defined in Cortlett (2013) [https://doi.org/gw6j]
  list(name = "Temperate",  lower = 23.4365, upper = 66.5635),
  list(name = "Frigid",     lower = 66.5635, upper = 90),
  list(name = "All",        lower = 0,       upper = 90)
)

################################################################################
# Log files                                                                    #
################################################################################

log_net <- "logs/netcdf.log"    # Log file for 4_netcdf.R
log_rho <- "logs/rho.log"       # Log file for 5_rho.R
log_par <- "logs/rho_plot.log"  # Log file for 6_rho_plot.R
log_wnd <- "logs/wnd.log"       # Log file for 7_headwind.R

################################################################################
# Database parameters                                                          #
################################################################################

# Set the file name that contains the database connection parameters
db_cnf <- ".my.cnf"

# Set the group name within the cnf file that contains the connection parameters
db_grp <- "phd"

# Set the table names and prefixes
db_pop <- "pop" # population and sample airports
db_rho <- "rho" # Air density observations
db_wnd <- "wnd" # Wind observations

################################################################################
# Natural constants                                                            #
################################################################################

# Gravitational acceleration, assuming a non-oblate, non-rotating Earth (Blake, 2009; Daidzic, 2016)
g     <- c(
  "i" = 32.174,  # In slugs per cubic foot
  "m" = 9.806665 # In meters per second per second
)

# Air density at sea level under ISA conditions
rho   <- c(
  "i" = 0.0765, # In pounds per cubic foot
  "m" = 1.225   # In kilograms per cubic meter
)

################################################################################
# Runway assumptions                                                           #
################################################################################

mu    <- c(                                                                     # Dimensionless coefficient of rolling friction. ESDU 85029 (p. 32) suggests .02 as a typical value. Values below are taken from Filippone (2012), Table 9.3
  "blake" = .0165,                                                              # Value used in the Blake (2009) example, p. 18-11
  "dca"   = .02,                                                                # Value for dry concrete/asphalt
  "htg"   = .04,                                                                # Value for hard turf and gravel
  "sdg"   = .05,                                                                # Value for short and dry grass
  "lg"    = .10,                                                                # Value for long grass
  "sg_lo" = .10,                                                                # Value for soft ground (low softness)
  "sg_mi" = .20,                                                                # Value for soft ground (medium softness)
  "sg_hi" = .30                                                                 # Value for soft ground (high softness)
)

theta <- 0                                                                      # Runway slope in degrees. Assumed to be zero as a positive slope in one runway heading would cancel out the negative slope in the reciprocal heading

Hp  <- 0                                                                        # Geopotential pressure altitude in feet

################################################################################
# Unit system                                                                  #
################################################################################

u     <- "i"                                                                    # Choose "i" for imperial, "m" for metric

# EOF