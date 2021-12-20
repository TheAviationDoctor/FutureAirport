################################################################################
#    NAME: scripts/0_common.R                                                  #
#   INPUT: None                                                                #
# ACTIONS: Set common settings used across the scripts                         #
#  OUTPUT: Set of global constants loaded into R's environment                 #
# RUNTIME: <1 second on the researcher's config (https://bit.ly/3ChCBAP)       #
################################################################################

################################################################################
# Project directories                                                          #
################################################################################

# Directory paths
path_aer <- "data/aer" # Aeronautical data (aircraft and engines)
path_oem <- "data/oem" # Takeoff performance data (OEM-calibrated)
path_pop <- "data/pop" # Population data (airports and runways)
path_cli <- "data/cli" # Climate data (from CMIP6)
path_plt <- "plots"    # Generated plots

################################################################################
# Project files                                                                #
################################################################################

# File names
aer_act <- "aircraft.csv"    # Aircraft characteristics
aer_mu  <- "mu.csv"          # Runway friction coefficients
pop_geo <- "geolocation.csv" # Airport locations
pop_rwy <- "runways.csv"     # Runways
pop_tra <- "traffic.csv"     # Airport traffic
cli_esg <- "esgf.csv"        # Output of the ESGF search

################################################################################
# Log files                                                                    #
################################################################################

log_4 <- "logs/4_import.log"    # For 4_import.R
log_5 <- "logs/5_transform.log" # For 5_transform.R
log_7 <- "logs/7_calibrate.log" # For 7_calibrate.R
log_8 <- "logs/8_simulate.log"  # For 8_simulate.R

################################################################################
# Database parameters                                                          #
################################################################################

# Set the file name that contains the database connection parameters
db_cnf <- ".my.cnf"

# Set the group name within the cnf file that contains the connection parameters
# This is also the schema name
db_grp <- "phd"

# Set the table prefixes and names
db_act <- "act" # Aircraft characteristics for the takeoff simulation
db_cal <- "cal" # Calibration data used in 6_calibration.R
db_cli <- "cli" # Climate data transformed in wide/tidy format resulting from 5_transform.R
db_imp <- "imp" # Climate data imported from the NetCDF files in long format resulting from 4_import.R
db_pop <- "pop" # Population and sample airports resulting from 1_population.R
db_tko <- "tko" # Takeoff performance calculation outputs resulting from 6_takeoff.R

################################################################################
# Airport sample settings                                                      #
################################################################################

# Airport traffic threshold for the sample (in passengers per annum)
pop_thr <- 10^6

################################################################################
# Climate settings                                                             #
################################################################################

# CMIP6 experiments (Shared Socioeconomic Pathways, or SSPs)
nc_exps <- c("ssp126", "ssp245", "ssp370", "ssp585")

################################################################################
# Takeoff simulation constants and settings                                    #
################################################################################

sim <- list(
  # Natural constants
  "g"          = 9.806665,          # Gravitational acceleration constant in m/s², assuming a non-oblate, non-rotating Earth (Blake, 2009; Daidzic, 2016)
  "ms_to_kt"   = 1.9438445,         # Factor to convert speed from m/s to kt
  "m_to_ft"    = 3.280839895,       # Factor to convert distance from m to ft
  "gamma"      = 1.401,             # Adiabatic index (a.k.a., heat capacity ratio) for dry air
  "ps_isa"     = 101325L,           # Air pressure in Pa at sea level under international standard atmospheric conditions
  "Rd"         = 287.058,           # Specific gas constant for dry air in J/(kg·K)
  # Runway constants
  "mu"         = .02,               # Dimensionless coefficient of friction for dry concrete/asphalt at the runway-tire interface (ESDU 85029, p. 32)
  "theta"      = 0L,                # Runway slope in °
  # Regulatory constants
  "reg_dis"    = 115L,              # Percent of the horizontal distance along the takeoff path, with all engines operating, from the start of the takeoff to a point equidistant between the point at which VLOF is reached and the point at which the airplane is 35 feet above the takeoff surface, according to 14 CFR § 25.113 (1998)
  "reg_rto"    = 25L,               # Maximum percentage of takeoff thrust reduction permissible FAA Advisory Circular 25-13 (1988)
  # Calibration constants
  "hurs"       = 0L,                # Sea-level relative humidity in % at ISA
  "ps"         = 101325,            # Sea-level air pressure in Pa at ISA
  "tas"        = 273.15,            # Sea-level air temperature in K at ISA
  "rho"        = 1.225,             # Sea-level air density in kg/m³ at ISA
  "hdw"        = 0L,                # Headwind in m/s
  # Simulation settings
  "aircraft"   = c("A320", "B789"), # Aircraft to simulate
  "flap_angle" = 10L,               # Flap deflection angle in takeoff configuration
  "int"        = 10L                # Simulation resolution / number of integration steps
)

################################################################################
# References                                                                   #
################################################################################

# Airbus (2020). A320 Aircraft Characteristics, Airport And Maintenance Planning.
# Boeing (2018). 787 Airplane Characteristics for Airport Planning.
# Blake, W. (2009). Jet Transport Performance Methods.
# Filippone, A. (2012). Advanced Aircraft Flight Performance. https://doi.org/gdjz
# Sun et al. (2018). Aircraft Drag Polar Estimation Based on a Stochastic Hierarchical Model. Eighth SESAR Innovation Days, 3rd – 7th December 2018. https://www.sesarju.eu/sites/default/files/documents/sid/2018/papers/SIDs_2018_paper_75.pdf
# Sun et al. (2020). OpenAP: An Open-Source Aircraft Performance Model for Air Transportation Studies and Simulations. https://doi.org/g2tj

# EOF