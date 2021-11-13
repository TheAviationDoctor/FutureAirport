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
log_6 <- "logs/6_takeoff.log"   # For 6_takeoff.R

################################################################################
# Database parameters                                                          #
################################################################################

# Set the file name that contains the database connection parameters
db_cnf <- ".my.cnf"

# Set the group name within the cnf file that contains the connection parameters
# This is also the schema name
db_grp <- "phd"

# Set the table prefixes and names
db_pop <- "pop" # Population and sample airports resulting from 1_population.R
db_imp <- "imp" # Climate data imported from the NetCDF files in long format resulting from 4_import.R
db_cli <- "cli" # Climate data transformed in wide/tidy format resulting from 5_transform.R
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
# References                                                                   #
################################################################################

# Filippone, A. (2012). Advanced Aircraft Flight Performance. https://doi.org/gdjz
# Sun et al. (2018). Aircraft Drag Polar Estimation Based on a Stochastic Hierarchical Model. Eighth SESAR Innovation Days, 3rd – 7th December 2018. https://www.sesarju.eu/sites/default/files/documents/sid/2018/papers/SIDs_2018_paper_75.pdf
# Sun et al. (2020). OpenAP: An Open-Source Aircraft Performance Model for Air Transportation Studies and Simulations. https://doi.org/g2tj

# EOF