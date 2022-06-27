# Load the required libraries
library(data.table)
library(DBI)
library(parallel)
library(stringr)

# Import the common settings
source("scripts/0_common.R")

# Run the import script
source("scripts/4_import.R")

# Clear the environment
rm(list = ls())

# Garbage collect
gc()

# Run the transform script
source("scripts/5_transform.R")

# Clear the environment
rm(list = ls())

# Garbage collect
gc()