
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/sars/merged/processed"

# Read Pacific
pac_orig <- readRDS("data/sars/pacific/processed/Pacific_SARs_parameters.Rds")

# Read Alaska
ak_orig <- readRDS("data/sars/alaska/processed/Alaska_SARs_parameters.Rds")

# Read Atlantic
atl_orig <- readRDS("data/sars/atlantic/processed/Atlantic_SARs_parameters.Rds")


# Format
################################################################################

# 
pac <- pac_orig %>% 
  filter(year==2023)



# Merge
################################################################################


# Export
################################################################################
