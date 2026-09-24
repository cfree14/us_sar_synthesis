
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/sars/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(data, file=file.path(outdir, "US_sars_data.Rds")) 


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to 2024
  filter(year==2024) 

# Count
table(data$esa_status)

# Examine moree
esa_spp <- data %>% 
  filter(esa_status!="Not listed")

