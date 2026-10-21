
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/Whales/us_sar_synthesis_data/atlantic/tables_ngom_bse_bottlenose_dolphins"
keydir <- "data/sars/keys"
outdir <- "data/sars/processed"

# Species key
species_key <- readxl::read_excel("data/species_key.xlsx")

# Read data
data_orig <- readxl::read_excel("/Users/cfree/Library/CloudStorage/Dropbox/Whales/us_sar_synthesis_data/usfws/usfws_sars.xlsx") %>% 
  # Add species
  rename(comm_name=species) %>% 
  # Add stock
  mutate(stock=paste0(comm_name, " (", area, ")")) %>% 
  # Add revised y/n
  mutate(revised_yn="yes") %>% 
  # Arrrange
  select(stock, group, comm_name, species, region, area, year, everything())

# Add species

# Expand
################################################################################

# Build template
stocks <- freeR::uniq(data_orig$stock)
data_temp <- expand.grid(stock=stocks,
                         year=1995:2024) %>% 
  arrange(stock, year)

# Build data
data <- data_temp %>% 
  # Add data
  left_join(data_orig) %>% 
  # Fill data
  group_by(stock) %>% 
  fill(everything(), .direction="down") %>% 
  # Add "no" to revised y/n
  mutate(revised_yn=ifelse(is.na(revised_yn), "no", revised_yn)) %>% 
  # Mark first year
  mutate(year1=min(year[!is.na(comm_name)])) %>% 
  filter(year>=year1) %>% 
  # Ungroup
  ungroup()

# Inspect
freeR::complete(data)

# Export data
saveRDS(data, file=file.path(outdir, "USFWS_SARs_parameters.Rds"))




