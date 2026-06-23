
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/sars/processed"

# Read Pacific
pac_orig <- readRDS(file.path(outdir, "Pacific_SARs_parameters.Rds"))

# Read Alaska
ak_orig <- readRDS(file.path(outdir, "Alaska_SARs_parameters.Rds"))

# Read Atlantic
atl_orig <- readRDS(file.path(outdir, "Atlantic_SARs_parameters.Rds"))


# Format
################################################################################

# Columns
colnames(pac_orig)
colnames(atl_orig)
colnames(ak_orig)

# Ultimate goal:
# year, group, comm_name, species, area,
# n_est, n_vc, n_min, r_max, rf, pbr,
# sim_total, sim_fisheries, strategic_yn, revised_yn, comments

# Pacific
pac <- pac_orig %>% 
  # Add 
  mutate(region1="Pacific") %>% 
  # Rename
  rename(sim_total=sim_tot,
         sim_fisheries=sim_fish,
         comments=notes) %>% 
  # Simplify
  select(region1, filename, 
         year, group, comm_name, species, area,
         n_est, n_min, r_max, rf, pbr,
         sim_total, sim_fisheries, 
         strategic_yn, revised_yn, comments)

# Atlantic
atl <- atl_orig %>% 
  # Add 
  mutate(region1="Atlantic") %>% 
  # Rename
  rename(n_est=n,
         sim_total=msi_total,
         sim_fisheries=msi_fisheries) %>% 
  # Simplify
  select(region1, filename, 
         year, group, comm_name, species, area,
         n_est, n_min, r_max, rf, pbr,
         #sim_total, sim_fisheries, # not numeric yet
         strategic_yn, revised_yn, comments)

# Atlantic
ak <- ak_orig %>% 
  # Add 
  mutate(region1="Alaska") %>% 
  # Rename
  rename(revised_yn=updated_yn) %>% 
  # Simplify
  select(region1, filename, 
         year, group, comm_name, species, area,
         n_est, n_cv, n_min, r_max, rf, pbr,
         sim_total, sim_fisheries, strategic_yn, revised_yn, comments)



# Merge
################################################################################

# Merge
data <- bind_rows(pac, atl, ak) %>% 
  # Add stock
  mutate(stock=paste0(comm_name, " (", area, ")")) %>% 
  # Arrange
  select(region1:group, stock, everything())

# Inspect
str(data)
freeR::complete(data)


# Export
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "US_sars_data.Rds"))



