
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
# region, subregion, year, group, comm_name, species, area,
# n_est, n_vc, n_min, r_max, rf, pbr,
# sim_total, sim_fisheries, strategic_yn, revised_yn, comments

# Pacific
pac <- pac_orig %>%
  # Region
  rename(subregion=region) %>% 
  mutate(region="Pacific") %>% 
  # Rename
  rename(sim_total=sim_tot,
         sim_fisheries=sim_fish,
         comments=notes) %>% 
  # Simplify
  select(region, subregion, filename, 
         year, group, comm_name, species, area,
         n_est, n_cv, n_min, r_max, rf, pbr,
         sim_total, sim_fisheries, 
         strategic_yn, revised_yn, comments,
         # 2024 ones
         osp_status, esa_status, mnpl)

# Atlantic
atl <- atl_orig %>% 
  # Region
  rename(subregion=region) %>% 
  mutate(region="Atlantic") %>% 
  # Rename
  rename(n_est=n,
         sim_total=msi_total,
         sim_fisheries=msi_fisheries) %>% 
  # Simplify
  select(region, subregion, filename, 
         year, group, comm_name, species, area,
         n_est, n_cv, n_min, r_max, rf, pbr,
         sim_total, sim_fisheries, 
         strategic_yn, revised_yn, comments)
         # 2024 ones
         # osp_status, esa_status, mnpl)

# Alaska
ak <- ak_orig %>% 
  # Region
  mutate(region="Alaska",
         subregion=region) %>% 
  # Rename
  rename(revised_yn=updated_yn) %>% 
  # Simplify
  select(region, subregion, filename, 
         year, group, comm_name, species, area,
         n_est, n_cv, n_min, r_max, rf, pbr,
         sim_total, sim_fisheries, strategic_yn, revised_yn, comments,
         # 2024 ones
         osp_status, esa_status, mnpl)



# Merge
################################################################################

# Merge
data <- bind_rows(pac, atl, ak) %>% 
  # Add stock
  mutate(stock=paste0(comm_name, " (", area, ")")) %>% 
  # Format revised_yn
  mutate(revised_yn=recode(revised_yn, 
                           "Revised" = "yes",
                           "Same as previous" = "no")) %>% 
  # Reassign ENP gray whale to Pacific region (in Alaska 1995-2011)
  # Reassign ENP Transient killer whale to Alaska (in West Coast 1999-2001)
  mutate(region=case_when(stock=="Gray whale (Eastern North Pacific)" ~ "Pacific",
                          stock=="Killer whale (ENP Transient)" ~ "Alaska",
                          T ~ region),
         subregion=case_when(stock=="Gray whale (Eastern North Pacific)" ~ "West Coast",
                             stock=="Killer whale (ENP Transient)" ~ "Alaska",
                             T ~ subregion)) %>% 
  # Check PBR
  mutate(pbr_calc=n_min*r_max/2*rf,
         pbr_diff=abs(pbr-pbr_calc) %>% round(., 2)) %>% 
  # Arrange
  select(region:group, stock, comm_name, species, area, revised_yn, 
         n_est, n_cv, n_min, r_max, rf, 
         pbr, pbr_calc, pbr_diff,
         sim_total, sim_fisheries, strategic_yn, comments,
         # 2024 ones
         osp_status, esa_status, mnpl, everything())


# Inspect
################################################################################

# Inspect
str(data)
freeR::complete(data)

# Confirm 1 row per stock year
data %>% 
  group_by(stock, year) %>% 
  summarize(n=n()) %>% filter(n>1)

# Region
table(data$region)
table(data$subregion)

# N_CV
# N_cv=0 appears to be true for NA right whale, Gulf of ME humpback whale, and 
range(data$n_cv, na.rm = T) # CV=0 real?
table(data$n_cv)

# RMAX - 0 values and 0.2 values real?
# The North Atlantic right whale Rmax was 0 from 2000-2009
# The WA Northern sea otter has RMAX=0.2
table(data$r_max)

# RF
# All range from 0.1 - 1 as expected
table(data$rf)

# Check PBR calculations
# The 2006 Eastern Steller sea lion PBR was calculated incorrectly (2000 instead of 2004)
# The 2009-10 GoM/BoF harbor porpoise PBR should be 703 and not 701 (arithmetic incorrect in SAR)
pbr_check <- data %>% 
  filter(pbr_diff>1)

# Confirm that SIM total is more than SIM fisheries
# Ultimately, need SIM native in there
sim_check <- data %>% 
  mutate(fish_larger=sim_fisheries > sim_total) %>% 
  filter(fish_larger==T)

# Confirm that Nmin is less than Nest
ncheck <- data %>% 
  mutate(nmin_larger=n_min > n_est) %>% 
  filter(nmin_larger==T)

# Status
table(data$strategic_yn)

# Revised
table(data$revised_yn)


# Export
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "US_sars_data.Rds"))



