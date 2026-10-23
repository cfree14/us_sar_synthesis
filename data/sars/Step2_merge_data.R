
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
atl_orig <- readRDS(file.path(outdir, "Atlantic_SARs_parameters_final.Rds"))

# Read USFWS
usfws_orig <- readRDS(file.path(outdir, "USFWS_SARs_parameters.Rds"))


# Format
################################################################################

# Columns
colnames(pac_orig) # sim_fisheries_cv, last_survey_yr
colnames(atl_orig) # msi_fisheries_cv, last_survey_yr
colnames(ak_orig) # sim_fisheries_cv, last_survey_yr
colnames(usfws_orig)

# Ultimate goal:
# region, subregion, year, group, comm_name, species, area,
# last_survey_yr, n_est, n_vc, n_min, r_max, rf, pbr,
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
         last_survey_yr, n_est, n_cv, n_min, r_max, rf, pbr,
         sim_total, sim_fisheries, sim_fisheries_cv,
         strategic_yn, revised_yn, comments,
         # 2024 ones
         osp_status, esa_status, mnpl, n_method, trend, unused_r) %>% 
  # Remove USFWS
  filter(!grepl("USFWS", group))

str(pac)

# Atlantic
atl <- atl_orig %>% 
  # Region
  rename(subregion=region,
         sim_fisheries_cv=msi_fisheries_cv) %>% 
  mutate(region="Atlantic") %>% 
  # Rename
  rename(sim_total=msi_total,
         sim_fisheries=msi_fisheries) %>% 
  # Simplify
  select(region, subregion, filename, 
         year, group, comm_name, species, area,
         last_survey_yr, n_est, n_cv, n_min, r_max, rf, pbr,
         sim_total, sim_fisheries, sim_fisheries_cv,
         strategic_yn, revised_yn, comments,
         # 2024 ones
         osp_status, esa_status, mnpl, n_method, trend, unused_r) %>% 
  # Remove USFWS
  filter(!grepl("USFWS", group))

str(atl)

# Alaska
ak <- ak_orig %>% 
  # Region
  mutate(region="Alaska",
         subregion=region) %>% 
  # Rename
  rename(revised_yn=updated_yn) %>% 
  # Format last survey year
  mutate(last_survey_yr=stringr::str_sub(last_survey_yr, -4) %>% as.numeric()) %>% 
  # Simplify
  select(region, subregion, filename, 
         year, group, comm_name, species, area,
         last_survey_yr, n_est, n_cv, n_min, r_max, rf, pbr,
         sim_total, sim_fisheries, sim_fisheries_cv, strategic_yn, revised_yn, comments,
         # 2024 ones
         osp_status, esa_status, mnpl, n_method, trend, unused_r) %>% 
  # Remove USFWS
  filter(!grepl("USFWS", group))

str(ak)

# USFWS 
usfws <- usfws_orig %>% 
  # Rename
  rename(comments=notes,
         subregion=region,
         osp_status=mmpa_status) %>% 
  # Region
  mutate(region=recode(subregion,
                       "Gulf of Mexico"="Atlantic",
                       "West Coast"="Pacific")) %>% 
  # Format group
  mutate(group="USFWS") %>% 
  # Format strategic
  mutate(strategic_yn=stringr::str_to_sentence(strategic_yn)) %>% 
  # Format trend
  mutate(trend=recode(trend, 
                      "decreasing"="decrease", 
                      "increasing"="increase")) %>% 
  # Simplify
  select(region, subregion, filename, 
         year, group, comm_name, species, area,
         last_survey_yr, n_est, n_cv, n_min, r_max, rf, pbr,
         sim_total, sim_fisheries, strategic_yn, revised_yn, comments,
         # 2024 ones
         osp_status, esa_status, n_method, # mnpl,
         trend)
         #unused_r)

str(usfws)


# Merge
################################################################################

# Merge
data <- bind_rows(pac, atl, ak, usfws) %>% 
  # Add stock
  mutate(stock=paste0(comm_name, " (", area, ")")) %>% 
  # Format ESA status
  mutate(esa_status=stringr::str_to_sentence(esa_status)) %>%
  # Format OSP status
  mutate(osp_status=stringr::str_to_sentence(osp_status),
         osp_status=recode(osp_status, "Not depleted"="Non-depleted")) %>% 
  # Format strategic
  mutate(strategic_yn=recode(strategic_yn, "yes"="Strategic")) %>% 
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
  # Format trend
  mutate(trend=gsub("\\s*\\([^)]*\\)", "", trend) %>% 
           stringr::str_trim(.) %>% stringr::str_to_sentence(.),
         trend=recode(trend, "Increase; unknown"="Unknown")) %>% 
  # Arrange
  select(region:group, stock, comm_name, species, area, revised_yn, 
         last_survey_yr, n_est, n_cv, n_min, r_max, rf, 
         pbr, pbr_calc, pbr_diff,
         sim_total, sim_fisheries, sim_fisheries_cv, strategic_yn, comments,
         # 2024 ones
         osp_status, esa_status, mnpl, n_method, trend, unused_r, everything())


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
count(data, region, subregion)

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
# Alaska: The 2006 Eastern Steller sea lion PBR was calculated incorrectly (2000 instead of 2004)
# Atlantic: The 2009-10 GoM/BoF harbor porpoise PBR should be 703 and not 701 (arithmetic incorrect in SAR)
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
table(data$osp_status)
table(data$esa_status)

# Revised
table(data$revised_yn)

# Trend
table(data$trend)

# Methods
methods <- data %>% 
  select(n_method) %>% 
  unique()
write.csv(methods, file="data/sars/keys/methods_key_raw.csv")

# Export
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "US_sars_data.Rds"))


# Build stock key
################################################################################

# Identify stocks
stocks <- data %>% 
  group_by(region, subregion, area, group, comm_name, species, stock) %>% 
  summarize(yr1=min(year),
            yr2=max(year)) %>% 
  ungroup()

# Export
saveRDS(stocks, file=file.path(outdir, "US_sars_stocks.Rds"))


