
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
  # Simplify
  select(region1, filename, 
         year, group, comm_name, species, area,
         r_max, rf, strategic_yn)

# Merge
################################################################################

# Merge
data <- bind_rows(pac, atl, ak)

# Inspect
str(data)
freeR::complete(data)


# Export
################################################################################




# Number of stocks over time
################################################################################

nstocks <- data %>% 
  group_by(region1, group, year) %>% 
  summarize(nstocks=n()) %>% 
  ungroup()

# Plot number of stocks over time
ggplot(nstocks, aes(x=year, y=nstocks, color=region1)) +
  facet_wrap(~group, ncol=4, scales="free_y") +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of stocks") +
  # Legend
  scale_color_discrete(name="Region") +
  # Axes
  scale_y_continuous(lim=c(0, NA)) +
  # Theme
  theme_bw() +
  theme(legend.position = "top")


# Inspect completeness over time
################################################################################

# Completeness
stats <- data %>% 
  select(region1, year, n_est, n_min, r_max, rf, pbr, strategic_yn) %>% 
  gather(key="variable", value="value", 3:ncol(.)) %>% 
  group_by(variable, region1, year) %>% 
  summarize(n=n(),
            n_complete=sum(!is.na(value)),
            p_complete=n_complete/n) %>% 
  ungroup() 

# Plot completeness over time
ggplot(stats, aes(x=year, y=p_complete, color=region1)) +
  facet_wrap(~variable) +
  geom_line() +
  # Labels
  labs(x="Year", y="Percent of SARs with value") +
  # Legend
  scale_color_discrete(name="Region") +
  # Theme
  theme_bw() +
  theme(legend.position = "top")





