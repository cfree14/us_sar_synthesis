
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

# Examine reason for MMPA status: Are all ESA-listed depleted? Why are non-ESA listed depleted?
# Examine reason for strategic status



# Are any 2024 stocks with SI/M > PBR not strategic?
################################################################################

check_strategic <- data_orig %>% 
  filter(year==2024 & strategic_yn=="Non-strategic" & ((sim_total>pbr) | (sim_fisheries>pbr)))

# Build data
################################################################################

# Prep data
data <- data_orig %>% 
  filter(group!="USFWS marine mammals") %>% 
  # Reduce to 2024
  filter(year==2024 & strategic_yn=="Strategic") %>% 
  # PBR test
  mutate(reason_pbr=sim_total>pbr,
         reason_mmpa=osp_status=="Depleted",
         reason_esa=esa_status %in% c("Endangered", "Threatened")) %>% 
  # Simplify
  select(region, group, stock, esa_status, reason_pbr, reason_mmpa, reason_esa) %>% 
  gather(key="reason", value="yesno", 5:ncol(.))

ggplot(data, aes(y=stock, x=reason, fill=yesno)) +
  facet_wrap(~esa_status, scales="free_y", space="free_y") +
  geom_tile() +
  # Legend
  scale_fill_manual(values=c("white", "red"), na.value="grey80") +
  # Theme
  theme_bw()




