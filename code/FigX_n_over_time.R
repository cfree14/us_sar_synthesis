
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


# Build data
################################################################################

# Prep data
data <- data_orig %>% 
  filter(group!="USFWS marine mammals")

# Status by region
stats_region <- data %>% 
  # Summarize
  group_by(region, year) %>% 
  summarize(nmin=sum(n_min, na.rm=T)) %>% ungroup()

# Status by group
stats_group <- data %>% 
  # Summarize
  group_by(group, year) %>% 
  summarize(nmin=sum(n_min, na.rm=T)) %>% ungroup()

# Status by group
stats_region_group <- data %>% 
  # Summarize
  group_by(region, group, year) %>% 
  summarize(nmin=sum(n_min, na.rm=T)) %>% ungroup()

ggplot(stats_region, mapping=aes(x=year, y=nmin/1e6, color=region)) +
  facet_wrap(~region, scales="free_y") +
  geom_line() + 
  # Axes
  scale_y_continuous(lim=c(0, NA)) +
  # Labels
  labs(x="Year", y="Population size (millions of animals)") +
  # Theme
  theme_bw()

ggplot(stats_group, mapping=aes(x=year, y=nmin/1e6, color=group)) +
  facet_wrap(~group, scales="free_y") +
  geom_line() + 
  # Axes
  scale_y_continuous(lim=c(0, NA)) +
  # Labels
  labs(x="Year", y="Population size (millions of animals)") +
  # Theme
  theme_bw()



ggplot(stats_region_group, mapping=aes(x=year, y=nmin/1e6, color=region)) +
  facet_wrap(~group, scales="free_y") +
  geom_line() + 
  # Axes
  scale_y_continuous(lim=c(0, NA)) +
  # Labels
  labs(x="Year", y="Population size (millions of animals)") +
  # Theme
  theme_bw()




# SI/M
################################################################################


# Status by group
stats_region_group1 <- data %>% 
  # Summarize
  group_by(region, group, year) %>% 
  summarize(sim_fisheries=sum(sim_fisheries, na.rm=T)) %>% ungroup()


ggplot(stats_region_group1, mapping=aes(x=year, y=sim_fisheries, color=region)) +
  facet_wrap(~group, scales="free_y") +
  geom_line() + 
  # Axes
  scale_x_continuous(breaks=seq(1995,2025, 5)) +
  scale_y_continuous(lim=c(0, NA)) +
  # Labels
  labs(x="Year", y="Fisheries SI/M") +
  # Theme
  theme_bw()



