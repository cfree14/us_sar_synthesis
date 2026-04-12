
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data"
tabledir <- "tables"
plotdir <- "figures"

# Read species
species <- readxl::read_excel(file.path(datadir, "species.xlsx"))

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "species_abundance.xlsx"), na="NA")

# Thigns to learn from Yutian
# 1) Why some missing RF? Also RMAX, RBR, NMIN stuff


# Format data
################################################################################

group_levels <- c("Large whales", "Small whales", "Dolphins", "Porpoises", "Phocids", "Otariids")

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sar_year=sar_revised_date,
         nmin_year=year,
         recovery_factor=recovery_factor_rf,
         status_mmpa=mmpa_stock_status,
         status_esa=esa_stock_status,
         status_osp=within_osp,
         status_strategic=strategic,
         rmax_orig=r_max) %>% 
  # Format OSP status
  # Revisit this decision
  mutate(status_osp=recode(status_osp,
                           "Unknown (previously reported within OSP)"="Yes")) %>% 
  # Format strategic status
  mutate(status_strategic=recode(status_strategic, 
                                "Yes"="Strategic", 
                                "No"="Non-strategic")) %>% 
  # Format trend
  # Revist this decision
  mutate(trend=recode(trend, 
                      "Increasing (from 2015-2016)"="Increasing",
                      "Increasing (migration)"="Increasing",
                      "Unknown (Van Cise et al., 2021 suggests declining)"="Unknown")) %>% 
  # Fix common name
  mutate(stock=recode(stock, 
                      "Dawrf sperm whale (Hawaii)" = "Dwarf sperm whale (Hawaii)")) %>% 
  # Split stock into common name / area
  separate(stock, into=c("comm_name", "area"), sep=" \\(", remove=F) %>% 
  mutate(area=gsub("\\)", "", area)) %>% 
  # Add species
  left_join(species, by="comm_name") %>% 
  # Order groups
  mutate(group=factor(group, levels=group_levels)) %>% 
  # Format RMAX
  separate(rmax_orig, into=c("rmax", "rmax_type"), sep="-", remove=F) %>% 
  separate(rmax_type, into=c("rmax_type", "rmax_notes"), sep=(" \\(")) %>% 
  separate(rmax_notes, into=c("rmax_other", "rmax_other_source"), sep=("%")) %>% 
  # Format Nmin method
  mutate(nmin_method=case_when(grepl("Multiple", nmin_method) ~ "Multiple methods",
                               T ~ nmin_method)) %>% 
  # Arrange
  select(region, group, stock, comm_name, area, sar_year, everything())

# Inspect
str(data)
freeR::complete(data)

# Trend
table(data$trend)

# Common name
sort(unique(data$comm_name))

# Status
table(data$status_esa)
table(data$status_mmpa)
table(data$status_osp)
table(data$status_strategic)

# RMAX
table(data$rmax)
table(data$rmax_type)
table(data$rmax_notes)


# Recovery factor by status
################################################################################

# I don't know why recovery factor is missing for some

data_rf <- data %>% 
  # Remove stocks without RF (not sure why they don't have it)
  filter(!is.na(recovery_factor)) %>% 
  # Simplify
  select(region, stock, recovery_factor, 
         status_mmpa, status_esa, status_osp, status_strategic) %>% 
  # Gather
  gather(key="status_type", value="status", 4:ncol(.)) %>% 
  # Format status type
  mutate(status_type=paste(gsub("status_", "", status_type) %>% toupper(.)),
         status_type=recode(status_type,
                            "STRATEGIC"="PBR"))


# Plot recovery factors
ggplot(data_rf, aes(y=status, x=recovery_factor)) +
  # Grid
  facet_grid(status_type~., space="free_y", scale="free_y") +
  # Boxplot
  geom_boxplot() + 
  # Labels
  labs(x="Recovery factor", y="") +
  scale_x_continuous(lim=c(0.1, 1), breaks=seq(0.1, 1, 0.1)) +
  # Theme
  theme_bw()


# Status by group
################################################################################

# ESA
stats_esa <- data %>% 
  count(group, status_esa)
ggplot(stats_esa, aes(y=group, x=n, fill=status_esa)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of stocks", y="") +
  # Theme
  theme_bw()


# MMPA
stats_mmpa <- data %>% 
  count(group, status_mmpa)
ggplot(stats_mmpa, aes(y=group, x=n, fill=status_mmpa)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of stocks", y="") +
  # Theme
  theme_bw()

# Strategic
stats_strategic <- data %>% 
  count(group, status_strategic)
ggplot(stats_strategic, aes(y=group, x=n, fill=status_strategic)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of stocks", y="") +
  # Theme
  theme_bw()

# Plot
stats_osp <- data %>% 
  count(group, status_osp)
ggplot(stats_osp, aes(y=group, x=n, fill=status_osp)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of stocks", y="") +
  # Theme
  theme_bw()


# RMAX default use
################################################################################

stats_rmax <- data %>% 
  count(group, rmax_type)
ggplot(stats_rmax, aes(y=group, x=n, fill=rmax_type)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of stocks", y="") +
  # Theme
  theme_bw()


# Nmin method
################################################################################

stats_nmin <- data %>% 
  count(group, nmin_method)
ggplot(stats_nmin, aes(y=group, x=n, fill=nmin_method)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of stocks", y="") +
  # Theme
  theme_bw()







