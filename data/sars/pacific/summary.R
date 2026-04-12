
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/sars/pacific/tables"
outdir <- "data/sars/pacific/processed"

# Read data
data <- readRDS(file=file.path(outdir, "Pacific_SARs_parameters.Rds"))

# Build data
################################################################################

stats <- data %>% 
  group_by(year) %>% 
  summarize(n_stocks=n_distinct(stock), 
            n_min=sum(n_min, na.rm = T),
            n_est=sum(n_est, na.rm = T),
            sim_tot=sum(sim_tot, na.rm = T),
            sim_fish=sum(sim_fish, na.rm=T),
            n_strategic=sum(strategic_yn=="Strategic" & !is.na(strategic_yn)),
            rf_med=mean(rf, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(p_strategic=n_strategic/n_stocks)

stats_rf <- data %>% 
  count(year, rf)

# Setup
################################################################################

ggplot(stats, aes(x=year, y=n_stocks)) +
  geom_line()

ggplot(stats, aes(x=year, y=n_min/1e6)) +
  geom_line()

ggplot(stats, aes(x=year, y=n_est/1e6)) +
  geom_line()

ggplot(stats, aes(x=year, y=sim_tot)) +
  geom_line()

ggplot(stats, aes(x=year, y=sim_fish)) +
  geom_line()

ggplot(stats, aes(x=year, y=p_strategic)) +
  geom_line()

ggplot(stats, aes(x=year, y=rf_med)) +
  geom_line()

ggplot(data, aes(x=year, y=rf, group=year)) +
  geom_boxplot()

ggplot(stats_rf, aes(x=year, y=rf, size=n)) +
  geom_point()
