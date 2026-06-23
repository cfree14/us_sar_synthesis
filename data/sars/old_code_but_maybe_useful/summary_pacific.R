
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
data_orig <- readRDS(file=file.path(outdir, "Pacific_SARs_parameters.Rds"))


# Build data
################################################################################

# Remove otters
data <- data_orig %>% 
  filter(group!="USFWS marine mammals")

# Summarize all stocks
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



# Base theme
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Number of stocks over time
################################################################################

# Number of stocks overall
nstock_tot <- data %>% 
  group_by(year, region) %>% 
  summarize(nstocks=n_distinct(stock)) %>% 
  ungroup() %>% 
  mutate(group="Overall")

# Number of stocks by group
nstocks <- data %>% 
  group_by(year, region, group) %>% 
  summarize(nstocks=n_distinct(stock)) %>% 
  ungroup() %>% 
  # Add totals
  bind_rows(nstock_tot)

# Plot number of stocks over time
ggplot(nstocks, aes(x=year, y=nstocks, color=group)) +
  facet_wrap(~region) +
  geom_line() + 
  # Labels
  labs(x="Year", y="Number of stocks") +
  # Axes
  lims(y=c(0, NA)) +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + base_theme


# Number of animals over time
################################################################################

# Number of animals overall
nanimals_tot <- data %>% 
  group_by(year, region) %>% 
  summarize(n_min=sum(n_min, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(group="Overall")

# Number of animals by group
nanimals <- data %>% 
  group_by(year, region, group) %>% 
  summarize(n_min=sum(n_min, na.rm=T)) %>% 
  ungroup() %>% 
  # Add totals
  bind_rows(nanimals_tot)

# Plot number of animals over time
ggplot(nanimals, aes(x=year, y=n_min/1e6, color=group)) +
  facet_wrap(~region) +
  geom_line() + 
  # Labels
  labs(x="Year", y="Millions of individuals") +
  # Axes
  lims(y=c(0, NA)) +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + base_theme



# Proportion strategic stocks
################################################################################

# Proportion strategic stocks overall
pstrategic_tot <- data %>% 
  group_by(year, region) %>% 
  summarize(nstocks=n(),
            nstrategic=sum(strategic_yn=="Strategic" & !is.na(strategic_yn))) %>% 
  ungroup() %>% 
  mutate(pstrategic=nstrategic/nstocks) %>% 
  mutate(group="Overall")

# Proportion strategic stocks by group
pstrategic <- data %>% 
  group_by(year, region, group) %>% 
  summarize(nstocks=n(),
            nstrategic=sum(strategic_yn=="Strategic"  & !is.na(strategic_yn))) %>% 
  ungroup() %>% 
  mutate(pstrategic=nstrategic/nstocks) %>% 
  # Add totals
  bind_rows(pstrategic_tot)

# Proportion strategic stocks over time
ggplot(pstrategic, aes(x=year, y=pstrategic, color=group)) +
  facet_wrap(~region) +
  geom_line() + 
  # Labels
  labs(x="Year", y="Proportion strategic") +
  # Axes
  lims(y=c(0, NA)) +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + base_theme




# Number of animals over time
################################################################################


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
