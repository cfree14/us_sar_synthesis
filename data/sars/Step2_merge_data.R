
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/sars/merged"

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
  # Reduce
  select(region1, year, n_est, n_cv, n_min, r_max, rf, 
         pbr, sim_total, sim_fisheries,strategic_yn) %>% 
  # Gather
  gather(key="variable", value="value", 3:ncol(.)) %>% 
  # Summarize
  group_by(variable, region1, year) %>% 
  summarize(n=n(),
            n_complete=sum(!is.na(value)),
            p_complete=n_complete/n) %>% 
  ungroup() %>% 
  # Rename variables
  mutate(variable=recode_factor(variable,
                                "n_est"="Nest",
                                "n_cv"="CVn",
                                "n_min"="Nmin",
                                "r_max"="Rmax",
                                "rf"="Recovery factor",
                                "pbr"="PBR",
                                "sim_total"="Total SI/M",
                                "sim_fisheries"="Fisheries SI/M",
                                "strategic_yn"="Status"))

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot completeness over time
ggplot(stats, aes(x=year, y=p_complete, color=region1)) +
  facet_wrap(~variable, ncol=3) +
  geom_line() +
  # Labels
  labs(x="Year", y="Percent of SARs with value") +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_color_discrete(name="Region") +
  # Theme
  theme_bw() +
  theme(legend.position = "top") + my_theme





