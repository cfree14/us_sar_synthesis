
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
pac_orig <- readRDS(data, file=file.path(outdir, "Pacific_SARs_parameters.Rds"))
ak_orig <- readRDS(data, file=file.path(outdir, "Alaska_SARs_parameters.Rds"))
atl_orig <- readRDS(data, file=file.path(outdir, "Atlantic_SARs_parameters.Rds"))


# Build data
################################################################################

# Pacific
pac <- pac_orig %>% 
  # Filter
  filter(year==2024) %>% 
  # FAKE SURVEY YEAR
  mutate(survey_yr=revision_yr) %>% 
  # Simplify
  select(region, group, stock, comm_name, area, n_cv, n_est, n_method, survey_yr)

# Atlantic
atl <- atl_orig %>% 
  # Filter
  filter(year==2024) %>% 
  # Rename
  rename(n_est=n, 
         survey_yr_orig=survey_yr) %>% 
  # Extract recent survey year
  mutate(survey_yr=str_extract(survey_yr_orig, "\\d(?=(?:\\D*\\d){3}\\D*$)(?:\\D*\\d){3}") %>% as.numeric(.)) %>% 
  # Simplify
  select(region, group, stock, comm_name, area, n_cv, n_est, n_method, survey_yr, survey_yr_orig) 

# Atlantic
ak <- ak_orig %>% 
  # Filter
  filter(year==2024) %>% 
  # Rename
  rename(survey_yr_orig=last_survey) %>% 
  # Add
  mutate(stock=paste0(comm_name, " (", area, ")"),
         survey_yr=str_extract(survey_yr_orig, "\\d(?=(?:\\D*\\d){3}\\D*$)(?:\\D*\\d){3}") %>% as.numeric(.)) %>% 
  # Simplify
  select(region, group, stock, comm_name, area, n_cv, n_est, n_method, survey_yr, survey_yr_orig) 

str(pac)
str(ak)
str(atl)

# Merge
data <- bind_rows(ak, atl, pac) %>% 
  mutate(yrs_since_survey=2024-survey_yr)

# Method type
method_stats <- data %>% 
  count(region, group, n_method) %>% 
  group_by(region, group) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup()


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag = element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Nest method
g1 <- ggplot(method_stats, aes(y=group, x=prop, fill=n_method)) +
  facet_wrap(~region, ncol=1, scales="free_y", space="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of stocks", y="", tag="A") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# Nest CV
g2 <- ggplot(data, aes(x=n_cv, y=group)) +
  facet_wrap(~region, ncol=1, scales="free_y", space="free_y") +
  geom_boxplot() +
  # Reference line
  geom_vline(xintercept=0.3, color="red") +
  # Labels
  labs(x="CV of Nest", y="", tag="B") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g2

# Nest year
g3 <- ggplot(data, aes(x=yrs_since_survey, y=group)) +
  facet_wrap(~region, ncol=1, scales="free_y", space="free_y") +
  geom_boxplot() +
  # Reference line
  geom_vline(xintercept=5, color="red") +
  # Labels
  labs(x="Years since last survey (yr)", y="", tag="C") +
  scale_x_continuous(breaks=seq(0, 35, 5)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.4, 0.3, 0.3))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_abundance_estimates.png"), 
       width=6.5, height=6.5, units="in", dpi=600, bg="white")


