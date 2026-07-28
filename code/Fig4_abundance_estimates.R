
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

# Methods key
methods_key <- readxl::read_excel("data/sars/keys/methods_key_final.xlsx")


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
  # Calculate years since survey
  mutate(yrs_since_survey=2024-survey_yr) %>% 
  # Add cleaned method
  rename(n_method_orig=n_method) %>% 
  left_join(methods_key, by="n_method_orig") %>% 
  mutate(n_method=stringr::str_to_sentence(n_method)) 

# Method type
method_stats <- data %>% 
  count(region, group, n_method) %>% 
  group_by(region, group) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup()

# Derive percent of stocks with CV>0.3 by region
percs_cv_hi <- data %>% 
  # Only stocks with CV estimates
  filter(!is.na(n_cv)) %>% 
  # Summarizw
  group_by(region) %>% 
  summarize(n=n(),
            n_hi=sum(n_cv>0.3),
            p_hi=n_hi/n) %>% 
  ungroup() %>% 
  # Add label and sort %>% 
  arrange(desc(p_hi)) %>% 
  mutate(region_label_cv=paste0(region, " (", round(p_hi*100, 0), "%)"))

# Derive percent of stocks with last survey >5 or >8 years
percs_srvy_yr <- data %>% 
  # Only stocks with survey years
  filter(!is.na(survey_yr)) %>% 
  # Summarizw
  group_by(region) %>% 
  summarize(n=n(),
            n_hi5=sum(yrs_since_survey>5),
            n_hi8=sum(yrs_since_survey>8),
            p_hi5=n_hi5/n, 
            p_hi8=n_hi8/n) %>% 
  ungroup() %>% 
  # Add label and sort %>% 
  # arrange(desc(p_hi5)) %>% 
  mutate(region_label_srvy=paste0(region, " (", round(p_hi5*100, 0), "% / ", round(p_hi8*100, 0), "%)"))

# Add to data
data1 <- data %>% 
  left_join(percs_cv_hi %>% select(region, region_label_cv), by="region") %>% 
  left_join(percs_srvy_yr %>% select(region, region_label_srvy), by="region")

# Calculate percent of stocks with Nest that don't have CV
stats_cv <- data %>% 
  # Has N_est
  filter(!is.na(n_est)) %>% 
  # Summarize
  group_by(region, group, n_method) %>% 
  summarize(n=n(),
            n_no_cv=sum(is.na(n_cv))) %>% 
  ungroup() %>% 
  # Summarize again
  group_by(region, group) %>% 
  mutate(n_region_group=sum(n),
         p_no_cv=n_no_cv/n_region_group) %>% 
  ungroup() %>% 
  # Region label
  group_by(region) %>% 
  mutate(n_region=sum(n),
         n_no_cv_region=sum(n_no_cv),
         p_no_cv_region=n_no_cv_region/n_region,
         region_label=paste0(region, " (", round(p_no_cv_region*100,0), "%)")) %>% 
  ungroup()

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Nest method
g1 <- ggplot(method_stats, aes(y=region, x=prop, fill=n_method)) +
  facet_wrap(~group, nrow=1) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of stocks", y="", tag="A") +
  scale_x_continuous(labels=scales::percent_format()) +
  scale_fill_discrete(name="Method") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "right",
        legend.key.size = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=6))
g1

# % on Nest without CV
g2 <- ggplot(stats_cv, aes(x=p_no_cv, y=group, fill=n_method)) +
  facet_wrap(~region_label, ncol=1, scales="free_y", space="free_y") +
  geom_bar(stat="identity") +
  # Labels 
  labs(x="% of Nest without CV", y="", tag="B") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Nest CV
g3 <- ggplot(data1, aes(x=n_cv, y=group)) +
  facet_wrap(~region_label_cv, ncol=1, scales="free_y", space="free_y") +
  geom_boxplot(fill="grey90", lwd=0.2, outlier.size = 1) +
  # Reference line
  geom_vline(xintercept=0.3, color="red") +
  # Labels
  labs(x="CV of Nest", y="", tag="C") +
  scale_x_continuous(breaks=c(0.3, seq(0,1.5, 0.5))) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g3

# Nest year
g4 <- ggplot(data1, aes(x=yrs_since_survey, y=group)) +
  facet_wrap(~region_label_srvy, ncol=1, scales="free_y", space="free_y") +
  geom_boxplot(fill="grey90", lwd=0.2, outlier.size = 1) +
  # Reference line
  geom_vline(xintercept=5, color="red") +
  geom_vline(xintercept=8, color="red", linetype="dashed") +
  # Labels
  labs(x="Years since last survey (yr)", y="", tag="D") +
  scale_x_continuous(breaks=c(8, seq(0, 35, 5))) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g4

# Merge bottom
g234 <- gridExtra::grid.arrange(g2, g3, g4, nrow=1, widths=c(0.38, 0.31, 0.31))

# Merge all
g <- gridExtra::grid.arrange(g1, g234, nrow=2, heights=c(0.32, 0.68))

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_abundance_estimates.png"), 
       width=6.5, height=6.5, units="in", dpi=600, bg="white")


