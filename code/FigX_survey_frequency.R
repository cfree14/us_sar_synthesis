
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

# Read Yutian's abundance database
nts_orig <- readRDS(data, file="data/abundance/processed/abundance_time_series_database_yutian.Rds") %>% 
  # Add stock name
  mutate(stock=paste0(comm_name, " (", area, ")"))
stock_key <- readxl::read_excel("data/abundance/processed/stock_crosswalk.xlsx")

# Format Yutian data
################################################################################

# Compute number of surveys and survey interval for Yutian's data
stats_yutian <- nts_orig %>% 
  # Add stock name
  rename(stock_yutian=stock) %>% 
  left_join(stock_key) %>% 
  # Remove non-extant stocks
  filter(!is.na(stock)) %>% 
  # Calculate time series length
  group_by(stock, comm_name, area, survey_method, n_units) %>% 
  summarize(nsurveys=n(),
            year1=min(year),
            year2=max(year),
            nyears=length(year1:year2),
            interval_yr=nyears/nsurveys) %>% 
  ungroup() %>% 
  # Select longest for each stock
  arrange(stock, desc(nsurveys)) %>% 
  group_by(stock) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Rename
  rename(nsurveys_yutian=nsurveys,
         interval_yr_yutian=interval_yr)
  
freeR::which_duplicated(nts$stock)

# Build data
################################################################################

# Identify stocks that exist in 2024
stocks <- data_orig %>% 
  # Stock stats
  group_by(stock) %>% 
  summarize(year1=min(year),
            year2=max(year), 
            nyr=n()) %>% 
  ungroup() %>% 
  # Reduce to stocks extant in 2024
  filter(year2==2024)

# Reduce data to stocks of interest
data <- data_orig %>% 
  filter(stock %in% stocks$stock)

# Compute stats
stats <- data %>% 
  group_by(region, subregion, group, comm_name, area, stock) %>% 
  summarize(year1=min(year),
            year2=max(year),
            nyears=length(year1:year2),
            nsurveys=n_distinct(n_est),
            interval_yr=nyears/nsurveys) %>% 
  ungroup() 

stats_use <- stats %>% 
  # Simplify
  select(region, subregion, group, comm_name, area, stock, nsurveys, interval_yr) %>% 
  rename(nsurveys_sars=nsurveys, 
         interval_yr_sars=interval_yr) %>% 
  # Add Yutian time series lengths
  left_join(stats_yutian %>% select(stock, nsurveys_yutian, interval_yr_yutian)) %>% 
  mutate(nsurveys_yutian=ifelse(is.na(nsurveys_yutian), 0, nsurveys_yutian)) %>% 
  # Select whichever is longer
  mutate(nsurveys=pmax(nsurveys_sars, nsurveys_yutian), 
         interval_yr=ifelse(nsurveys_yutian>nsurveys_sars, interval_yr_yutian, interval_yr_sars))

# Calculate group level stats
stats_group <- stats_use %>% 
  group_by(group) %>% 
  summarize(nstocks=n(),
            nstocks_7yr=sum(nsurveys>=7),
            pstocks_7yr=nstocks_7yr/nstocks) %>% 
  ungroup() %>% 
  arrange(desc(pstocks_7yr))

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(stats_use, aes(x=nsurveys, y=factor(group, stats_group$group))) +
  # Datae
  geom_boxplot() +
  # Reference line
  geom_vline(xintercept=7, linetype="dashed", color="red") +
  # Labels
  labs(x="Number of years\nwith abundace estimates", y="", tag="A") +
  scale_x_continuous(breaks=seq(0,50,10)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot data
g2 <- ggplot(stats_use, aes(x=interval_yr, y=factor(group, stats_group$group))) +
  # Datae
  geom_boxplot() +
  # Reference line
  geom_vline(xintercept=5, linetype="solid", color="red") +
  geom_vline(xintercept=8, linetype="dashed", color="red") +
  # Labels
  labs(x="Average interval\nbetween surveys (yr)", y="", tag="B") +
  scale_x_continuous(breaks=seq(0, 30, 5)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.55, 0.45))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_survey_frequency.png"), 
       width=6.5, height=3, units="in", dpi=600, bg="white")


