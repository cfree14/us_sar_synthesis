
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
data <- readRDS(file=file.path(outdir, "US_sars_data.Rds"))


# To do:
# Remove not stocks

# Build data
################################################################################

# Build
nstocks <- data %>% 
  filter(group!="USFWS marine mammals") %>% 
  group_by(region1, group, year) %>% 
  summarize(nstocks=n()) %>% 
  ungroup()

# Final year total by group and region
max_year <- max(data$year)
nstocks_labels <- nstocks %>% 
  filter(year==max_year)

# Final year total by group
nstocks_group <-nstocks_labels %>% 
  group_by(group) %>% 
  summarize(nstocks=sum(nstocks)) %>% 
  ungroup() %>% 
  arrange(desc(nstocks)) %>% 
  mutate(group_label=paste0(group, " (n=", nstocks, ")") %>% factor(., levels=.))

# Order data and labels
nstocks_ord <- nstocks %>% 
  left_join(nstocks_group %>% select(group, group_label))
nstocks_labels_org <- nstocks_labels %>% 
  left_join(nstocks_group %>% select(group, group_label))

  

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
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

# Plot number of stocks over time
g <- ggplot(nstocks_ord, aes(x=year, y=nstocks, color=region1)) +
  facet_wrap(~group_label, ncol=3, scales="free_y") +
  geom_line() +
  geom_point(data=nstocks_labels_org) +
  geom_text(data=nstocks_labels_org, mapping=aes(hjust=0, label=nstocks), x=2024, size=2.2) +
  # Labels
  labs(x="Year", y="Number of stocks") +
  # Legend
  scale_color_discrete(name="Region") +
  # Axes
  scale_y_continuous(lim=c(0, NA)) +
  scale_x_continuous(lim=c(1995, 2025), breaks = seq(1995,2025, 5)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_nstocks_over_time.png"), 
       width=6.5, height=4, units="in", dpi=600, bg="white")

