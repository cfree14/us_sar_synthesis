
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

# Number of stocks meeting various M/SI performance metrics by group
stats_group <- data_orig %>% 
  group_by(year, group) %>% 
  summarize(msi_fisheries=sum(sim_fisheries, na.rm = T),
            msi_total=sum(sim_total, na.rm = T))%>% 
  ungroup() 

# Number of stocks meeting various M/SI performance metrics overall
stats_all <- data_orig %>% 
  group_by(year) %>% 
  summarize(msi_fisheries=sum(sim_fisheries, na.rm = T),
            msi_total=sum(sim_total, na.rm = T))%>%  
  ungroup() %>% 
  mutate(group="All stocks")

# Order groups
groups <- stats_group %>% 
  filter(year==2024) %>% 
  arrange(desc(msi_total)) %>% 
  pull(group)

# Merge
stats <- bind_rows(stats_group, stats_all) %>% 
  # Gather
  gather(key="metric", value="msi", 3:ncol(.)) %>% 
  # Format metric
  mutate(metric=recode_factor(metric,
                              "msi_fisheries" = "Fisheries",
                              "msi_total" = "Total")) %>% 
  # Order groups
  mutate(group=factor(group, levels=c("All stocks", groups)))

# Calculate % fisheries
pfisheries <- stats %>% 
  spread(key="metric", value="msi") %>%
  mutate(pfisheries=Fisheries/Total) %>% 
  # TEMPORARY CAP
  mutate(pfisheries=pmin(pfisheries, 1))

# Split data
stats1 <- stats %>% 
  filter(group %in% levels(stats$group)[1:4])
stats2 <- stats %>% 
  filter(!group %in% levels(stats$group)[1:4])
pfisheries1 <- pfisheries %>% 
  filter(group %in% levels(stats$group)[1:4])
pfisheries2 <- pfisheries %>% 
  filter(!group %in% levels(stats$group)[1:4])

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   axis.text.y = element_text(angle = 90, hjust = 0.5), 
                   axis.title=element_text(size=8),
                   plot.tag = element_text(size=9),
                   legend.text = element_text(size=7),
                   legend.title = element_text(size=8),
                   strip.text = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "top",
                   legend.box.spacing = grid::unit(0, "pt"),
                   legend.margin = margin(t = 0, r = 0, b = 5, l = 0),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# M/SI first row
g1 <- ggplot(stats1, aes(x=year, y=msi/1e3, color=metric)) +
  facet_wrap(~group, ncol=4, scales="free_y") +
  geom_line() +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Thousands of mortalities and\nserious injuries (M/SI)") +
  scale_x_continuous(breaks=seq(1995, 2025, 5), lim=c(1995, 2025)) +
  # Legend
  scale_color_manual(name="M/SI source", values=c("red", "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x=element_blank(), 
        axis.title.x=element_blank())
g1

# MSI second row
g2 <- ggplot(stats2, aes(x=year, y=msi/1e3, color=metric)) +
  facet_wrap(~group, ncol=4, scales="free_y") +
  geom_line() +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Thousands of mortalities and\nserious injuries (M/SI)") +
  scale_x_continuous(breaks=seq(1995, 2025, 5), lim=c(1995, 2025)) +
  # Legend
  scale_color_manual(name="M/SI source", values=c("red", "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.title.x=element_blank())
g2

# P(fisheries) first row
g1a <- ggplot(pfisheries1, aes(x=year, y=pfisheries)) +
  facet_wrap(~group, ncol=4) +
  geom_line(color="red") +
  # Labels
  labs(x="Year", y="Percent of\ntotal M/SI") +
  scale_x_continuous(breaks=seq(1995, 2025, 5), lim=c(1995, 2025)) +
  scale_y_continuous(labels=scales::percent_format(), 
                     breaks=c(0, 0.5, 1), lim=c(0, 1)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x=element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank())
g1a

# P(fisheries) first row
g2a <- ggplot(pfisheries2, aes(x=year, y=pfisheries)) +
  facet_wrap(~group, ncol=4) +
  geom_line(color="red") +
  # Labels
  labs(x="Year", y="Percent of\ntotal M/SI") +
  scale_x_continuous(breaks=seq(1995, 2025, 5), lim=c(1995, 2025)) +
  scale_y_continuous(labels=scales::percent_format(), 
                     breaks=c(0, 0.5, 1), lim=c(0, 1)) +
  # Theme
  theme_bw() + my_theme + 
  theme(strip.background = element_blank(),
        strip.text = element_blank())
g2a

# Merge
g <- gridExtra::grid.arrange(g1, g1a, g2, g2a, ncol=1,
                             heights=c(0.33, 0.17, 0.3, 0.2))

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_msi_over_time_new.png"), 
       width=6.5, height=6, units="in", dpi=600, bg="white")




