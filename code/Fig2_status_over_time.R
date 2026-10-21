
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

# Remove non-stocks

# Build data
################################################################################

# Prep data
data <- data_orig

# 2024 stats for manuscript
data %>% 
  filter(year==2024) %>% 
  count(strategic_yn) %>% 
  mutate(prop=n/sum(n))

# Status by region
stats_region <- data %>% 
  group_by(subregion, year) %>% 
  summarize(n=n(),
            n_ns=sum(!is.na(strategic_yn) & strategic_yn=="Non-strategic"), 
            prop_ns = n_ns / n) %>% 
  ungroup() %>% 
  mutate(label=paste0(round(prop_ns*100,0), "%"))

# Status by group
stats_group <- data %>% 
  group_by(group, year) %>% 
  summarize(n=n(),
            n_ns=sum(!is.na(strategic_yn) & strategic_yn=="Non-strategic"), 
            prop_ns = n_ns / n) %>% 
  ungroup() %>% 
  mutate(label=paste0(round(prop_ns*100,0), "%"))

# Plot data
################################################################################

# Subregion order
subregions <- c("Atlantic", "Gulf of Mexico", "West Coast", "Hawaii", "Alaska")
subregion_colors <- c(RColorBrewer::brewer.pal(9, "Blues")[c(5, 7)],
                      RColorBrewer::brewer.pal(9, "Greens")[c(5, 7)],
                      RColorBrewer::brewer.pal(9, "Purples")[7])

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot status by region
g1 <- ggplot(stats_region, aes(x=year, y=prop_ns, color=factor(subregion, subregions))) +
  geom_line() +
  geom_point(data=stats_region %>% filter(year==max(year))) +
  geom_text(data=stats_region %>% filter(year==max(year)), 
            mapping=aes(label=label), x=2025, size=2.2, hjust=0, show.legend = F) +
  # Labels
  labs(x="Year", y="Percent non-strategic", tag="A") +
  scale_x_continuous(breaks=seq(1995, 2025, 5), lim=c(1995, 2027)) +
  scale_y_continuous(labels=scales::percent_format(), lim=c(0,1)) +
  # Legend
  scale_color_manual(name="", values=subregion_colors) +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = c(0.7, 0.22),
        legend.key.size=unit(0.3, "cm")) 
g1

# Plot status by group
g2 <- ggplot(stats_group, aes(x=year, y=prop_ns, color=group)) +
  geom_line() +
  geom_point(data=stats_group %>% filter(year==max(year))) +
  geom_text(data=stats_group %>% filter(year==max(year)), 
            mapping=aes(label=label), x=2025, size=2.2, hjust=0, show.legend = F) +
  # Labels
  labs(x="Year", y="", tag="B") +
  scale_x_continuous(breaks=seq(1995, 2025, 5), lim=c(1995, 2027)) +
  scale_y_continuous(labels=scales::percent_format(), lim=c(0,1)) +
  # Legend
  scale_color_manual(name="Group",
                       values=RColorBrewer::brewer.pal(7, "Set2")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "right", 
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.42, 0.58))

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_status_over_time.png"), 
       width=6.5, height=3, units="in", dpi=600, bg="white")
 




