
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
data_orig <- readRDS(data, file=file.path(outdir, "US_sars_data.Rds")) %>% 
  # Add random trend data
  mutate(trend=sample(x=c("Unknown", "Increasing", "Stable", "Decreasing"),
                      size=n(), prob=c(0.5, 0.25, 0.125, 0.125), replace=T))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to 2024
  filter(year==2024) %>% 
  # Factor trend
  mutate(trend=factor(trend, levels=c("Unknown", "Increasing", "Stable", "Decreasing")))

# Prep stats
stats <- data %>% 
  # Summarize
  count(subregion, group, trend) %>% 
  group_by(subregion, group) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  # Calculate percent of each group that is unknown
  group_by(subregion) %>% 
  mutate(subregion_punknown=sum(n[trend=="Unknown"])/sum(n)) %>% 
  ungroup() %>% 
  group_by(group) %>% 
  mutate(group_punknown=sum(n[trend=="Unknown"])/sum(n)) %>% 
  ungroup() %>% 
  # Build labels
  mutate(subregion_label=paste0(subregion, " (", round(subregion_punknown*100,0), "%)"),
         group_label=paste0(group, " (", round(group_punknown*100,0), "%)"))

# Prep group orders
subregion_order <- stats %>% 
  select(subregion_label, subregion_punknown) %>% 
  unique() %>% 
  arrange(desc(subregion_punknown)) %>% 
  pull(subregion_label)
group_order <- stats %>% 
  select(group_label, group_punknown) %>% 
  unique() %>% 
  arrange(desc(group_punknown)) %>% 
  pull(group_label)


# Setup
################################################################################

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
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g1 <- ggplot(stats, aes(y=group, x=prop, fill=trend)) +
  facet_wrap(~factor(subregion_label, levels=subregion_order), scale="free_y", space="free_y") +
  geom_bar(stat="identity", color="grey30", lwd=0.2, ) +
  # Labels
  labs(x="Percent of stocks", y="", tag="A") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Trend", values=c("grey80", "blue", "white", "red")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g1

# Plot
g2 <- ggplot(stats, aes(y=subregion, x=prop, fill=trend)) +
  facet_wrap(~factor(group_label, levels=group_order), scale="free_y", space="free_y") +
  geom_bar(stat="identity", color="grey30", lwd=0.2, ) +
  # Labels
  labs(x="Percent of stocks", y="", tag="B") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Trend", values=c("grey80", "blue", "white", "red")) +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = "top")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "Fig5_trend_info.png"), 
       width=6.5, height=5.5, units="in", dpi=600, bg="white")





