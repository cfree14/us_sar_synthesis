
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

# Merge
stats <- bind_rows(stats_group) %>% 
  # Gather
  gather(key="metric", value="msi", 3:ncol(.)) %>% 
  # Format metric
  mutate(metric=recode_factor(metric,
                              "msi_fisheries" = "Fisheries",
                              "msi_total" = "Total"))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   axis.title=element_text(size=9),
                   plot.tag = element_text(size=9),
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


# Plot data
g <- ggplot(stats, aes(x=year, y=msi, color=metric)) +
  facet_wrap(~group, ncol=4, scales="free_y") +
  geom_line() +
  # Labels
  labs(x="Year", y="Mortality and\nserious injury (M/SI)") +
  scale_x_continuous(breaks=seq(1995, 2025, 5), lim=c(1995, 2028)) +
  # Legend
  scale_color_manual(name="", values=c("red", "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_msi_over_time.png"), 
       width=6.5, height=4.25, units="in", dpi=600, bg="white")




