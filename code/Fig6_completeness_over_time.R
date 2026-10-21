
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
data <- readRDS(data, file=file.path(outdir, "US_sars_data.Rds"))


# Build data
################################################################################

# Completeness
stats <- data %>% 
  # Reduce
  select(subregion, year, n_est, n_cv, n_min, r_max, rf, 
         pbr, sim_total, sim_fisheries,strategic_yn) %>% 
  # Gather
  gather(key="variable", value="value", 3:ncol(.)) %>% 
  # Summarize
  group_by(variable, subregion, year) %>% 
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
                                "sim_total"="Total M/SI",
                                "sim_fisheries"="Fisheries M/SI",
                                "strategic_yn"="Status"))

# Facet labels
facet_labels <- c(
  "Nest" = 'N["EST"]',
  "CVn" = 'CV["N"]',
  "Nmin" = 'N["MIN"]',
  "Rmax" = 'R["MAX"]',
  "Recovery factor" = '"Recovery factor"',
  "PBR" = '"PBR"',
  "Total M/SI" = '"Total M/SI"',
  "Fisheries M/SI" = '"Fisheries M/SI"',
  "Status" = '"Status"'
)

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
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot completeness over time
g <- ggplot(stats, aes(x=year, y=p_complete, color=factor(subregion, subregions))) +
  facet_wrap(~variable, ncol=3, 
             labeller = as_labeller(facet_labels, default = label_parsed)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Percent of SARs with value") +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_color_manual(name="Region",
                     values=subregion_colors) +
  # Theme
  theme_bw() +
  theme(legend.position = "top") + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig6_completeness_over_time.png"), 
       width=6.5, height=6.5, units="in", dpi=600, bg="white")


