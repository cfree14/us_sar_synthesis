
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/sars/merged"
plotdir <- "figures"

# Read data
data_orig <- readRDS(data, file=file.path(outdir, "US_sars_data.Rds"))


# Build data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "top",
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_orig %>% filter(region1=="Alaska"), 
       aes(x=year, 
           y=paste(comm_name, area, sep="-"), fill=strategic_yn)) +
  facet_grid(group~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Year", y="", fill='Status') +
  scale_x_continuous(breaks=seq(1992, 2026,2)) +
  # Legend
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_sar_coverage_ak.png"), 
       width=6.5, height=7.0, units="in", dpi=600, bg="white")


# Pacific
##################

# Plot data
g <- ggplot(data_orig %>% filter(region1=="Pacific"), 
            aes(x=year, 
                y=paste(comm_name, area, sep="-"), fill=strategic_yn)) +
  facet_grid(group~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Year", y="", fill='Status') +
  scale_x_continuous(breaks=seq(1992, 2026,2)) +
  # Legend
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_sar_coverage_pacific.png"), 
       width=6.5, height=7.0, units="in", dpi=600, bg="white")


# Atlantic
##################

# Plot data
g <- ggplot(data_orig %>% filter(region1=="Atlantic"), 
            aes(x=year, 
                y=paste(comm_name, area, sep="-"), fill=strategic_yn)) +
  facet_grid(group~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Year", y="", fill='Status') +
  scale_x_continuous(breaks=seq(1992, 2026,2)) +
  # Legend
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_sar_coverage_atlantic.png"), 
       width=6.5, height=7.0, units="in", dpi=600, bg="white")
