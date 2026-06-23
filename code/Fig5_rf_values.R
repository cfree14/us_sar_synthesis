
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
data <- data_orig %>% 
  # Filter
  filter(group!="USFWS marine mammals" & year==max(year))


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
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Recovery factor
g <- ggplot(data, aes(y=group, x=rf, fill=strategic_yn)) +
  # Reference lines
  geom_vline(xintercept=c(0.1, 0.5, 1), linetype="dotted", color="grey30") +
  # Data
  geom_boxplot() +
  # Labels
  labs(x="Recovery factor (FR)", y="") +
  # Scales
  scale_x_continuous(breaks=seq(0.1, 1, 0.1)) +
  # Legend
  scale_fill_discrete(name="Status") +
  # Theme
  theme_bw() + my_theme
g


# Export
ggsave(g, filename=file.path(plotdir, "Fig5_rf_values.png"), 
       width=6.5, height=3., units="in", dpi=600, bg="white")





