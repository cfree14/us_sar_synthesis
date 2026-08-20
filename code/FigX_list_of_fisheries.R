

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/Whales/us_sar_synthesis_data/list_of_fisheries/"
outdir <- "data/sars/processed"
plotdir <- "figures"

# Read data
spp <- readRDS(file=file.path(outdir, "1995_2024_list_of_fisheries_stocks.Rds"))
data <- readRDS(file=file.path(outdir, "1995_2024_list_of_fisheries.Rds"))


# Summarize data
################################################################################

# N fisheries over time
nfisheries <- data %>% 
  count(year, category)

# N vessels over time
nvessels <- data %>% 
  group_by(year, category) %>% 
  summarize(nvessels=sum(nvessels, na.rm=T)) %>% 
  ungroup()

# N stocks over time
nstocks <- spp %>% 
  count(year, region) 


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.title=element_blank(),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Number of fisheries listed over time
g1 <- ggplot(nfisheries, aes(x=year, y=n, color=as.character(category))) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of fisheries", tag="A", color="Category") +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = "none")
g1

# Number of fisheries listed over time
g2 <- ggplot(nvessels, aes(x=year, y=nvessels/1000, color=as.character(category))) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of vessels (1000s)", tag="B", color="Category") +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = c(0.8, 0.8),
        legend.key.size = unit(0.3, "cm"))
g2

# Number of stocks
g3 <- ggplot(nstocks, aes(x=year, y=n, color=region)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of stocks", tag="C", color="Region") +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = c(0.25, 0.8),
        legend.key.size = unit(0.3, "cm"))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "FigX_list_of_fisheries.png"),
       width=6.5, height=2.5, units="in", dpi=600, bg="white")

