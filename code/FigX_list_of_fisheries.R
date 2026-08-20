

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
  count(year, category) %>% 
  # Format catg
  mutate(catg_label=as.character(category),
         catg_label=recode(catg_label, 
                           "1"="1-Frequent",
                           "2"="2-Occassional",
                           "3"="3-Rare"))

# N vessels over time
nvessels <- data %>% 
  group_by(year, category) %>% 
  summarize(nvessels=sum(nvessels, na.rm=T)) %>% 
  ungroup() %>% 
  # Format catg
  mutate(catg_label=as.character(category),
         catg_label=recode(catg_label, 
                           "1"="1-Frequent",
                           "2"="2-Occassional",
                           "3"="3-Rare"))

# N stocks over time
nstocks <- spp %>% 
  # Remove non-stocks
  filter(!comm_name %in% c("None documented in the most recent 5 years", "No information")) %>% 
  # Reduce to unique combos of year-category-stock
  # This is b/c a stock could intereact with multiple category X fisheries
  # But we just want to calculate how many stocks interact with at least category X fishery
  select(year, category, stock) %>% 
  unique() %>% 
  # Count
  count(year, category) %>% 
  # Format catg
  mutate(catg_label=as.character(category),
         catg_label=recode(catg_label, 
                           "1"="1-Frequent",
                           "2"="2-Occassional",
                           "3"="3-Rare"))


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
g1 <- ggplot(nfisheries, aes(x=year, y=n, color=catg_label)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of fisheries", tag="A", color="M/SI Category") +
  # Legend
  scale_color_manual(values=c("red", "orange", "darkgreen")) +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = "none")
g1

# Number of fisheries listed over time
g2 <- ggplot(nvessels, aes(x=year, y=nvessels/1000, color=catg_label)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of vessels (1000s)", tag="B", color="M/SI Category") +
  # Legend
  scale_color_manual(values=c("red", "orange", "darkgreen")) +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = "none")
g2

# Number of stocks
g3 <- ggplot(nstocks, aes(x=year, y=n, color=catg_label)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of stocks", tag="C", color="M/SI Category") +
  # Legend
  scale_color_manual(values=c("red", "orange", "darkgreen")) +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = c(0.3, 0.83),
        legend.key.size = unit(0.3, "cm"))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "FigX_list_of_fisheries.png"),
       width=6.5, height=2.5, units="in", dpi=600, bg="white")

