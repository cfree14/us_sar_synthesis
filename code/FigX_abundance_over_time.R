
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

# Number of animals by group (all stocks)
stats1 <- data_orig %>% 
  group_by(group, year) %>% 
  summarize(n_min=sum(n_min, na.rm=T)) %>% 
  ungroup() %>% 
  # Add catg
  mutate(catg="All stocks (n=286)")

# Consistent stocks
stocks_consistent <- data_orig %>% 
  group_by(stock) %>% 
  summarise(year1=min(year),
            year2=max(year)) %>% 
  ungroup() %>% 
  filter(year1==1995 & year2==2024) 

# Number of animals by group (consistent stocks)
stats2 <- data_orig %>% 
  filter(stock %in% stocks_consistent$stock) %>% 
  group_by(group, year) %>% 
  summarize(n_min=sum(n_min, na.rm=T)) %>% 
  ungroup() %>% 
  # Add catg
  mutate(catg="Consistent stocks (n=160)")

n_distinct(data_orig$stock)
n_distinct(stocks_consistent$stock)

# Merge 
stats <- bind_rows(stats1, stats2)

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   plot.tag = element_text(size=10),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.box.spacing = grid::unit(0, "pt"),
                   legend.margin = margin(t = 0, r = 0, b = 5, l = 0),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

g <- ggplot(stats, aes(x=year, y=n_min/1e3, color=catg)) +
  facet_wrap(~group, scales="free_y", ncol=4) +
  geom_line() +
  # Labels
  labs(x="Year", y="Thousands of animals") +
  # Axes
  scale_x_continuous(breaks=seq(1995, 2025, 5)) +
  scale_y_continuous(lim=c(0,NA)) +
  # Scale
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top") # legend.position = c(0.85, 0.2)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_abundance_over_time.png"), 
       width=6.5, height=4.25, units="in", dpi=600, bg="white")





