
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
  mutate(zrmg=pbr*0.1) %>% 
  summarize(nstocks=n(),
            nstocks_zero=sum( !is.na(sim_fisheries) & sim_fisheries==0 ),
            nstocks_zmrg=sum( (!is.na(zrmg) & !is.na(sim_fisheries) & sim_fisheries<=zrmg) | !is.na(sim_fisheries) & sim_fisheries==0),
            nstocks_pbr=sum( (!is.na(pbr) & !is.na(sim_fisheries) & sim_fisheries<=pbr) | !is.na(sim_fisheries) & sim_fisheries==0)) %>% 
  ungroup() 

# Number of stocks meeting various M/SI performance metrics overall
stats_all <- data_orig %>% 
  group_by(year) %>% 
  mutate(zrmg=pbr*0.1) %>% 
  summarize(nstocks=n(),
            nstocks_zero=sum( !is.na(sim_fisheries) & sim_fisheries==0 ),
            nstocks_zmrg=sum( (!is.na(zrmg) & !is.na(sim_fisheries) & sim_fisheries<=zrmg) | !is.na(sim_fisheries) & sim_fisheries==0),
            nstocks_pbr=sum( (!is.na(pbr) & !is.na(sim_fisheries) & sim_fisheries<=pbr) | !is.na(sim_fisheries) & sim_fisheries==0)) %>% 
  ungroup() %>% 
  mutate(group="All stocks")

# Merge
stats <- bind_rows(stats_group, stats_all) %>% 
  # Compute percentages
  mutate(pstocks_zero=nstocks_zero/nstocks,
         pstocks_zmrg=nstocks_zmrg/nstocks,
         pstocks_pbr=nstocks_pbr/nstocks) %>% 
  # Gather
  select(year, group, pstocks_zero:pstocks_pbr) %>% 
  gather(key="metric", value="prop", 3:ncol(.)) %>% 
  # Format metric
  mutate(metric=recode_factor(metric,
                              "pstocks_zero" = "M/SI = 0",
                              "pstocks_zmrg" = "M/SI ≤ ZMRG",
                              "pstocks_pbr" = "M/SI ≤ PBR"))

# Final year stats
stats24 <- stats %>% 
  filter(year==2024) %>% 
  mutate(label=paste0(round(prop*100, 0), "%" ))

# Group order
group_order <- stats24 %>% 
  filter(group!="All stocks" & metric=="M/SI ≤ ZMRG") %>% 
  arrange(desc(prop)) %>% 
  pull(group)
group_order1 <- c("All stocks", group_order)



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
g <- ggplot(stats, aes(x=year, y=prop, color=metric)) +
  facet_wrap(~factor(group, group_order1), ncol=4) +
  geom_line() +
  geom_text(data=stats24, aes(label=label), x=2024.5, 
            hjust=0, size=2.4, show.legend = F) +
  # Labels
  labs(x="Year", y="Percentage of stocks") +
  scale_x_continuous(breaks=seq(1995, 2025, 5), lim=c(1995, 2028)) +
  scale_y_continuous(label=scales::percent_format()) +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_msi_stats_over_time.png"), 
       width=6.5, height=4.25, units="in", dpi=600, bg="white")




