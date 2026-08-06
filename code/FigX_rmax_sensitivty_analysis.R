
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

# Export
rmax_limits <- read.csv(file=file.path("tables/TableX_rmax_group_limits.csv"))


# Build data
################################################################################

# 2024 data
data1 <- data_orig %>% 
  # Reduce to 2024
  filter(year==2024) %>% 
  # Mark whether using Rmax default
  mutate(rmax_default=case_when(group %in% c("Otariids", "Phocids") ~ 0.12,
                                group %in% c("Small whales", "Porpoises", "Large whales", "Dolphins") ~ 0.04,
                                T ~ NA)) %>% 
  # Mark whether Rmax is default
  mutate(rmax_default_yn=ifelse(r_max==rmax_default, "Default", "Custom"))

# Reduce to ones with default
data2 <- data1 %>% 
  # Reduce to only ones using default
  # filter(rmax_default_yn=="Default") %>% 
  # Simplify
  select(region, subregion, group, comm_name, stock,
         n_min, r_max, rf, pbr, sim_fisheries)

# Reduce to ones with PBR and fisherie SI/M
data3 <- data2 %>% 
  filter(!is.na(pbr) & !is.na(sim_fisheries))

# Number of stocks with default
nrow(data1)
nrow(data2)
nrow(data3)

# Generate RMAX values
################################################################################

x <- 1
rmax_vals <- purrr::map_df(1:nrow(rmax_limits), function(x){
  
  # Extracts
  group <- rmax_limits$group[x]
  rmax_lo <- rmax_limits$r_max_lo[x]
  rmax_hi <- rmax_limits$r_max_hi[x]
  
  # Generate sequence
  rmax_int <- 0.005
  rmax_lo_use <- ceiling(rmax_lo / rmax_int) * rmax_int
  rmax_hi_use <- floor(rmax_hi / rmax_int) * rmax_int
  rmax_vals <- c(rmax_lo, seq(rmax_lo_use, rmax_hi_use, rmax_int), rmax_hi) %>% unique() 
  
  # Merge
  df <- tibble(group=group,
               rmax_sim=rmax_vals)
  
})

  
# Perform analysis
################################################################################

# Merge data for analysis
output <- data3 %>% 
  # Change Dolphins/Porpoises
  mutate(group=recode(group, 
                      "Dolphins"="Dolphins/porpoises",
                      "Porpoises"="Dolphins/porpoises")) %>% 
  left_join(rmax_vals, by="group") %>% 
  # Arrange
  arrange(stock, rmax_sim) %>% 
  # Compute
  mutate(pbr_sim=n_min*rmax_sim/2*rf,
         strategic_yn=ifelse(sim_fisheries>pbr_sim, "yes", "no"))


# Compute stats
stats <- output %>% 
  # Summarize
  group_by(group, rmax_sim) %>% 
  summarize(n=n(),
            nstocks=n_distinct(stock),
            nstrategic=sum(strategic_yn=="yes"),
            pstrategic=nstrategic/nstocks) %>% 
  ungroup() %>% 
  # Mark whether using Rmax default
  mutate(rmax_default=case_when(group %in% c("Otariids", "Phocids") ~ 0.12,
                                group %in% c("Small whales", "Dolphins/porpoises", "Large whales") ~ 0.04,
                                T ~ NA))

# Export
write.csv(stats, file=file.path("output/rmax_sensitivity_analysis.csv"), row.names = F)


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats, aes(x=rmax_sim, y=pstrategic, color=group)) +
  # Reference lines
  geom_vline(xintercept = c(0.04, 0.12), color="grey30", linetype="dotted") +
  # Lines
  geom_line(linewidth = 1.2) +
  geom_point(data=stats %>% filter(rmax_sim==rmax_default), show.legend = F,
             mapping=aes(fill=group), pch=21, color="black", size=2) +
  # Labels
  labs(x=expression("R"["MAX"]*" value"), y="Percentage of stocks with\nfisheries SI/M exceeding the PBR") +
  scale_x_continuous(lim=c(0, NA), breaks=seq(0,0.2, 0.02)) +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_color_discrete(name="Group") +
  # Theme
  theme_bw() + my_theme
g 

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_rmax_sensitivity_analysis.png"), 
       width=5.5, height=3.5, units="in", dpi=600, bg="white")

