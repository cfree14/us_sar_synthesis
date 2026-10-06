
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

# Add a figure showing reported values when default selected to get bias in choice


# Build data
################################################################################

# Default values
# 0.12 for pinnipeds and sea otters
# 0.04 for cetaceans and manatees

# Prep data
data <- data_orig %>% 
  # Not USFWS 
  filter(group!="USFWS marine mammals") %>% 
  # Recent
  filter(year==max(year)) %>% 
  # Has unused R values
  filter(!is.na(unused_r) & unused_r!="NA") %>% 
  # Add default rmax
  mutate(rmax_default=case_when(group %in% c("Otariids", "Phocids") ~ 0.12,
                                group %in% c("Small whales", "Porpoises", "Large whales", "Dolphins") ~ 0.04,
                                T ~ NA)) %>% 
  # Mark whether Rmax is default
  mutate(rmax_default_yn=ifelse(r_max==rmax_default, "Default", "Custom"),
         rmax_type=case_when(r_max==rmax_default ~ "Default",
                             r_max < rmax_default ~ "Lower",
                             r_max > rmax_default ~ "Higher") %>% factor(., levels=c("Default", "Lower", "Higher"))) %>% 
  # Simplify
  select(region, subregion, group, stock, comm_name, area, rmax_default, rmax_default_yn, rmax_type, r_max, unused_r) %>% 
 # Split
  separate(unused_r, sep="; ", into=paste0("unused_r", 1:6)) %>% 
  # Gather
  gather(key="val", value="r_source", 11:ncol(.)) %>% 
  select(-val) %>% 
  filter(!is.na(r_source)) %>% 
  # Split
  separate(r_source, into=c("unused_r", "unused_r_source"), sep=" \\(") %>% 
  # Convert to number
  mutate(unused_r=recode(unused_r, 
                         "0.055-0.06"="0.0575",
                         "0.0292-0.0254"="0.0273") %>% as.numeric() ) %>% 
  # Calc unused vs default and unused vs actual
  # Positive = Unused value higher than used
  mutate(unused_r_vs_act=unused_r-r_max)

my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
ggplot(data, aes(y=group, x=unused_r_vs_act, color=rmax_type)) +
  geom_point(size=3) +
  # Reference
  geom_vline(xintercept=0) +
  # Labels
  labs(x=expression("Unused - Used R"["max"]), y="", tag="C") +
  # Legend
  scale_color_manual(name=expression("R"["MAX"]*" type"), 
                    values=c("grey80", "red", "blue"),
                    guide = guide_legend(title.position = "top")) +
  # Theme
  theme_bw() + my_theme

