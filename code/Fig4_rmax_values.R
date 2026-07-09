
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
# Remove non-stocks

# Build data
################################################################################

# Default values
# 0.12 for pinnipeds and sea otters
# 0.04 for cetaceans and manatees

# Prep data
data <- data_orig %>% 
  # Filter
  filter(group!="USFWS marine mammals" & year==max(year)) %>% 
  # Add default rmax
  mutate(rmax_default=case_when(group %in% c("Otariids", "Phocids") ~ 0.12,
                                group %in% c("Small whales", "Porpoises", "Large whales", "Dolphins") ~ 0.04,
                                T ~ NA)) %>% 
  # Mark whether Rmax is default
  mutate(rmax_default_yn=ifelse(r_max==rmax_default, "Default", "Custom"),
         rmax_type=case_when(r_max==rmax_default ~ "Default",
                             r_max < rmax_default ~ "Lower",
                             r_max > rmax_default ~ "Higher") %>% factor(., levels=c("Default", "Lower", "Higher"))) %>% 
  # Factor groups
  mutate(group=factor(group, levels=c("Dolphins", "Small whales", "Porpoises", "Large whales", "Otariids", "Phocids"))) %>% 
  # Order regions
  mutate(region=factor(region, levels=c("Alaska", "Pacific", "Atlantic") %>% rev()))

# Summarize percent by region, group, type
stats <- data %>% 
  group_by(region, group, rmax_type) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(region, group) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup()

# Summarize percent by region, group, type, and specific value
stats1 <- data %>% 
  group_by(region, group, rmax_type, r_max) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(region, group) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup()

# Build reference lines
ref_lines <- data %>% 
  select(group, rmax_default) %>% unique()


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
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


# Percent default
g1 <- ggplot(stats, aes(y=region, x=prop, fill=rmax_type)) +
  facet_grid(group~., scales="free_y", space="free_y") +
  # facet_wrap(~group, ncol=1) + # Shows Atlantic Otariids 
  # Data
  geom_col(position = position_stack(reverse = TRUE)) + 
  # Labels
  labs(x="Percent of stocks", y="", tag="A") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name=expression("R"["max"]*" type"), 
                     values=c("grey80", "red", "blue"),
                    guide = guide_legend(title.position = "top")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g1

g2 <- ggplot(stats1, aes(y=region, x=r_max, size=prop)) +
  facet_grid(group~., scales="free_y", space="free_y") +
  # facet_wrap(~group, ncol=1) + # Shows Atlantic Otariids 
  # Data
  geom_point(mapping=aes(color=rmax_type)) +
  # Ref lines
  geom_vline(data=ref_lines, mapping=aes(xintercept = rmax_default), 
             color="grey30", linetype="dotted", inherit.aes = F) +
  # Labels
  labs(x=expression("R"["max"]), y="", tag="B") +
  scale_x_continuous(lim=c(0, NA), breaks=seq(0,0.2,0.02)) +
  # Legend
  scale_color_manual(name=expression("R"["max"]*" type"), 
                     values=c("grey80", "red", "blue"), guide="none") +
  scale_size_continuous(name="Percent of stocks", 
                        labels=scales::percent_format(), 
                        guide = guide_legend(title.position = "top")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_rmax_values.png"), 
       width=6.5, height=6.5, units="in", dpi=600, bg="white")

