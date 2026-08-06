
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

# Build ids
data <- data_orig %>% 
  # Temporarily remove USFWS
  filter(group!="USFWS marine mammals") %>% 
  # Identify unique Rmax values ever used for group
  filter(!is.na(r_max)) %>% 
  select(group, r_max) %>% 
  unique() %>% 
  # Add default rmax
  mutate(rmax_default=case_when(group %in% c("Otariids", "Phocids") ~ 0.12,
                                group %in% c("Small whales", "Porpoises", "Large whales", "Dolphins") ~ 0.04,
                                T ~ NA)) %>% 
  # Add RMAX group
  mutate(rmax_group=ifelse(rmax_default==0.12, "High productivity", "Low productivity")) %>% 
  # Mark whether Rmax is default
  mutate(rmax_default_yn=ifelse(r_max==rmax_default, "Default", "Custom"),
         rmax_type=case_when(r_max==rmax_default ~ "Default",
                             r_max < rmax_default ~ "Lower",
                             r_max > rmax_default ~ "Higher") %>% factor(., levels=c("Default", "Lower", "Higher")))

# Merge dolphins/porpoises
dp <- bind_rows(data %>% filter(group %in% c("Dolphins", "Porpoises"))) %>% 
  mutate(group="Dolphins/porpoises")

# Add in
data1 <- bind_rows(data, dp)

# Order
order <- data1 %>% 
  group_by(group) %>% 
  summarize(rmax_hi=max(r_max)) %>% 
  ungroup() %>% 
  arrange(rmax_hi)
data2 <- data1 %>% 
  mutate(group=factor(group, levels=order$group))

# Calculate stats
stats <- data2 %>% 
  # Eliminate 0 (right whale)
  filter(r_max!=0) %>% 
  # Summarize
  group_by(rmax_group, group, rmax_default) %>% 
  summarize(r_max_lo=min(r_max),
            r_max_hi=max(r_max)) %>% 
  ungroup() 

# Flip into labels
end_labels <- stats %>% 
  gather(key="metric", value="r_max", 4:5) %>% 
  # Mark whether Rmax is default
  mutate(rmax_default_yn=ifelse(r_max==rmax_default, "Default", "Custom"),
         rmax_type=case_when(r_max==rmax_default ~ "Default",
                             r_max < rmax_default ~ "Lower",
                             r_max > rmax_default ~ "Higher") %>% factor(., levels=c("Default", "Lower", "Higher")))

# Ref lines
ref_lines <- data2 %>% 
  select(rmax_group, rmax_default) %>% unique()

# Export
write.csv(stats, file=file.path("tables/TableX_rmax_group_limits.csv"), row.names = F)


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
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data2, aes(y=group, x=r_max, color=rmax_type)) +
  facet_wrap(~rmax_group, ncol=1, scales="free_y", space="free_y") +
  # Reference lines
  geom_vline(data=ref_lines, mapping=aes(xintercept = rmax_default), 
             color="grey30", linetype="dotted", inherit.aes = F) +
  # Segments
  geom_segment(data=stats, mapping=aes(y=group, x=r_max_lo, xend=r_max_hi), color="black") +
  geom_text(data=end_labels, mapping=aes(y=group, x=r_max, color=rmax_type, label=r_max), 
            vjust=-0.8, size=2.8, show.legend = F) +
  # Points
  geom_point(size=1.8) +
  # Labels
  labs(x=expression("R"["MAX"]), y="") +
  scale_x_continuous(lim=c(0, NA), breaks=seq(0,0.2,0.02)) +
  # Legend
  scale_color_manual(name=expression("R"["max"]*" type"), 
                     values=c("grey30", "red", "blue")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_rmax_group_limits.png"), 
       width=6.5, height=3.25, units="in", dpi=600, bg="white")


