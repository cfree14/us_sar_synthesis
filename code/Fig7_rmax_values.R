
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

# USFWS to remove
usfws_remove <- c("Polar bear", "Pacific walrus") # "West Indian manatee")

# Prep data
data <- data_orig %>% 
  # Filter recents
  filter(year==max(year)) %>% 
  # Handle USFWS
  # Filter
  filter(!comm_name %in% usfws_remove) %>% 
  mutate(group=case_when(grepl("otter", comm_name) ~ "Sea otters", 
                         grepl("manatee", comm_name) ~ "Manatees",
                         T ~ group)) %>% 
  # Add default rmax
  mutate(rmax_default=case_when(group %in% c("Otariids", "Phocids", "Sea otters") ~ 0.12,
                                group %in% c("Small whales", "Porpoises", "Large whales", "Dolphins", "Manatees") ~ 0.04,
                                T ~ NA)) %>% 
  # Mark whether Rmax is default
  mutate(rmax_default_yn=ifelse(r_max==rmax_default, "Default", "Custom"),
         rmax_type=case_when(r_max==rmax_default ~ "Default",
                             r_max < rmax_default ~ "Lower",
                             r_max > rmax_default ~ "Higher") %>% factor(., levels=c("Default", "Lower", "Higher"))) %>% 
  # Factor groups
  mutate(group=recode(group, "Manatees"="Mn.")) %>% 
  mutate(group=factor(group, levels=c("Mn.", "Dolphins", "Small whales", "Porpoises", "Large whales", "Otariids", "Phocids", "Sea otters"))) %>% 
  # Order regions
  mutate(region=factor(region, levels=c("Alaska", "Pacific", "Atlantic") %>% rev()))

# Summarize percent by region, group, type
stats <- data %>% 
  group_by(subregion, group, rmax_type) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(subregion, group) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup()

# Summarize percent by region, group, type, and specific value
stats1 <- data %>% 
  group_by(subregion, group, rmax_type, r_max) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(subregion, group) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup()

# Build reference lines
ref_lines <- data %>% 
  select(group, rmax_default) %>% unique()



# Prep unused Rmax data
################################################################################

data1 <- data %>% 
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


# Prep stats for manuscript
################################################################################

# % non-default
ndefault <- sum(data$rmax_default_yn=="Default")
ntotal <- nrow(data)
ndefault/ntotal
ndefault
ntotal

# % higher when custom
ncustom <- sum(data$rmax_default_yn=="Custom")
nhigher <- sum(data$rmax_type=="Higher")
nhigher / ncustom
nhigher
ncustom

# Dolphin stocks
ndolphin <- sum(data$group=="Dolphins")
ndolphin_low <- sum(data$group=="Dolphins" & data$rmax_type=="Lower")
data %>% 
  filter(group=="Dolphins" & rmax_type=="Lower")


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
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Percent default
g1 <- ggplot(stats, aes(y=subregion, x=prop, fill=rmax_type)) +
  facet_grid(group~., scales="free_y", space="free_y") +
  # facet_wrap(~group, ncol=1) + # Shows Atlantic Otariids 
  # Data
  geom_col(position = position_stack(reverse = TRUE)) + 
  # Labels
  labs(x="Percent of stocks", y="", tag="A") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name=expression("R"["MAX"]*" type"), 
                     values=c("grey80", "red", "blue"),
                    guide = guide_legend(title.position = "top")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g1

g2 <- ggplot(stats1, aes(y=subregion, x=r_max, size=prop)) +
  facet_grid(group~., scales="free_y", space="free_y") +
  # facet_wrap(~group, ncol=1) + # Shows Atlantic Otariids 
  # Data
  geom_point(mapping=aes(color=rmax_type)) +
  # Ref lines
  geom_vline(data=ref_lines, mapping=aes(xintercept = rmax_default), 
             color="grey30", linetype="dotted", inherit.aes = F) +
  # Labels
  labs(x=expression("R"["MAX"]), y="", tag="B") +
  scale_x_continuous(lim=c(0, NA), breaks=seq(0, 0.3, 0.05)) +
  # Legend
  scale_color_manual(name=expression("R"["max"]*" type"), 
                     values=c("grey80", "red", "blue"), guide="none") +
  scale_size_continuous(name="Percent of stocks", 
                        labels=scales::percent_format(), 
                        guide = guide_legend(title.position = "top")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top",
        axis.text.y=element_blank())
g2

# Unused R max values
g3 <- ggplot(data1, aes(y=group, x=unused_r_vs_act, color=rmax_type)) +
  geom_point(size=3) +
  # Reference
  geom_vline(xintercept=0) +
  # Labels
  labs(x=expression("Alternative R"["MAX"]*" - Selected R"["MAX"]), y="", tag="C") +
  scale_x_continuous(breaks=seq(-0.2, 0.2, 0.02)) +
  # Legend
  scale_color_manual(name=expression("R"["MAX"]*" type"), 
                     values=c("grey80", "red", "blue"),
                     guide = guide_legend(title.position = "top")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3

# Merge
layout_matrix <- matrix(data=c(1,2, 
                               3,3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, 
                             widths=c(0.5, 0.5), heights=c(0.8, 0.2))

# Export
ggsave(g, filename=file.path(plotdir, "Fig7_rmax_values_new.png"), 
       width=6.5, height=7.5, units="in", dpi=600, bg="white")





