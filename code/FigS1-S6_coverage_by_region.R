
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

# Format data
data <- data_orig %>% 
  # Remove US FWS
  filter(group!="USFWS marine mammals") %>% 
  # Big class
  mutate(group_catg=ifelse(group %in% c("Large whales", "Small whales", "Dolphins", "Porpoises"), "Cetaceans", "Non-cetaceans")) %>% 
  # Factor group
  mutate(group=factor(group, levels=c("Large whales", "Small whales", "Dolphins", "Porpoises", "Phocids", "Otariids")))

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "top",
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Alaska
################################################################################

# Plot
g1 <- ggplot(data %>% filter(region == "Alaska"),
            aes(x = year, y = area, fill = strategic_yn)) +
  # Facet
  ggh4x::facet_nested(
    rows = vars(group, comm_name),
    scales = "free_y",
    space = "free_y",
    nest_line = element_line()   # draws a separator between groups
  ) +
  # Plot data
  geom_tile() +
  # Labels
  labs(x = "Year", y = "", fill = "Status") +
  # Axes
  scale_x_continuous(breaks = seq(1992, 2024, 2)) +
  # Legend
  scale_fill_manual(values=c("grey80", "darkred"), na.value = "grey10") +
  # Theme
  theme_bw() +
  my_theme +
  theme(
    strip.background = element_rect(
      colour = "black",
      fill = NA,   # or whatever fill you're using
      linewidth = 0.2
    ),
    strip.text.y = element_text(angle = 0),
    panel.spacing.y = unit(0, "mm")   # removes spacing between all panels
  ) 
g1

# Export
ggsave(g1, filename=file.path(plotdir, "FigS1_sar_coverage_ak.png"), 
       width=6.5, height=7.0, units="in", dpi=600, bg="white")


# Pacific
################################################################################

# Plot
g2 <- ggplot(data %>% filter(subregion == "Hawaii"),
            aes(x = year, y = area, fill = strategic_yn)) +
  # Facet
  ggh4x::facet_nested(
    rows = vars(group, comm_name),
    scales = "free_y",
    space = "free_y",
    nest_line = element_line()   # draws a separator between groups
  ) +
  # Plot data
  geom_tile() +
  # Labels
  labs(x = "Year", y = "", fill = "Status") +
  # Axes
  scale_x_continuous(breaks = seq(1992, 2024, 2)) +
  # Legend
  scale_fill_manual(values=c("grey80", "darkred"), na.value = "grey10") +
  # Theme
  theme_bw() +
  my_theme +
  theme(
    strip.background = element_rect(
      colour = "black",
      fill = NA,   # or whatever fill you're using
      linewidth = 0.2
    ),
    strip.text.y = element_text(angle = 0),
    panel.spacing.y = unit(0, "mm")   # removes spacing between all panels
  ) 
g2

# Export
ggsave(g2, filename=file.path(plotdir, "FigS2_sar_coverage_hi.png"), 
       width=6.5, height=6.0, units="in", dpi=600, bg="white")

# Plot
g3 <- ggplot(data %>% filter(subregion == "West Coast"),
            aes(x = year, y = area, fill = strategic_yn)) +
  # Facet
  ggh4x::facet_nested(
    rows = vars(group, comm_name),
    scales = "free_y",
    space = "free_y",
    nest_line = element_line()   # draws a separator between groups
  ) +
  # Plot data
  geom_tile() +
  # Labels
  labs(x = "Year", y = "", fill = "Status") +
  # Axes
  scale_x_continuous(breaks = seq(1992, 2024, 2)) +
  # Legend
  scale_fill_manual(values=c("grey80", "darkred"), na.value = "grey10") +
  # Theme
  theme_bw() +
  my_theme +
  theme(
    strip.background = element_rect(
      colour = "black",
      fill = NA,   # or whatever fill you're using
      linewidth = 0.2
    ),
    strip.text.y = element_text(angle = 0),
    panel.spacing.y = unit(0, "mm")   # removes spacing between all panels
  ) 
g3

# Export
ggsave(g3, filename=file.path(plotdir, "FigS3_sar_coverage_wc.png"), 
       width=6.5, height=6.0, units="in", dpi=600, bg="white")


# Atlantic
################################################################################

# Plot
g4 <- ggplot(data %>% filter(region=="Atlantic" & group != "Dolphins"),
            aes(x = year, y = area, fill = strategic_yn)) +
  # Facet
  ggh4x::facet_nested(
    rows = vars(group, comm_name),
    scales = "free_y",
    space = "free_y",
    nest_line = element_line()   # draws a separator between groups
  ) +
  # Plot data
  geom_tile() +
  # Labels
  labs(x = "Year", y = "", fill = "Status") +
  # Axes
  scale_x_continuous(breaks = seq(1992, 2024, 2)) +
  # Legend
  scale_fill_manual(values=c("grey80", "darkred"), na.value = "grey10") +
  # Theme
  theme_bw() +
  my_theme +
  theme(
    strip.background = element_rect(
      colour = "black",
      fill = NA,   # or whatever fill you're using
      linewidth = 0.2
    ),
    strip.text.y = element_text(angle = 0),
    panel.spacing.y = unit(0, "mm")   # removes spacing between all panels
  ) 
g4

# Export
ggsave(g4, filename=file.path(plotdir, "FigS4_sar_coverage_atl_nondolphins.png"), 
       width=6.5, height=6.0, units="in", dpi=600, bg="white")


# Plot
g5 <- ggplot(data %>% filter(region=="Atlantic" & group == "Dolphins"),
             aes(x = year, y = area, fill = strategic_yn)) +
  # Facet
  ggh4x::facet_nested(
    rows = vars(comm_name, subregion),
    scales = "free_y",
    space = "free_y",
    nest_line = element_line()   # draws a separator between groups
  ) +
  # Plot data
  geom_tile() +
  # Labels
  labs(x = "Year", y = "", fill = "Status") +
  # Axes
  scale_x_continuous(breaks = seq(1992, 2024, 2)) +
  # Legend
  scale_fill_manual(values=c("grey80", "darkred"), na.value = "grey10") +
  # Theme
  theme_bw() +
  my_theme +
  theme(
    strip.background = element_rect(
      colour = "black",
      fill = NA,   # or whatever fill you're using
      linewidth = 0.2
    ),
    strip.text.y = element_text(angle = 0),
    panel.spacing.y = unit(0, "mm")   # removes spacing between all panels
  ) 
g5

# Export
ggsave(g5, filename=file.path(plotdir, "FigS5_sar_coverage_atl_dolphins.png"), 
       width=6.5, height=7.0, units="in", dpi=600, bg="white")

