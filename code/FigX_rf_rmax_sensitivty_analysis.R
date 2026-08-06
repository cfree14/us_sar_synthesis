
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
stats_rf <- read.csv(file=file.path("output/rf_sensitivity_analysis.csv"), as.is=T)
stats_rmax <- read.csv(file=file.path("output/rmax_sensitivity_analysis.csv"), as.is=T)


# Build data
################################################################################

# Extract points
pts_rf <- stats_rf %>% 
  filter(pstrategic==0) %>% 
  group_by(group, osp_status) %>% 
  filter(rf_sim==min(rf_sim)) %>% 
  ungroup() %>% 
  # Remove the ones that are already 100% non-strat at 0.5
  filter(rf_sim!=0.5)


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
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
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Rmax
g1 <- ggplot(stats_rmax, aes(x=rmax_sim, y=pstrategic, color=group)) +
  # Reference lines
  geom_vline(xintercept = c(0.04, 0.12), color="grey30", linetype="dotted") +
  # Lines
  geom_line(linewidth = 0.7) +
  geom_point(data=stats_rmax %>% filter(rmax_sim==rmax_default), show.legend = F,
             mapping=aes(fill=group), pch=21, color="black", size=2) +
  # Labels
  labs(x=expression("R"["MAX"]*" value"), 
       y="Percentage of stocks with\nfisheries SI/M exceeding the PBR",
       tag="A") +
  scale_x_continuous(lim=c(0, NA), breaks=seq(0, 0.2, 0.02)) +
  # scale_y_continuous(lim=c(-0.007, 0.35), labels=scales::percent_format(), breaks=seq(0, 0.14, 0.02)) +
  scale_y_continuous(lim=c(-0.007, 0.35), labels=scales::percent_format(), breaks=seq(0, 0.4, 0.05)) +
  # Legend
  scale_color_discrete(name="Group") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.65, 0.83),
        legend.title = element_blank())
g1

# RF
g2 <- ggplot(stats_rf, aes(x=rf_sim, y=pstrategic, color=group, linetype=osp_status)) +
  # Lines
  geom_line(linewidth = 0.7) +
  geom_point(data=pts_rf,  size=1.5, show.legend = F) + # mapping=aes(shape=osp_status), 
  geom_text(data=pts_rf, mapping=aes(label=rf_sim), show.legend = F, vjust=1.8, size=2) +
  # Labels
  labs(x=expression("R"["F"]*" value"), 
       y="Percentage of stocks with\nfisheries SI/M exceeding the PBR",
       tag="B") +
  # scale_x_continuous(lim=c(0, NA), breaks=seq(0,0.2, 0.02)) +
  scale_y_continuous(lim=c(-0.007, 0.35), labels=scales::percent_format(), breaks=seq(0, 0.4, 0.05)) +
  # Legend
  scale_color_discrete(name="Group") +
  scale_linetype_manual(name="OSP status", values=c("solid", "dotted")) +
  # scale_shape_manual(name="OSP status", values=c(16, 18)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.43, 0.57))

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_rf_rmax_sensitivity_analysis.png"), 
       width=6.5, height=3, units="in", dpi=600, bg="white")


