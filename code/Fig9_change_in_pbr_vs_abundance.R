
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

# Format
data <- data_orig %>% 
  filter(group!="USFWS marine mammals")

# Build data
stats <- data %>% 
  # Reduce to years with PBR and PMIN
  filter(!is.na(pbr) & !is.na(n_min)) %>% 
  # Summarize by stock
  group_by(region, group, comm_name, area) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            pbr1=pbr[year==yr1],
            pbr2=pbr[year==yr2],
            nmin1=n_min[year==yr1],
            nmin2=n_min[year==yr2],
            rf1=rf[year==yr1],
            rf2=rf[year==yr2],
            pbr_ratio=pbr2/pbr1,
            nmin_ratio=nmin2/nmin1,
            rf_change=rf2-rf1,
            pbr_change=(pbr2-pbr1)/pbr1,
            nmin_change=(nmin2-nmin1)/nmin1) %>% 
  ungroup() %>% 
  # Limit to finite
  filter(is.finite(pbr_ratio) & is.finite(nmin_ratio))

# Build data
stats2 <- data %>% 
  # Reduce to years with PBR and PMIN
  filter(!is.na(pbr) & !is.na(n_est)) %>% 
  # Summarize by stock
  group_by(region, group, comm_name, area) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            pbr1=pbr[year==yr1],
            pbr2=pbr[year==yr2],
            n1=n_est[year==yr1],
            n2=n_est[year==yr2],
            rf1=rf[year==yr1],
            rf2=rf[year==yr2],
            pbr_ratio=pbr2/pbr1,
            n_ratio=n2/n1,
            rf_change=rf2-rf1,
            pbr_change=(pbr2-pbr1)/pbr1,
            n_change=(n2-n1)/n1) %>% 
  ungroup() %>% 
  # Limit to finite
  filter(is.finite(pbr_ratio) & is.finite(n_ratio))

# Correlations
cor(x=stats$nmin_ratio, y=stats$pbr_ratio, method="pearson")
cor(x=stats2$n_ratio, y=stats2$pbr_ratio, method="pearson")

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   plot.subtitle = element_text(size=7),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# N_min
amin <- min(c(stats$nmin_ratio, stats$pbr_ratio))
amax <- max(c(stats$nmin_ratio, stats$pbr_ratio))
g1 <- ggplot(stats, aes(x=nmin_ratio, y=pbr_ratio, fill=rf_change)) +
  # Ref line
  geom_hline(yintercept=1, color="grey60", linetype="dotted") +
  geom_vline(xintercept=1, color="grey60", linetype="dotted") +
  # Data
  geom_point(pch=21, size=2.5) +
  # geom_smooth(method="lm") +
  # 1:1 line
  geom_abline(slope=1) +
  annotate(geom="text", x=0.1, y=100, label="PBR liberal", size=2) +
  annotate(geom="text", x=100, y=0.1, label="PBR constrained", size=2) +
  # Axes
  # lims(x=c(0, 40), y=c(0, 40)) +
  scale_x_continuous(trans="log10", 
                     lim=c(amin, amax),
                     breaks=c(0.01, 0.1, 1, 10, 100, 1000),
                     labels=c("0.01", "0.1", "1", "10", "100", "1000")) +
  scale_y_continuous(trans="log10", 
                     lim=c(amin, amax),
                     breaks=c(0.01, 0.1, 1, 10, 100, 1000),
                     labels=c("0.01", "0.1", "1", "10", "100", "1000")) +
  # Labs
  labs(x="Nmin ratio\n(final / initial)", y="PBR ratio\n(final / initial)", 
       tag="A", subtitle=paste(nrow(stats), "stocks")) +
  # Legend
  scale_fill_gradient2(mid="white", high="navy", low="darkred") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# N_est
amin2 <- min(c(stats2$n_ratio, stats2$pbr_ratio))
amax2 <- max(c(stats2$n_ratio, stats2$pbr_ratio))
g2 <- ggplot(stats2, aes(x=n_ratio, y=pbr_ratio, fill=rf_change)) +
  # Ref line
  geom_hline(yintercept=1, color="grey60", linetype="dotted") +
  geom_vline(xintercept=1, color="grey60", linetype="dotted") +
  # Data
  geom_point(pch=21, size=2.5) +
  # geom_smooth(method="lm") +
  # 1:1 line
  geom_abline(slope=1) +
  annotate(geom="text", x=0.1, y=100, label="PBR liberal", size=2) +
  annotate(geom="text", x=10, y=0.1, label="PBR constrained", size=2) +
  # Axes
  # lims(x=c(0, 40), y=c(0, 40)) +
  scale_x_continuous(trans="log10",
                     lim=c(amin2, amax2),
                     breaks=c(0.01, 0.1, 1, 10, 100, 1000),
                     labels=c("0.01", "0.1", "1", "10", "100", "1000")) +
  scale_y_continuous(trans="log10", 
                     lim=c(amin2, amax2),
                     breaks=c(0.01, 0.1, 1, 10, 100, 1000),
                     labels=c("0.01", "0.1", "1", "10", "100", "1000")) +
  # Labs
  labs(x="Nest ratio\n(final / initial)", y="PBR ratio\n(final / initial)", 
       tag="B", subtitle=paste(nrow(stats2), "stocks")) +
  # Legend
  scale_fill_gradient2(name="ΔRecovery factor\n(final - initial)", mid="white", high="navy", low="darkred") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.5, "cm"))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.4, 0.6))


# Export
ggsave(g, filename=file.path(plotdir, "Fig9_change_in_pbr_vs_abundance.png"),
       width=6.5, height=2.75, units="in", dpi=600, bg="white")


