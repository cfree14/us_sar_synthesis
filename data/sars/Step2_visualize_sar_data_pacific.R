
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/sars/pacific/tables"
outdir <- "data/sars/pacific/processed"
plotdir <- "data/sars/pacific/figures"

# Read data
data <- readRDS(file=file.path(outdir, "Pacific_SARs_parameters.Rds"))


# Prep data
################################################################################

# Stocks
stocks <- sort(unique(data$stock))

# Loop through
i <- 1
for(i in 1:length(stocks)){
  
  # Stock do
  stock_do <- stocks[i]
  
  # Subset
  sdata <- data %>% 
    filter(stock==stock_do) # "Humpback whale (CA/OR/WA)"
  
  # Prep N data
  sdata1 <- sdata %>% 
    select(year, revised_yn, n_est, n_min) %>% 
    gather(key="metric", value="n", 3:ncol(.)) %>% 
    mutate(metric=recode(metric,
                         "n_est"="Estimated abundance",
                         "n_min"="Minimum abundance"))
  
  # Prep SIM data
  sim <- sdata %>% 
    select(year, sim_tot, sim_fish) %>%
    mutate(sim_nonfish=sim_tot-sim_fish) %>% 
    select(year, sim_nonfish, sim_fish) %>%
    gather(key="sim_type", value="sim", 2:ncol(.)) %>% 
    mutate(sim_type=recode_factor(sim_type,
                                  "sim_fish"="Fisheries",
                                  "sim_nonfish"="Non-fisheries"))
  
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
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Nest and Nmin
  # Add CV and abundance times series
  # Add MNPL if its available
  g1 <- ggplot(sdata1, aes(x=year, y=n, color=metric, group=metric, shape=revised_yn)) +
    geom_line() +
    geom_point(size=2) + 
    # Labels
    labs(x="Year", y="Number of animals",  tag="A", title=stock_do) +
    # Legend
    scale_color_discrete(name="Abunance type") +
    scale_shape_manual(name="SAR type", values=c(16, 21)) +
    # Axes
    scale_y_continuous(lim=c(0, NA)) +
    scale_x_continuous(lim=c(1995,2025), breaks=seq(1995, 2025, 5)) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position = c(0.15, 0.8),
          legend.key.size = unit(0.3, "cm"))
  g1
  
  # Recovery factor
  g2 <- ggplot(sdata, aes(x=year, y=rf)) +
    geom_hline(yintercept=c(0.1, 0.5), color="grey80", linetype="dashed") +
    geom_line() + 
    geom_point() + 
    # Axes
    scale_y_continuous(lim=c(0.1, 1), breaks=seq(0.1, 1, 0.1)) +
    scale_x_continuous(lim=c(1995,2025), breaks=seq(2000, 2020, 10)) +
    # Labels
    labs(x="Year", y="Recovery factor (RF)", tag="B", title=" ") +
    # Theme
    theme_bw() + my_theme
  g2
  
  # PBR vs SI/M
  # Add SI/M and strategic status
  g3 <- ggplot(sdata, aes(x=year, y=pbr)) +
    geom_bar(data=sim, mapping=aes(x=year, y=sim, fill=sim_type), stat="identity") +
    geom_line() + 
    geom_point() + 
    # Axes
    scale_y_continuous(lim=c(0, NA)) +
    scale_x_continuous(lim=c(1995,2025), breaks=seq(1995, 2025, 5)) +
    # Labels
    labs(x="Year", y="Potential biological removal (PBR)", tag="C") +
    # Legend
    scale_fill_discrete(name="SI/M type") +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position = c(0.15, 0.8),
          legend.key.size = unit(0.3, "cm"))
  g3
  
  # Rmax
  g4 <- ggplot(sdata, aes(x=year, y=r_max)) +
    geom_hline(yintercept=c(0.04, 0.12), color="grey80", linetype="dashed") +
    geom_line() + 
    geom_point() + 
    # Axes
    scale_y_continuous(lim=c(0, 0.2)) +
    scale_x_continuous(lim=c(1995,2025), breaks=seq(2000, 2020, 10)) +
    # Labels
    labs(x="Year", y="Rmax", tag="D") +
    # Theme
    theme_bw() + my_theme
  g4
  
  # Merge
  layout_matrix <- matrix(data=c(1,2,
                                 3,4), ncol=2, byrow=2)
  g <- gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix=layout_matrix, widths=c(0.7, 0.3))
  
  # Export
  filename <- paste0(stock_do, ".png") %>% gsub("/", "-", .)
  ggsave(g, filename=file.path(plotdir,   filename), 
         width=6.5, height=4.5, units="in", dpi=600, bg="white")
  
}

