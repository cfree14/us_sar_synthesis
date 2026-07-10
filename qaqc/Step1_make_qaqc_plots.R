
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/sars/processed"
plotdir <- "qaqc"

# Read data
data <- readRDS(file=file.path(outdir, "US_sars_data.Rds")) %>% 
  mutate(revised_yn=factor(revised_yn, levels=c("yes", "no")))


# Prep data
################################################################################

# Stocks
stocks <- sort(unique(data$stock))
stocks <- data %>% 
  filter(region=="Pacific") %>% 
  slice(1:5) %>% 
  pull(stock)

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
    select(year, sim_total, sim_fisheries) %>%
    mutate(sim_nonfisheries=sim_total-sim_fisheries) %>% 
    select(year, sim_nonfisheries, sim_fisheries) %>%
    gather(key="sim_type", value="sim", 2:ncol(.)) %>% 
    mutate(sim_type=recode_factor(sim_type,
                                  "sim_fisheries"="Fisheries",
                                  "sim_nonfisheries"="Non-fisheries"))
  
  # Plot data
  ################################################################################
  
  # Nest and Nmin
  # Add CV and abundance times series
  # Add MNPL if its available
  g1 <- ggplot(sdata1, aes(x=year, y=n, color=metric, group=metric, shape=revised_yn)) +
    geom_line() +
    geom_point(size=2, fill="white", color="white", shape=16) + # this is just to make prettier 
    geom_point(size=2) + 
    # Labels
    labs(x="Year", y=" \nNumber of animals",  tag="A", title=stock_do) +
    # Legend
    scale_color_manual(name="Abunance type", values=c("red", "blue")) +
    scale_shape_manual(name="SAR type", values=c(16, 21), guide="none") +
    # Axes
    scale_y_continuous(lim=c(0, NA)) +
    scale_x_continuous(lim=c(1995,2025), breaks=seq(1995, 2025, 5)) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position = c(0.2, 0.8),
          legend.key.size = unit(0.3, "cm"),
          axis.title.x=element_blank())
  g1
  
  # Recovery factor
  g2 <- ggplot(sdata, aes(x=year, y=rf)) +
    geom_hline(yintercept=c(0.1, 0.5), color="grey80", linetype="dashed") +
    geom_line() + 
    geom_point(mapping=aes(fill=revised_yn), size=2, pch=21) + 
    # Axes
    scale_y_continuous(lim=c(0.1, 1), breaks=seq(0.1, 1, 0.1)) +
    scale_x_continuous(lim=c(1995,2025), breaks=seq(2000, 2020, 10)) +
    # Labels
    labs(x="Year", y=expression("Recovery factor (R"["F"]*")"), 
         tag="B", title=" ") +
    # Legend
    scale_fill_manual(name="SAR revised?", values=c("black", "white")) +
    # Theme
    theme_bw() + my_theme + 
    theme(legend.position = c(0.35, 0.8),
          legend.key.size = unit(0.3, "cm"),
          axis.title.x=element_blank())
  g2
  
  # PBR vs SI/M
  # Add SI/M and strategic status
  g3 <- ggplot(sdata, aes(x=year, y=pbr)) +
    geom_bar(data=sim, mapping=aes(x=year, y=sim, fill=sim_type), stat="identity") +
    geom_line() + 
    geom_point(size=2, fill="white", color="white", shape=16) + # this is just to make prettier 
    geom_point(mapping=aes(shape=revised_yn), size=2) + 
    # Axes
    scale_y_continuous(lim=c(0, NA)) +
    scale_x_continuous(lim=c(1995,2025), breaks=seq(1995, 2025, 5)) +
    # Labels
    labs(x="Year", 
         y="Potential biological removal (PBR)\n& mortality/serious injury (M/SI)", 
         tag="C") +
    # Legend
    scale_fill_manual(name="M/SI source", values=c("grey30", "grey90")) +
    scale_shape_manual(name="SAR revised?", values=c(16, 21), guide = "none") +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position = c(0.15, 0.8),
          legend.key.size = unit(0.3, "cm"))
  g3
  
  # Rmax
  g4 <- ggplot(sdata, aes(x=year, y=r_max)) +
    geom_hline(yintercept=c(0.04, 0.12), color="grey80", linetype="dashed") +
    geom_line() + 
    geom_point(mapping=aes(fill=revised_yn), size=2, pch=21) + 
    # Axes
    scale_y_continuous(lim=c(0, 0.2)) +
    scale_x_continuous(lim=c(1995,2025), breaks=seq(2000, 2020, 10)) +
    # Labels
    labs(x="Year", y=expression("R"["MAX"]), tag="D") +
    # Legend
    scale_fill_manual(name="SAR revised?", values=c("black", "white"), guide = "none") +
    # Theme
    theme_bw() + my_theme
  g4
  
  # Merge
  layout_matrix <- matrix(data=c(1,2,
                                 3,4), ncol=2, byrow=2)
  g <- gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix=layout_matrix, 
                               widths=c(0.65, 0.35), heights=c(0.45, 0.55))
  
  # Export
  region <- tolower(sdata$region[1])
  filename <- paste0(stock_do, ".png") %>% gsub("/", "-", .)
  ggsave(g, filename=file.path(plotdir, region,  filename), 
         width=6.5, height=5, units="in", dpi=600, bg="white")
  
}

