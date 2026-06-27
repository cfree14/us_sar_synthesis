
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"


# Simulate K data
################################################################################

# Time series characteristics
yrs <- 1980:2025
nyrs <- length(1980:2025)

# Regime characteristics
mus <- c(18000, 23000, 20000)
cv <- 0.05
sds <- cv * mus 
nsteps <- c(15, 15, 16)

# Confirm
sum(nsteps) == nyrs

# Loop through regimes and create data
x <- 1
kvals <- purrr::map_df(1:length(mus), function(x){
  
  # Retrieve values
  mu <- mus[x]
  sd <- sds[x]
  nstep <- nsteps[x]
  
  # Random walk increments
  eps <- rnorm(nstep - 1, mean = 0, sd = sd)
  
  # Initialize series at the target mean
  vec <- numeric(nstep)
  vec[1] <- mu
  
  # Perform walk
  for (t in 2:nstep) {
    vec[t] <- vec[t - 1] + eps[t - 1]
  }
  
  # Build data
  df <- tibble(regime=x,
               mu=mu,
               sd=sd,
               ky=vec)
  
})

# Add years
z <- 2.4
kvals1 <- kvals %>% 
  mutate(year=yrs) %>% 
  select(year, everything()) %>% 
  mutate(mnpl= ky*(1/(1+z)^(1/z)))

# Regime stats
kstats <- kvals1 %>% 
  group_by(regime) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            kavg=mean(ky), 
            ksd=sd(ky),
            mnpl_avg=mean(mnpl),
            mnpl_sd=sd(mnpl)) %>% 
  ungroup() %>% 
  mutate(k_lo=kavg-ksd,#*1.96,
         k_hi=kavg+ksd,#*1.96,
         mnpl_lo=mnpl_avg-mnpl_sd,#*1.96,
         mnpl_hi=mnpl_avg+mnpl_sd)#*1.96)

# K avg
kavg <- mean(kvals1$ky)
mnpl_avg <- mean(kvals1$mnpl)

# Build K labels
k_labels <- tibble(ref=c(expression("K"["AVG"]), 
                         expression("K"["R"]),  
                         expression("K"["Y"])),
                   k=c(kavg, 
                       kstats$kavg[kstats$regime==3], 
                       kvals1$ky[kvals1$year==2025]))

# Build MNPL labels
mnpl_labels <- tibble(ref=c(expression("MNPL"["AVG"]), 
                         expression("MNPL"["R"]),  
                         expression("MNPL"["Y"])),
                   mnpl=c(mnpl_avg, 
                          kstats$mnpl_avg[kstats$regime==3], 
                          kvals1$mnpl[kvals1$year==2025]))


# Simulate R data
################################################################################

# 4 is pretty good; tried up to 8
set.seed(4)

# Time series characteristics
yrs <- 1980:2025
nyrs <- length(1980:2025)

# Regime characteristics
mus <- c(0.09, 0.08, 0.085, 0.095)
cv <- 0.08
sds <- cv * mus 
nsteps <- c(11, 11, 12, 12)

# Confirm
sum(nsteps) == nyrs

# Loop through regimes and create data
x <- 1
rvals <- purrr::map_df(1:length(mus), function(x){
  
  # Retrieve values
  mu <- mus[x]
  sd <- sds[x]
  nstep <- nsteps[x]
  
  # Random walk increments
  eps <- rnorm(nstep - 1, mean = 0, sd = sd)
  
  # Initialize series at the target mean
  vec <- numeric(nstep)
  vec[1] <- mu
  
  # Perform walk
  for (t in 2:nstep) {
    vec[t] <- vec[t - 1] + eps[t - 1]
  }
  
  # Build data
  df <- tibble(regime=x,
               mu=mu,
               sd=sd,
               ry=vec)
  
})

# Add years
rvals1 <- rvals %>% 
  mutate(year=yrs) %>% 
  select(year, everything())

# Regime stats
rstats <- rvals1 %>% 
  group_by(regime) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            ravg=mean(ry), 
            rsd=sd(ry)) %>% 
  ungroup() %>% 
  mutate(r_lo=ravg-rsd,#*1.96,
         r_hi=ravg+rsd)#*1.96)

# R avg
ravg <- mean(rvals1$ry)

# Build r labels
r_labels <- tibble(ref=c(expression("r"["AVG"]), 
                         expression("r"["R"]),  
                         expression("r"["Y"])),
                   r=c(ravg, 
                       rstats$ravg[rstats$regime==4], 
                       rvals1$ry[rvals1$year==2025]))



# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.border = element_blank(),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# K values
g1 <- ggplot(kvals1, aes(x=year, y=ky)) +
  # Plot regime CI
  geom_rect(data=kstats, mapping=aes(xmin=yr1, 
                                     xmax=yr2, 
                                     ymin=k_lo, 
                                     ymax=k_hi, 
                                     fill=as.character(regime)),
            show.legend = F, alpha=0.5, inherit.aes = F) +
  # Plot regime average
  geom_segment(data=kstats, mapping=aes(x=yr1, 
                                        xend=yr2, 
                                        y=kavg, 
                                        color=as.character(regime)), 
               show.legend = F, lwd=1.5, inherit.aes = F) +
  # Plot data
  geom_line() +
  geom_point(size=1) +
  # Plot regime CI
  geom_rect(data=kstats, mapping=aes(xmin=yr1, 
                                     xmax=yr2, 
                                     ymin=mnpl_lo, 
                                     ymax=mnpl_hi, 
                                     fill=as.character(regime)),
            show.legend = F, alpha=0.5, inherit.aes = F) +
  # Plot regime average
  geom_segment(data=kstats, mapping=aes(x=yr1, 
                                        xend=yr2, 
                                        y=mnpl_avg, 
                                        color=as.character(regime)), 
               show.legend = F, lwd=1.5, inherit.aes = F) +
  # Plot data
  geom_line(data=kvals1, mapping=aes(x=year, y=mnpl), inherit.aes = F) +
  geom_point(data=kvals1, mapping=aes(x=year, y=mnpl), size=1, inherit.aes = F) +
  # Plot average
  geom_segment(x=1980, xend=2025, y=kavg, color="grey50", linetype="dashed") +
  geom_segment(x=1980, xend=2025, y=mnpl_avg, color="grey50", linetype="dashed") +
  # Plot labels
  geom_text(data=k_labels, mapping=aes(y=k, label=ref), 
            x=2026, hjust=0, size=2.4, inherit.aes = F) +
  geom_text(data=mnpl_labels, mapping=aes(y=mnpl, label=ref), 
            x=2026, hjust=0, size=2.4, inherit.aes = F) +
  # Labels
  labs(x="Year", 
       y="Carrying capacity (K)\nMaximum net productivity level (MNPL)", 
       tag="A", title=expression("Climate-linked K and MNPL")) +
  # Axes
  scale_y_continuous(lim=c(0, NA), breaks=seq(0, 25000, 5000)) +
  scale_x_continuous(breaks=seq(1980, 2025, 5), lim=c(1980, 2033)) +
  # Theme
  theme_bw() + my_theme
g1

# R values
g2 <- ggplot(rvals1, aes(x=year, y=ry)) +
  # Plot regime CI
  geom_rect(data=rstats, mapping=aes(xmin=yr1, 
                                     xmax=yr2, 
                                     ymin=r_lo, 
                                     ymax=r_hi, 
                                     fill=as.character(regime)),
            show.legend = F, alpha=0.5, inherit.aes = F) +
  # Plot regime average
  geom_segment(data=rstats, mapping=aes(x=yr1, 
                                        xend=yr2, 
                                        y=ravg, 
                                        color=as.character(regime)), 
               show.legend = F, lwd=1.5, inherit.aes = F) +
  # Plot data
  geom_line() +
  geom_point(size=1) +
  # Plot average
  geom_segment(x=1980, xend=2025, y=ravg, color="grey50", linetype="dashed") +
  # Plot labels
  geom_text(data=r_labels, mapping=aes(y=r, label=ref), 
            x=2026, hjust=0, size=2.4, inherit.aes = F) +
  # Labels
  labs(x="Year", y="Intrinsic growth rate (r)", tag="B", title=expression("Climate-linked R"["MAX"])) +
  # Axes
  scale_y_continuous(lim=c(0, NA), breaks=seq(0,0.14, 0.02)) +
  scale_x_continuous(breaks=seq(1980, 2025, 5), lim=c(1980, 2028)) +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "FigX_climate_linked_mnpl_rmax.png"), 
       width=6.5, height=3.5, units="in", dpi=600, bg="white")

