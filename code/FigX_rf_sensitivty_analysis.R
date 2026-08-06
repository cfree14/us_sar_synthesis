
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

# 2024 data
data1 <- data_orig %>% 
  # Reduce to 2024
  filter(year==2024)

# Reduce to ones with unknown/non-depleted status
data2 <- data1 %>% 
  # Reduce to ones with unknown/non-depleted status
  filter(osp_status %in% c("Unknown", "Non-depleted")) %>% 
  # Simplify
  select(region, subregion, group, comm_name, stock,
         n_min, r_max, rf, pbr, sim_fisheries, osp_status, esa_status)

# Reduce to ones with PBR and fisheries SI/M
data3 <- data2 %>% 
  filter(!is.na(pbr) & !is.na(sim_fisheries))

# Number of stocks with default
nrow(data1)
nrow(data2)
nrow(data3)
table(data3$osp_status)

count(data3, group, osp_status)

  
# Perform analysis
################################################################################

# Build output
stocks <- sort(unique(data3$stock))
x <- stocks[1]
output <- purrr::map_df(stocks, function(x){
  
  # RF df
  rf_df <- tibble(stock=x,
                  rf_sim=seq(0.5, 1.0, 0.01))
  
  # Subset data
  sdata <- data3 %>% 
    # Reduce
    filter(stock==x) %>% 
    # Expand
    full_join(rf_df, by="stock") %>% 
    # Compute
    mutate(pbr_sim=n_min*r_max/2*rf_sim,
           strategic_yn=ifelse(sim_fisheries>pbr_sim, "yes", "no"))
  

})
  
# Compute stats
stats <- output %>% 
  # Summarize
  group_by(group, osp_status, rf_sim) %>% 
  summarize(n=n(),
            nstocks=n_distinct(stock),
            nstrategic=sum(strategic_yn=="yes"),
            pstrategic=nstrategic/nstocks) %>% 
  ungroup()

# Extract points
pts <- stats %>% 
  filter(pstrategic==0) %>% 
  group_by(group, osp_status) %>% 
  filter(rf_sim==min(rf_sim)) %>% 
  ungroup() %>% 
  # Remove the ones that are already 100% non-strat at 0.5
  filter(rf_sim!=0.5)

# Export
write.csv(stats, file=file.path("output/rf_sensitivity_analysis.csv"), row.names = F)

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
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats, aes(x=rf_sim, y=pstrategic, color=group, linetype=osp_status)) +
  # Lines
  geom_line(linewidth = 0.7) +
  geom_point(data=pts, size=1.5) + # mapping=aes(shape=osp_status), 
  geom_text(data=pts, mapping=aes(label=rf_sim), show.legend = F, vjust=1.8, size=2) +
  # Labels
  labs(x=expression("R"["F"]*" value"), y="Percentage of stocks with\nfisheries SI/M exceeding the PBR") +
  # scale_x_continuous(lim=c(0, NA), breaks=seq(0,0.2, 0.02)) +
  scale_y_continuous(labels=scales::percent_format(), breaks=seq(0, 0.4, 0.05)) +
  # Legend
  scale_color_discrete(name="Group") +
  scale_linetype_manual(name="OSP status", values=c("solid", "dashed")) +
  # scale_shape_manual(name="OSP status", values=c(16, 18)) +
  # Theme
  theme_bw() + my_theme
g 

ggsave(g, filename=file.path(plotdir, "FigSX_rf_sensitivity_analysis.png"), 
       width=5.5, height=3.5, units="in", dpi=600, bg="white")

