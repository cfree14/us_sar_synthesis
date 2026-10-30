
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

# Number of animals by group (all stocks)
stats1 <- data_orig %>% 
  # Remove one consitent stock that jumps from huge
  filter(stock!="Harp seal (Western North Atlantic)") %>% 
  # Identify N to use
  mutate(n_use=ifelse(!is.na(n_est), n_est, n_min)) %>% 
  # Summarise
  group_by(group, year) %>% 
  summarize(n_use=sum(n_use, na.rm=T)) %>% 
  ungroup() %>% 
  # Add catg
  mutate(catg="All stocks (n=285)")

# Consistent stocks
stocks_consistent <- data_orig %>%
  # Remove one consitent stock that jumps from huge
  filter(stock!="Harp seal (Western North Atlantic)") %>% 
  # Summarise
  group_by(stock) %>% 
  summarise(year1=min(year),
            year2=max(year)) %>% 
  ungroup() %>% 
  filter(year1==1995 & year2==2024)

# Number of animals by group (consistent stocks)
stats2 <- data_orig %>% 
  # Consistent stocks
  filter(stock %in% c(stocks_consistent$stock)) %>% 
  # Identify N to use
  mutate(n_use=ifelse(!is.na(n_est), n_est, n_min)) %>% 
  # Summarize
  group_by(group, year) %>% 
  summarize(n_use=sum(n_use, na.rm=T)) %>% 
  ungroup() %>% 
  # Add catg
  mutate(catg="Consistent stocks (n=159)")

# Number of stocks
n_distinct(data_orig$stock) -1
n_distinct(stocks_consistent$stock)

# Compute overall
stats1_tot <- stats1 %>% 
  group_by(catg, year) %>% 
  summarize(n_use=sum(n_use, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(group="Overall")
stats2_tot <- stats2 %>% 
  group_by(catg, year) %>% 
  summarize(n_use=sum(n_use, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(group="Overall")

# Group order
groups <- stats1 %>% 
  filter(year==2024) %>% 
  arrange(desc(n_use)) %>% pull(group)

# Merge 
stats <- bind_rows(stats1, stats1_tot, stats2, stats2_tot) %>% 
  # Order groups
  mutate(group=factor(group, levels=c("Overall", groups)))

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   plot.tag = element_text(size=10),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.box.spacing = grid::unit(0, "pt"),
                   legend.margin = margin(t = 0, r = 0, b = 5, l = 0),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

g <- ggplot(stats, aes(x=year, y=n_use/1e3, color=catg)) +
  facet_wrap(~group, scales="free_y", ncol=4) +
  geom_line() +
  # Labels
  labs(x="Year", y="Thousands of animals") +
  # Axes
  scale_x_continuous(breaks=seq(1995, 2025, 5)) +
  scale_y_continuous(lim=c(0,NA)) +
  # Scale
  scale_color_manual(name="", values=c("black", "grey60")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top") # legend.position = c(0.85, 0.2)
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_abundance_over_time.png"), 
       width=6.5, height=4.25, units="in", dpi=600, bg="white")





