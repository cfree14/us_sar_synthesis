
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


# Build abundance data
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

# Split data
groups1 <- levels(stats$group)[1:4]
data1 <- stats %>% 
  filter(group %in% levels(stats$group)[1:4])
data2 <- stats %>% 
  filter(!group %in% levels(stats$group)[1:4])


# Build trend data
################################################################################

# 2024 trends by group
trends_grp <- data_orig %>% 
  filter(year==2024) %>% 
  group_by(group, trend) %>% 
  summarize(nstocks=n()) %>% 
  ungroup()

# 2024 trends overall
trends_tot <- trends_grp %>% 
  group_by(trend) %>% 
  summarize(nstocks=sum(nstocks)) %>% 
  ungroup() %>% 
  mutate(group="Overall")

# Merge trends data
trends <- bind_rows(trends_grp, trends_tot) %>% 
  # Order groups
  mutate(group=factor(group, levels=levels(stats$group))) %>% 
  # Order trends
  mutate(trend=factor(trend, levels=c("Decrease", "Stable", "Increase", "Unknown"))) %>% 
  # Calculate percentages
  group_by(group) %>% 
  mutate(prop=nstocks/sum(nstocks)) %>% 
  ungroup() %>% 
  # Add dummy column
  mutate(dummy="")

# Split data
trends1 <- trends %>% 
  filter(group %in% levels(stats$group)[1:4])
trends2 <- trends %>% 
  filter(!group %in% levels(stats$group)[1:4])

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
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

g1 <- ggplot(data1, aes(x=year, y=n_use/1e6, color=catg)) +
  facet_wrap(~group, scales="free_y", ncol=4) +
  geom_line() +
  # Labels
  labs(x="Year", y="Millions of animals") +
  # Axes
  scale_x_continuous(breaks=seq(1995, 2025, 5)) +
  scale_y_continuous(lim=c(0,NA)) +
  # Scale
  scale_color_manual(name="", values=c("black", "grey60")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x=element_blank(), 
        legend.position = "top") # legend.position = c(0.85, 0.2)
g1

g2 <- ggplot(data2, aes(x=year, y=n_use/1e3, color=catg)) +
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
  theme(axis.title.x=element_blank(),
        legend.position = "none") # legend.position = c(0.85, 0.2)
g2

# Plot trends
g1a <- ggplot(trends1, aes(x=prop, fill=trend, y=dummy)) +
  facet_wrap(~group, scales="free_y", ncol=4) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Axes
  labs(y="", x="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Trend", values=c("red", "white", "blue", "grey90")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5))
g1a

# Plot trends
g2a <- ggplot(trends2, aes(x=prop, fill=trend, y=dummy)) +
  facet_wrap(~group, scales="free_y", ncol=4) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Axes
  labs(y="", x="Percent of 2024 stocks") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Trend", values=c("red", "white", "blue", "grey90")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="bottom",
        legend.key.size=unit(0.3, "cm"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5))
g2a

# Merge
g <- gridExtra::grid.arrange(g1, g1a, g2, g2a, ncol=1, heights=c(0.40, 0.1, 0.37, 0.17))

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_abundance_over_time_new.png"), 
       width=6.5, height=5, units="in", dpi=600, bg="white")





