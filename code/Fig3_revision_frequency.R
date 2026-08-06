
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
data_orig <- readRDS(data, file=file.path(outdir, "US_sars_data.Rds")) %>% 
  filter(group!="USFWS marine mammals")


# Build data
################################################################################

# 2024 stocks
stocks_do <- data_orig %>% 
  filter(year==2024) %>% pull(stock)

# Build data
data <- data_orig %>% 
  # Extant stocks
  filter(stock %in% stocks_do) %>% 
  # Reduce to revisions
  filter(revised_yn=="yes" | year==2024) %>% 
  # Get last two revisions
  arrange(stock, desc(year)) %>% 
  group_by(stock) %>% 
  slice(1:2) %>% 
  ungroup() %>% 
  # Summarize
  group_by(region, subregion, comm_name, group, stock) %>% 
  summarize(nyr=n(), 
            status=strategic_yn[year==min(year)],
            interval_yr=max(year)-min(year)) %>% 
  ungroup() %>% 
  # Recode status
  mutate(status=ifelse(is.na(status), "Unknown", status) %>% factor(., levels=c("Strategic", "Non-strategic", "Unknown"))) %>% 
  # Add target year
  mutate(target_yr=ifelse(status=="Strategic", 1, 3)) %>% 
  # Old?
  mutate(old_yn=ifelse(interval_yr>target_yr, "yes", "no"))

# % of all that are old
sum(data$old_yn=="yes") / nrow(data) *100
  
# By status
table(data$status)
data %>% 
  count(status, old_yn) %>% 
  group_by(status) %>% 
  mutate(prop=n/sum(n)*100)

# By region
data %>% 
  count(subregion, old_yn) %>% 
  group_by(subregion) %>% 
  mutate(prop=n/sum(n)*100) %>% 
  filter(old_yn=="yes") %>% 
  arrange(desc(prop))

# Build stats
stats <- data %>% 
  # Summarize
  count(group, status, target_yr, interval_yr) %>% 
  # Compue
  group_by(group, status) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  # Build status labels
  group_by(status) %>%
  mutate(nstatus=sum(n),
         nstatus_above=sum(n[interval_yr>target_yr]),
         pstatus_above=nstatus_above/nstatus,
         status_label=paste0(status, " (", round(pstatus_above*100, 0), "%)")) %>%
  ungroup() %>%
  # Build group labels
  group_by(group, status) %>% 
  mutate(ncatg=sum(n),
         ncatg_above=sum(n[interval_yr>target_yr]),
         pcatg_above=ncatg_above/ncatg,
         group_label=paste0(group, " (", round(pcatg_above*100, 0), "%)")) %>% 
  ungroup() 

# Build stats by region
stats2 <- data %>% 
  # Summarize
  count(subregion, status, target_yr, interval_yr) %>% 
  # Compue
  group_by(subregion, status) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
# Build status labels
  group_by(status) %>%
  mutate(nstatus=sum(n),
         nstatus_above=sum(n[interval_yr>target_yr]),
         pstatus_above=nstatus_above/nstatus,
         status_label=paste0(status, " (", round(pstatus_above*100, 0), "%)")) %>%
  ungroup() %>%
  # Build subregion labels
  group_by(subregion, status) %>% 
  mutate(ncatg=sum(n),
         ncatg_above=sum(n[interval_yr>target_yr]),
         pcatg_above=ncatg_above/ncatg,
         subregion_label=paste0(subregion, " (", round(pcatg_above*100, 0), "%)")) %>% 
  ungroup() 


# Plot data
################################################################################

# Ref lines
ref_lines <- tibble(status=c("Strategic", "Non-strategic", "Unknown") %>% factor(., levels=levels(stats$status)),
                    interval_yr=c(1, 3, 3))

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot by group
g1 <- ggplot(stats, aes(x=interval_yr, 
                        fill=prop, 
                        y=tidytext::reorder_within(group_label, pcatg_above, status), 
                        size=prop)) +
  facet_wrap(~status, ncol=1, space="free_y", scales="free_y") +
  # Reference lines
  geom_vline(data=ref_lines, mapping=aes(xintercept=interval_yr), 
             linetype="dashed", color="grey30", inherit.aes = F) +
  # Data
  geom_point(pch=21) +
  # Labels
  labs(x="Years since last revision", y="", tag="A") +
  # Axes
  tidytext::scale_y_reordered() +
  scale_x_continuous(lim=c(0, NA), breaks=c(1, 3, seq(0, 16, 2))) +
  # Legends
  scale_size_continuous(name="% of stocks", 
                        labels=scales::percent_format(),
                        lim=c(0.01, 1)) +
  scale_fill_gradientn(name="% of stocks", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       labels=scales::percent_format(),
                       lim=c(0.0, 1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g1

g2 <- ggplot(stats2, aes(x=interval_yr, 
                        fill=prop, 
                        y=tidytext::reorder_within(subregion_label, pcatg_above, status), 
                        size=prop)) +
  facet_wrap(~status, ncol=1, space="free_y", scales="free_y") +
  # Reference lines
  geom_vline(data=ref_lines, mapping=aes(xintercept=interval_yr), 
             linetype="dashed", color="grey30", inherit.aes = F) +
  # Data
  geom_point(pch=21) +
  # Labels
  labs(x="Years since last revision", y="", tag="B") +
  # Axes
  tidytext::scale_y_reordered() +
  scale_x_continuous(lim=c(0, NA), breaks=c(1, 3, seq(0, 16, 2))) +
  # Legends
  scale_size_continuous(name="% of stocks", 
                        labels=scales::percent_format(),
                        lim=c(0.01, 1)) +
  scale_fill_gradientn(name="% of stocks", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       labels=scales::percent_format(),
                       lim=c(0.0, 1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.5, "cm"))
g2


# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.42, 0.58))

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_revision_frequency.png"), 
       width=6.5, height=4.25, units="in", dpi=600, bg="white")

