
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

# Calculate number of years between revisions
rdata <- data_orig %>% 
  # Simplify
  select(region, subregion, group, comm_name, stock, year, revised_yn) %>% 
  # Reduce to relevant years (revised years and last year)
  group_by(region, group, stock) %>% 
  filter(revised_yn=="yes" | year==max(year)) %>% 
  # Arrange
  arrange(region, subregion, group, comm_name, stock, year) %>% 
  # Eliminate stocks that only existed for a year 
  # (time since revision is not possible to calculate)
  mutate(nrevisions=n()) %>% 
  filter(nrevisions>1) %>% 
  # Add time since revision
  mutate(years_since_revision=c(0, year[2:n()]-year[1:(n()-1)])) %>% 
  ungroup()

# Identify the maximum number of years between revisions
rdata_max <- rdata %>% 
  group_by(region, group, stock) %>% 
  summarize(revision_yr_max=max(years_since_revision)) %>% 
  ungroup()

# Build data
stats <- data_orig %>% 
  group_by(region, group, stock) %>% 
  summarize(yr1=min(year),
            yr2=max(year),
            nyrs=length(yr1:yr2),
            nrevisions=sum(revised_yn=="yes")-1, # Minus 1 b/c first SAR doesn't count
            revision_yr_avg=nyrs/nrevisions) %>% 
  ungroup() %>% 
  # Add max time since revision
  left_join(rdata_max)

# Build data for facet plottings
stats1 <- stats %>% 
  mutate(category="Region",
         subcategory=region)
stats2 <- stats %>% 
  mutate(category="Taxanomic group", 
         subcategory=group)
stats3 <- bind_rows(stats1, stats2)

# Quick plot
ggplot(stats, aes(x=revision_yr_max)) +
  facet_wrap(~region) +
  geom_histogram(binwidth = 1)


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=10),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Max
g1 <- ggplot(stats3, aes(y=subcategory, x=revision_yr_max)) +
  facet_wrap(~category, scales="free_y", space="free_y") +
  geom_violin() +
  # Reference line
  geom_vline(xintercept=3, color="red") +
  # Labels
  labs(x="Maximum number of years\nbetween SAR revisions", y="", tag="A") +
  # Theme
  theme_bw() + my_theme
g1

# Average
g2 <- ggplot(stats3, aes(y=subcategory, x=revision_yr_avg)) +
  facet_wrap(~category, scales="free_y", space="free_y") +
  geom_violin() +
  # Reference line
  geom_vline(xintercept=3, color="red") +
  # Labels
  labs(x="Average number of years\nbetween SAR revisions", y="", tag="B") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.55, 0.45))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_revision_frequency.png"), 
       width=6.5, height=6.5, units="in", dpi=600, bg="white")




