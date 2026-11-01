
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/sars/processed"
plotdir <- "figures"
tabledir <- "tables"

# Read data
data_orig <- readRDS(data, file=file.path(outdir, "US_sars_data.Rds"))

# Examine reason for MMPA status: Are all ESA-listed depleted? Why are non-ESA listed depleted?
# Examine reason for strategic status


# Are any 2024 stocks with SI/M > PBR not strategic?
################################################################################

check_strategic <- data_orig %>% 
  filter(year==2024 & strategic_yn=="Non-strategic" & ((sim_total>pbr) | (sim_fisheries>pbr)))


# Build data
################################################################################

# Prep data
data <- data_orig %>% 
  # Reduce to 2024
  filter(year==2024 & strategic_yn=="Strategic") %>% 
  # PBR test
  mutate(reason_pbr=sim_total>pbr,
         reason_mmpa=osp_status=="Depleted",
         reason_esa=esa_status %in% c("Endangered", "Threatened")) %>% 
  # Build reason string
  mutate(reason_catg=paste(reason_esa, reason_mmpa, reason_pbr, sep="-")) %>% 
  # Simplify
  select(region, group, stock, comm_name, area, esa_status, reason_catg, 
         reason_esa, reason_mmpa, reason_pbr) %>% 
  # Format reason catg
  mutate(reason_catg=recode(reason_catg,
                            "FALSE-FALSE-FALSE" = "None",   
                            "FALSE-FALSE-NA" = "None (PBR|M/SI unknown)",  
                            "FALSE-FALSE-TRUE" = "M/SI > PBR", 
                            "FALSE-TRUE-FALSE" = "Depleted (M/SI<PBR)", 
                            "FALSE-TRUE-NA" = "Depleted (PBR|M/SI unknown)", 
                            "FALSE-TRUE-TRUE" = "Depleted & M/SI>PBR", 
                            "TRUE-TRUE-NA" = "Endangered (PBR|M/SI unknown)",
                            "TRUE-TRUE-FALSE" = "Endangered (M/SI<PBR)", 
                            "TRUE-TRUE-TRUE" = "Endangered & M/SI>PBR)")) %>% 
  # Gather
  gather(key="reason", value="yesno", 8:ncol(.)) %>% 
  # Factor reason
  mutate(reason=recode_factor(reason, 
                       "reason_esa"="ESA\nstatus", 
                       "reason_mmpa"="MMPA\nstatus",
                       "reason_pbr"="M/SI >\nPBR")) %>% 
  # Factor ESA status
  mutate(esa_status=factor(esa_status, c("Endangered", "Threatened", "Not listed"))) %>% 
  # Add short stock
  mutate(stock_short=paste0(comm_name, " (", substr(area, 1, 12), ")"))

# Number of reason stats
stats <- data %>% 
  group_by(esa_status, stock) %>% 
  summarize(nreasons=sum(yesno)) %>% 
  ungroup() %>% 
  arrange(esa_status, desc(nreasons))

table(data$reason_catg)

# Order categories
reason_catgs <- c("Endangered & M/SI>PBR", "Endangered (M/SI<PBR)", "Endangered (PBR|M/SI unknown)",
                  "Depleted & M/SI>PBR", "Depleted (M/SI<PBR)", "Depleted (PBR|M/SI unknown)", 
                  "M/SI > PBR", "None",  "None (PBR|M/SI unknown)")

# Build stock key
stock_key <- data %>% 
  select(stock, stock_short, esa_status, reason_catg) %>%
  unique() %>% 
  mutate(reason_catg=factor(reason_catg, reason_catgs)) %>% 
  arrange(esa_status, reason_catg) %>% 
  # Add big picture reason
  mutate(reason_catg_big=case_when(grepl("Endangered", reason_catg) ~ "ESA",
                                   grepl("Depleted", reason_catg) ~ "MMPA",
                                   reason_catg=="M/SI > PBR" ~ "PBR",
                                   T ~ "Expert opinion"))

table(stock_key$reason_catg_big) / nrow(stock_key) * 100 

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.tag = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# # Plot data
# g <- ggplot(data, aes(y=factor(stock, stats$stock), x=reason, fill=yesno)) +
#   facet_wrap(~esa_status, scales="free_y", space="free_y") +
#   geom_tile() +
#   # Labels
#   labs(x="Reason") +
#   # Legend
#   scale_fill_manual(name="", values=c("white", "red"), na.value="grey80") +
#   # Theme
#   theme_bw() + my_theme +
#   theme(legend.position="none")
# g

# Plot data
g <- ggplot(data, aes(y=factor(stock, stock_key$stock), x=reason, fill=yesno)) +
  facet_wrap(~esa_status, scales="free_y", space="free_y", strip.position="right") +
  geom_tile() +
  # Labels
  labs(x="Reason") +
  # Legend
  scale_fill_manual(name="", values=c("white", "red"), na.value="grey80") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS10_strategic_reasons.png"), 
       width=6.5, height=8.5, units="in", dpi=600, bg="white")


# Version 2
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.x=element_text(size=6),
                   axis.title=element_text(size=8),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data %>% filter(esa_status!="Not listed"), 
             aes(y=factor(stock_short, stock_key$stock_short), x=reason, fill=yesno)) +
  facet_wrap(~esa_status, scales="free_y", space="free_y", strip.position="right") +
  geom_tile() +
  # Labels
  labs(x="Reason") +
  # Legend
  scale_fill_manual(name="", values=c("white", "red"), na.value="grey80") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g1

# Plot more
g2 <- ggplot(data %>% filter(esa_status=="Not listed"), 
             aes(y=factor(stock_short, stock_key$stock_short), x=reason, fill=yesno)) +
  facet_wrap(~esa_status, scales="free_y", space="free_y", strip.position="right") +
  geom_tile() +
  # Labels
  labs(x="Reason") +
  # Legend
  scale_fill_manual(name="", values=c("white", "red"), na.value="grey80") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, widths=c(0.48, 0.52), nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "FigS10_strategic_reasons_wide.png"), 
       width=6.5, height=6.5, units="in", dpi=600, bg="white")


# Reason for depleted
################################################################################

# Most depleted stocks are depleted because Endangered/Threatened
# The rest are depleted based on expert judgement; a lot are really old
# Big declines, small population sizes

# Number of depleted stocks
data_orig %>% 
  # Reduce to 2024
  filter(year==2024) %>% 
  count(osp_status) 

# Number of depleted stocks by ESA status
data_orig %>% 
  # Reduce to 2024
  filter(year==2024) %>% 
  count(osp_status, esa_status) 

# Prep data
dep_stocks <- data_orig %>% 
  # Reduce to 2024
  filter(year==2024 & osp_status=="Depleted" & esa_status=="Not listed") %>% 
  # Simplify
  select(subregion, stock, n_est, n_min)

# Export
write.csv(dep_stocks, file=file.path(tabledir, "TableS4_depleted_stocks_not_on_esa.csv"), row.names=F)
