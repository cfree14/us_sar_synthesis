
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

# Read RF guidelines
rf_guidelines <- readxl::read_excel("tables/rf_values.xlsx")


# Build data
################################################################################

# RF groups
rf_groups <- c(rev(c("Size unknown",
                     "<1,500 animals", 
                     "1,500-5,000 animals (CV ≤ 0.5)",
                     "1,500-7,500 animals (CV > 0.5)",
                     ">5,000 animals (CV ≤ 0.5)",
                     ">7,500 animals (CV > 0.5)")),
               "Threatened",
               rev(c("Depleted",
                     "Unknown",
                     "Non-depleted")))

# Prep data
data <- data_orig %>% 
  # Filter
  filter(group!="USFWS marine mammals" & year==2024) %>% 
  # Order ESA status
  mutate(esa_status=factor(esa_status, levels=c("Endangered", "Threatened", "Not listed"))) %>% 
  # Add RF group
  mutate(esa_listed=ifelse(esa_status=="Not listed", "Not listed", "Listed"),
         rf_group_esa=case_when(esa_status=="Endangered" & n_min < 1500 ~ "<1,500 animals",
                                esa_status=="Endangered" & n_min >= 1500 & n_min < 5000 & n_cv <= 0.5 ~ "1,500-5,000 animals (CV ≤ 0.5)",
                                esa_status=="Endangered" & n_min >= 1500 & n_min < 7500 & n_cv > 0.5 ~ "1,500-7,500 animals (CV > 0.5)",
                                esa_status=="Endangered" & n_min >= 5000 & n_cv <= 0.5 ~ ">5,000 animals (CV ≤ 0.5)",
                                esa_status=="Endangered" & n_min >= 7500 & (n_cv > 0.5 | is.na(n_cv)) ~ ">7,500 animals (CV > 0.5)",
                                esa_status=="Endangered" & is.na(n_min) ~ "Size unknown",
                                T ~ esa_status), 
         rf_group=case_when(esa_listed=="Listed" ~ rf_group_esa,
                            T ~ osp_status)) %>% 
  # Order groups
  mutate(rf_group=factor(rf_group, levels=rf_groups)) %>% 
  # Simplify
  select(region, subregion, group, stock, n_min, n_cv, 
        rf, 
        esa_listed, esa_status, osp_status, 
        rf_group_esa, rf_group) 


# Inspect
freeR::complete(data)

# Order RF guidelines
rf_guidelines_plot <- rf_guidelines %>% 
  mutate(pop_size=factor(pop_size, levels=rf_groups)) %>% 
  mutate(esa_status="Endangered",
         esa_status=factor(esa_status, levels=levels(data$esa_status)))


# Inspect the outliers
################################################################################

data %>% 
  filter(rf==1) %>% 
  select(subregion, group, stock, rf, osp_status)

# Define wierd ones
wierd_vec <- c(# Depleted with RF < 0.5
               "Killer whale (AT1 Transient)")
               # "Common bottlenose dolphin (Mississippi Sound, Lake Borgne, Bay Boudreau)")
               # Endangered with RF > 0.1
               # "Blue whale (Eastern North Pacific)",       
               # "Fin whale (California-Oregon-Washington)", 
               # "Sperm whale (Hawaii)",                  
               # "Bowhead whale (Western Arctic)" )

# Explain wierd ones
# 1) AT1 killer whale: 
# The recovery factor (FR) for this stock is 0.1, as the 
# stock is considered depleted under the Marine Mammal Protection Act (MMPA)
# and there has been no recruitment into the stock since 1984.
# 2) MI Sound bottlenose dolphin: 
# The recovery factor is 0.45 because the CV of the shrimp trawl mortality
# estimate for Mississippi and Alabama BSE stocks is greater than 0.6 (Wade and Angliss 1997).
# 3) Blue whale
# a recovery factor of 0.2 (for an endangered species with a minimum abundance
# greater than 1,500 and unknown population trend)
# 4) Fin whale
# recovery factor of 0.5 (for an endangered species, with Nmin > 5,000 and 
# CVNmin < 0.50, Taylor et al. 2003),
# 5) Sperm whale
# recovery factor of 0.2 (for an endangered species with Nmin > 1,500 and 
# CVN <= 0.50, with low vulnerability to extinction; (Taylor et al. 2003)
# 6) Bowhead whale
# The recovery factor (FR) for this stock has been set at 0.5 rather than the 
# default value of 0.1 for endangered species because population levels are not known to be
# decreasing (Givens et al. 2021a, 2021b) in the presence of known take (NMFS 2023a)

# Subset wierd ones and shorten names
wierd_df <- data %>% 
  filter(stock %in% wierd_vec) %>% 
  mutate(stock_label=recode(stock, 
                            "Killer whale (AT1 Transient)" = "Killer whale\n(AT1 Transient)",
                            "Common bottlenose dolphin (Mississippi Sound, Lake Borgne, Bay Boudreau)" = "Bottlenose dolphin\n(Mississippi Sound)",
                            "Blue whale (Eastern North Pacific)" = "Blue whale (ENP)",       
                            "Fin whale (California-Oregon-Washington)" = "Fin whale (CA-OR-WA)", 
                            "Sperm whale (Hawaii)" = "Sperm whale (HI)",                  
                            "Bowhead whale (Western Arctic)" = "Bowhead whale (W. Arctic)"))


# Plot data
################################################################################

# Ref line
ref_lines_end <- tibble(esa_status="Endangered",
                        rf=seq(0.1, 0.5, 0.1)) 
ref_lines_other <- expand.grid(esa_status=c("Threatened", "Not listed"),
                               rf=c(0.4, 0.45, 0.48, 0.5))
ref_lines <- bind_rows(ref_lines_end, ref_lines_other) %>% 
  mutate(esa_status=factor(esa_status, levels=levels(data$esa_status)))

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(y=rf_group, x=rf)) +
  # Facet
  facet_wrap(~esa_status, scales="free_y", space="free_y", strip.position="right") +
  # Ref lines
  geom_vline(data=ref_lines, mapping=aes(xintercept=rf), 
             color="black", linetype="dotted", inherit.aes = F, linewidth=0.5) +
  # Guidelines
  geom_point(data=rf_guidelines_plot, inherit.aes = F,
             mapping=aes(y=pop_size, 
                         x=rf_not_vulnerable, 
                         color=trend), pch=16, size=7) +
  # Points
  geom_jitter(pch=21, size=3, height=0.2, width=0, fill="white") +
  # Label wierd ones
  # ggrepel::geom_text_repel(data=wierd_df, mapping=aes(label=stock_label), 
  #                          min.segment.length=0.1, size=3) +
  # Labels
  labs(x=expression("Recovery factor (R"["F"]*")"), y="") +
  # Legend
  scale_color_manual(name="Trend default", values=c("black", "red", "blue", "grey70")) +
  # Axes
  scale_x_continuous(breaks=seq(0, 1, 0.1)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.8))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig8_rf_values.png"), 
       width=6.5, height=4.5, units="in", dpi=600, bg="white")


# RF = 0.7: Harbor seal (Bristol Bay), Harbor seal (South Kodiak)
# RF = 0.3: Harbor seal (Aleutian Islands), Harbor seal (Glacier Bay/Icy Strait), Harbor seal (Lynn Canal/Stephens Passage)
# RF = 1.0: 
# RF = 0.44: False killer whale (Hawaii Pelagic)


