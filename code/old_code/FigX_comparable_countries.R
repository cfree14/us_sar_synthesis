

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/Whales/us_sar_synthesis_data/list_of_fisheries/"
outdir <- "data/sars/processed"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel("data/comparability_findings/2025_comparability_findings.xlsx")


# Spatial data
################################################################################

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")
world_sm_orig <- rnaturalearth::ne_countries(scale="small", returnclass = "sf") %>% sf::st_transform(wgs84)
world_lg_orig <- rnaturalearth::ne_countries(scale="large", returnclass = "sf") %>% sf::st_transform(wgs84)
world_tiny_orig <- rnaturalearth::ne_countries(type="tiny_countries", returnclass="sf") %>% sf::st_transform(wgs84)


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  rename(country_orig=country) %>% 
  mutate(country_orig=recode(country_orig, 
                             "France—St. Pierre et Miquelon"="Saint Pierre and Miquelon"),
         country=countrycode::countrycode(country_orig, "country.name", "country.name"),
         iso3=countrycode::countrycode(country, "country.name", "iso3c"))

# Spatialize
world_sm <- world_sm_orig %>%
  # Reduce
  select(geounit, gu_a3) %>%
  # Clean
  mutate(iso3=countrycode::countrycode(geounit, "country.name", "iso3c"),
         iso3=ifelse(!is.na(iso3), iso3, gu_a3)) %>%
  # Add metadata
  left_join(data %>% select(iso3, loff_status), by="iso3") %>% 
  # Factor
  mutate(loff_status=case_when(iso3=="USA" ~ "All", 
                               is.na(loff_status) ~ "Inland", 
                               T ~ loff_status), 
         loff_status=factor(loff_status,
                            levels=c("All", "Some", "None", "No submission", "Inland")))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_blank(),
                   axis.title=element_blank(),
                   axis.ticks=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.margin = margin(0,0,0,0),
                   legend.position = "top",
                   legend.key.size=unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(world_sm, aes(fill=loff_status)) +
  geom_sf(color="black", lwd=0.2) +
  # Crop
  coord_sf(ylim=c(-52,68)) +
  # Legend
  scale_fill_manual(name="Comparable?", values=c("darkgreen", "orange", "darkred", "grey90", "white")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_comparable_countries.png"),
       width=6.5, height=2.6, units="in", dpi=600, bg="white")
 



