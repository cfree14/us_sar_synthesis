
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
data_orig <- readxl::read_excel(file.path(indir, "database_final_version.xlsx"))

# Species orig
spp_orig <- readxl::read_excel(file.path(indir, "database_final_version.xlsx"), sheet=2)


# Format data
################################################################################

# Read fishery key
fishery_key <- readxl::read_excel("data/sars/keys/fishery_key_raw.xlsx")

# Handle XXX (XXX AK) thing
# Assign real region?

# Format data
data <- data_orig %>% 
  # Format region
  rename(region_orig=region) %>% 
  mutate(region_orig=recode(region_orig,
                            "Atlantic ocean, Gulf of Mexico, and Caribbean"="Atlantic",
                            "High Seas"="High seas")) %>% 
  # Add region
  left_join(fishery_key %>% select(region, fishery)) %>% 
  # Format n vessels
  rename(nvessels_orig=nvessels) %>% 
  mutate(nvessels_orig=gsub("< ", "<", nvessels_orig),
         nvessels_orig=gsub("> ", ">", nvessels_orig),
         nvessels_orig=gsub("fewer than |less than |Less than ", "<", nvessels_orig),
         nvessels_orig=ifelse(nvessels_orig %in% c("N/A", "unknown", "Unknown", "None recorded"), NA, nvessels_orig)) %>% 
  # Convert to number
  mutate(nvessels=gsub("<|>", "", nvessels_orig) %>% as.numeric(.)) %>% 
  # Recode fishery type
  mutate(fishery_type=stringr::str_squish(fishery_type), 
         fishery_type=gsub(" fisheries", "", fishery_type),
         fishery_type=recode(fishery_type,
                             "commercial passenger fishing vessel (Charter Boat)"="Charter boat",
                             "dive, handline/mechanical collection"="dive, hand/mechanical collection",
                             "haul seine"="haul/beach seine")) %>% 
  # Arrange
  select(year, region_orig, region, category, fishery_type, fishery,
         nvessels_orig, nvessels, everything())

# Inspect
str(data)
freeR::complete(data)

# Regions
table(data$region_orig)

# Fishery key
fishery_key_orig <- data %>% 
  count(region_orig, fishery)
write.csv(fishery_key_orig, file=file.path("data/sars/keys/fishery_key_raw.csv"), row.names=F)

# Fishery types
sort(unique(data$fishery_type))

# Number of vessels
sort(unique(data$nvessels_orig))

# Export
saveRDS(data, file=file.path(outdir, "1995_2024_list_of_fisheries.Rds"))


# Format species
################################################################################

# TO-DO LIST
# Harmonize common names against data (remove dangling periods)
# Harmonize areas against data (remove dangling periods)
# Merge into a new stock name
# Make fishery match other dataset
# Add in category of fishery
# Assign real region?
# Plot number of stocks over time

# Format species
spp <- spp_orig %>% 
  # Reduce
  select(year:species) %>% 
  # Split stock
  rename(stock=species) %>% 
  separate(stock, into=c("comm_name", "area"), sep=",", remove=F) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Format area
  mutate(area=stringr::str_squish(area)) %>% 
  # Format region
  mutate(region=recode(region, 
                       "Atlantic ocean, Gulf of Mexico, and Caribbean"="Atlantic"))

# Common names
freeR::uniq(spp$comm_name)

# Areas
freeR::uniq(spp$area)

# Export
saveRDS(spp, file=file.path(outdir, "1995_2024_list_of_fisheries_stocks.Rds"))


# Summarize data
################################################################################

# N fisheries over time
nfisheries <- data %>% 
  count(year, category)

# N vessels over time
nvessels <- data %>% 
  group_by(year, category) %>% 
  summarize(nvessels=sum(nvessels, na.rm=T)) %>% 
  ungroup()

# N stocks over time
nstocks <- spp %>% 
  count(year, region) 


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.title=element_blank(),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Number of fisheries listed over time
g1 <- ggplot(nfisheries, aes(x=year, y=n, color=as.character(category))) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of fisheries", tag="A", color="Category") +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = "none")
g1

# Number of fisheries listed over time
g2 <- ggplot(nvessels, aes(x=year, y=nvessels/1000, color=as.character(category))) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of vessels (1000s)", tag="B", color="Category") +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = c(0.8, 0.8),
        legend.key.size = unit(0.3, "cm"))
g2

# Number of stocks
g3 <- ggplot(nstocks, aes(x=year, y=n, color=region)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of stocks", tag="C", color="Region") +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = c(0.25, 0.8),
        legend.key.size = unit(0.3, "cm"))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "FigX_list_of_fisheries.png"), 
       width=6.5, height=2.5, units="in", dpi=600, bg="white")




