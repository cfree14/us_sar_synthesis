
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/Whales/us_sar_synthesis_data/atlantic/tables_ngom_bse_bottlenose_dolphins"
keydir <- "data/sars/keys"
outdir <- "data/sars/processed"

# Read NGOM dolphin
data_ngom <- readRDS(file.path(outdir, "Atlantic_NGOM_bdolphin_SARs_parameters.Rds"))
data_orig <- readRDS(file.path(outdir, "Atlantic_SARs_parameters.Rds"))

colnames(data_ngom)
colnames(data_orig)


# Merge
################################################################################

# Remove composite NGOM bottlenose dolphin stocks
data1 <- data_orig %>% 
  filter(!grepl("Stocks", stock)) %>% 
  # Format a few stock areas for merge
  mutate(area=gsub(", ", "/", area)) %>% 
  mutate(area=recode(area, 
                     "Mississippi Sound/Lake Borgne/Bay Boudreau" = "Bay Boudreau/Mississippi Sound/Lake Borgne",
                     "Neuces Bay/Corpus Christi Bay" = "Nueces Bay/Corpus Christi Bay",
                     "Florida Keys (Bahia Honda to Key West)" = "Florida Keys (Bahia Honda Key to Key West)"))
  

freeR::uniq(data_ngom$area)

# Format NGOM stocks
data_ngom1 <- data_ngom %>% 
  # Format areas to match full dataset
  mutate(area=recode(area,
                     "Bay Boundreau, Mississippi Sound" = "Bay Boudreau, Mississippi Sound, Lake Borgne",  
                     "St. John's Sound, Clearwater Harbor" = "St. Joseph Sound, Clearwater Harbor",
                     "Barataria Bay" = "Barataria Bay Estuarine System",
                     "Calcasieu lake" = "Calcasieu Lake",
                     "Compano Bay, Aransas Bay, San Antonio Bay, Redfish Bay, Espirito Santo Bay" = "Copano Bay, Aransas Bay, San Antonio Bay, Redfish Bay, Espiritu Santo Bay",
                     "Pine Sound, Charlotte Harbor, Gasparilla Sound" = "Pine Island Sound, Charlotte Harbor, Gasparilla Sound",                      
                     "Pine Sound, Charlotte Harbor, Gasparilla Sound, Lemon Bay" = "Pine Island Sound, Charlotte Harbor, Gasparilla Sound, Lemon Bay", 
                     "Matagodra Bay, Tres Palacios Bay, Lavaca Bay" = "Matagorda Bay, Tres Palacios Bay, Lavaca Bay",  
                     "Pensacula Bay, East Bay" = "Pensacola Bay, East Bay",
                     "Sabine lake"="Sabine Lake",
                     "St. Vincent Sound, Apalachicola Bay, St. Georges Sound" = "St. Vincent Sound, Apalachicola Bay, St. George Sound",  
                     "Florida Key (Bahia Honda Key to Key West)" = "Florida Keys (Bahia Honda Key to Key West)",
                     "Vermillion Bay, West Cote Blanche Bay, Atchafalaya Bay" = "Vermilion Bay, West Cote Blanche Bay, Atchafalaya Bay",
                     "TerreBonne Bay, Timbalier Bay"="Terrebonne-Timbalier Bay Estuarine System")) %>% 
  mutate(area=gsub(", ", "/", area))


# Merge
data <- bind_rows(data1, data_ngom1) %>% 
  # Remake stock name
  mutate(stock=paste0(comm_name, "(", area, ")")) %>% 
  # Arrange
  arrange(group, comm_name, area, year)



# Inspect dolphin stocks

ggplot(data %>% filter(comm_name=="Common bottlenose dolphin" & region=="Gulf of Mexico"), aes(x=year, y=area, fill=revised_yn)) +
  facet_wrap(~region, scales="free_y", space="free_y") +
  geom_tile() +
  # Legend
  scale_fill_manual(name="Revised?", values=c("grey70", "darkred")) +
  # Theme
  theme_bw()




