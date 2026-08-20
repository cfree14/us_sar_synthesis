
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

# Species orig
spp_orig <- readxl::read_excel(file.path(indir, "database_final_version.xlsx"), sheet=2)

# Read taxa key (reference key from SARS database)
taxa_key <- readxl::read_excel("data/species_key.xlsx")

# Read LOF data
data <- readRDS(file=file.path(outdir, "1995_2024_list_of_fisheries.Rds"))



# Format species
################################################################################

# TO-DO LIST
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
  # FORMAT COMMON NAME
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name) %>% stringr::str_squish(.)) %>% 
  # Strip dangling period from end of common name
  mutate(comm_name = str_remove(comm_name, "\\.$")) %>% 
  # Replace ’
  mutate(comm_name=gsub("’", "'", comm_name)) %>% 
  # Capitalize some
  mutate(comm_name=gsub("pacific", "Pacific", comm_name)) %>% 
  mutate(comm_name=gsub("atlantic", "Atlantic", comm_name)) %>% 
  mutate(comm_name=gsub("indian", "Indian", comm_name)) %>%
  # Remove plurals
  mutate(comm_name=gsub("whales", "whale", comm_name)) %>%
  mutate(comm_name=gsub("seals", "seal", comm_name)) %>%
  mutate(comm_name=gsub("dolphins", "dolphin", comm_name)) %>%
  # Fix no info, no recent, Pacific white-sided dolphin, Mesoplodont beaked whales
  mutate(comm_name=case_when(grepl("No information|Undetermined", comm_name) ~ "No information",
                             grepl("None documented", comm_name) ~ "None documented in the most recent 5 years",
                             grepl("sided dolphin", comm_name) ~ "Pacific white-sided dolphin",
                             grepl("arbor seal", comm_name) ~ "Harbor seal",
                             grepl("Mesoplo", comm_name) ~ "Mesoplodont beaked whales", 
                             grepl("Kogia", comm_name) ~ "Pygmy/dwarf sperm whale", 
                             grepl("vessel", tolower(comm_name)) ~ NA, 
                             T ~ comm_name)) %>% 
  # Fix sea otter
  mutate(comm_name=case_when(comm_name == "Sea otter" & grepl("CA", area) ~ "Southern sea otter",
                             comm_name == "Sea otter" & grepl("AK", area) ~ "Northern sea otter", 
                             T ~ comm_name)) %>% 
  # Recode specific ones
  mutate(comm_name=recode(comm_name, 
                          "Ak northern elephant seal" = "Northern elephant seal",                 
                          # "Antarctic fur seal"                        
                          "Beluga" = "Beluga whale",                                    
                          "Bottlenose dolphin" = "Common bottlenose dolphin",                        
                          "Dail's porpoise" = "Dall's porpoise",                            
                          "Florida manatee" = "West Indian manatee",                           
                          "Gervais beaked whale" = "Gervais' beaked whale",                      
                          # "Ginkgo-toothed beaked whale"               
                          "Grey whale" = "Gray whale",                                 
                          "Indo-Pacific dolphin" = "Indo-Pacific bottlenose dolphin",                     
                          # "Ing sea transient"                          
                          "Long-beaked common dolphin ca/or/wa"  = "Long-beaked common dolphin",      
                          "Monk seal" = "Hawaiian monk seal",                                
                          # "No information"                            
                          # "None documented in the most recent 5 years" 
                          # "North Pacific"                             
                          "Northern (ak) sea otter"  = "Northern sea otter",                   
                          "Northern (alaska) sea otter"  = "Northern sea otter",              
                          "Northern right-whale dolphin" = "Northern right whale dolphin",             
                          "Pelagic striped doiphin" = "Striped dolphin",                 
                          # "Pilot whale"                                
                          # "Pygmy/dwarf sperm whale"                   
                          # "Sea otter" = "",                                  
                          "Short-beaked common dolphin ca/or/wa" = "Short-beaked common dolphin",   
                          "Shortfinned pilot whale" = "Short-finned pilot whale",                  
                          "Spotted dolphin" = "Atlantic spotted dolphin",                         
                          "Steller sea tion" = "Steller sea lion",                         
                          # "Undetermined"                              
                          "Walrus" = "Pacific walrus" )) %>% 
  # ADD SCI NAME
  left_join(taxa_key) %>% 
  # FORMAT AREA
  mutate(area=stringr::str_squish(area)) %>% 
  # Strip dangling period from end of common name
  mutate(area = str_remove(area, "\\.$")) %>% 
  # Uppercase first letter
  mutate(area = str_replace(area, "^.", toupper)) %>% 
  # Format region
  mutate(region=recode(region, 
                       "Atlantic ocean, Gulf of Mexico, and Caribbean"="Atlantic")) %>% 
  # Arrange
  select(year, region, fishery, stock, 
         group, comm_name, species, area, everything()) %>% 
  # Remove duplicates
  unique()

# Inspect
freeR::complete(spp)

# Common names
cnames <- freeR::uniq(spp$comm_name)
cnames[!cnames %in% taxa_key$comm_name]

# Areas
freeR::uniq(spp$area)


# Add category
################################################################################

# Add cateogry
spp2 <- spp %>%
  left_join(
    data %>% select(year, fishery, category),
    by = "year",
    suffix = c("", "_data"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    fishery_dist = stringdist::stringdist(fishery, fishery_data)
  ) %>%
  group_by(across(all_of(names(spp)))) %>%
  slice_min(fishery_dist, n = 1, with_ties = FALSE) %>%
  ungroup()



# Export
################################################################################

# Export
saveRDS(spp2, file=file.path(outdir, "1995_2024_list_of_fisheries_stocks.Rds"))


