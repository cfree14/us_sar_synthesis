
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/sars/atlantic/tables"
outdir <- "data/sars/atlantic/processed"

# Species key
species_key <- readxl::read_excel("data/species_key.xlsx")

# TO DO LIST
# Harmonize dolphin stocks
# Add regions and area short names (maybe?)
# Finish converting values to numeric
# Check for missing values
# Handle different revision columns


# Merge data
################################################################################

# Read data
files2merge <- list.files(indir, pattern=".xlsx")
data_orig <- purrr::map_df(files2merge, function(x){
  df <- readxl::read_excel(file.path(indir, x), 
                           col_types = "text",
                           na=c("-", "unk", "undet", "n/a", "N/A", "NA", 
                                "unk for all but 2 stocks", "undet for all but 2 stocks",
                                "unk for all but 3 stocks", "undet for all but 3 stocks",
                                "unk for all but 4 stocks", "undet for all but 4 stocks")) %>% 
    mutate(filename=x)
})

# Format data
data <- data_orig %>% 
  # Add year
  mutate(year = str_split(filename, "_", simplify = TRUE)[, 2] %>% as.numeric(.)) %>% 
  # Format strategic
  mutate(strategic_yn=recode(strategic_yn, 
                             "No" = "N",
                             "Nr" = "N",
                             "Nt" = "N",
                             "N7" = "N",
                             "Y for all"="Y")) %>% 
  # Format species
  mutate(species=gsub("\r\n", " ", species),
         species=gsub("’|‘", "'", species),
         species=stringr::str_squish(species),
         species=gsub("- ", "-", species),
         species=case_when(grepl("short-finned", species) ~ "Short-finned pilot whale",
                           grepl("long-finned", species) ~ "Long-finned pilot whale",
                           grepl("Mesoplodon", species) ~ "Mesoplodont beaked whales",
                           T ~ species),
         species=recode(species, 
                        "Sperm Whale" = "Sperm whale",
                        "Clymene's dolphin" = "Clymene dolphin",
                        "Mellon-headed whale" = "Melon-headed whale",
                        "Gervais beaked whale" = "Gervais' beaked whale",
                        "Northern right whale" = "North Atlantic right whale",
                        "Blaineville's beaked whale" = "Blainville's beaked whale",
                        "Bottlenose dolphin" = "Common bottlenose dolphin")) %>% 
  # Add species info
  rename(comm_name=species) %>% 
  left_join(species_key, by="comm_name") %>% 
  # Format area
  mutate(area=gsub("\r\n", " ", area) %>% stringr::str_squish(.)) %>% 
  # Format N CV
  #mutate(n_cv=gsub(" k", "", n_cv)) %>% 
  # Convert to numeric
  mutate_at(vars(n, n_cv, n_min, r_max, rf, pbr), as.numeric) %>% # n_cv
  # Format Rmax
  # mutate(r_max=recode(r_max,
  #                     "0.02a" = "0.02",
  #                     "0.04a"="0.04") %>% as.numeric(.)) %>%
  # Format N
  # mutate(n=gsub(",", "", n),
  #        n=case_when(grepl("unk", n) ~ "",
  #                    n=="7.6M" ~ "7600000",
  #                    T ~ n),
  #        n=gsub("[^0-9]", "", n) %>% as.numeric(.)) %>% 
  # Extracted revision yr
  mutate(revised_yr=str_extract(revised_yn, "(?<=\\()\\d+(?=\\))") %>% as.numeric()) %>%
  # Clean revised yes/no
  mutate(revised_yn=str_remove_all(revised_yn, "[0-9() ]")) %>% 
  # Fill missing revision year
  mutate(revised_yr=ifelse(is.na(revised_yr) & revised_yn=="Y", year, revised_yr) ) %>% 
  # Add "nothing" to revision notes when no revision occured
  mutate(revised=ifelse(is.na(revised) & revised_yn=="N", "nothing", revised)) %>% 
  # Format strategic (Y/N)
  mutate(strategic_yn=recode(strategic_yn, 
                             "Y"="Strategic",
                             "N"="Non-strategic")) %>% 
  # Remove useless
  # select(-id) %>% 
  # Arrange
  select(filename, year, 
         group, comm_name, species, 
         center, region, area,
         n, n_cv, 
         n_min,
         r_max, rf, pbr, msi_total, 
         #msi_total_cv,
         msi_fisheries, msi_fisheries_cv,
         strategic_yn,
         revised_yn, revised_yr, revised,
         everything())

# N, N_CV, N_MIN, PBR, MSI, MSI_CV, MSI_FISHERIES

# Inspect
str(data)
freeR::complete(data)

# Year
table(data$year)

# Center
table(data$center)

# Strategic (Y/N)
table(data$strategic_yn)

# Revised info
table(data$revised_yn)
table(data$revised_yr)
sort(unique(data$revised))

# Revised
sort(unique(data$revised))

# RF and Rmax
table(data$rf)
table(data$r_max)

# N values
# sort(unique(data$n))
sort(unique(data$n_min))

# Species
spp_key <- data %>% 
  count(comm_name, species)

# cv_key <- data %>% 
#   count(n_cv)


# Fix areas
################################################################################

# Format data
data1 <- data %>% 
  # Format area
  mutate(area=gsub("No\\.", "Northern", area),
         area=gsub(" east", " East", area),
         area=gsub(" coast", " Coast", area),
         area=gsub(" north", " North", area),
         area=gsub(" west", " West", area),
         area=gsub(" continental", " Continental", area),
         area=gsub(" outer", " Outer", area),
         area=gsub(" offshore", " Offshore", area),
         area=gsub(" south", " South", area),
         area=gsub(" central", " Central", area),
         area=gsub(" migratory", " Migratory", area),
         
         area=recode(area,
                     "Gulf of Maine/ Bay of Fundy" = "Gulf of Maine/Bay of Fundy",
                     "Gulf of Maine, Bay of Fundy" = "Gulf of Maine/Bay of Fundy",
                     "Northwest North Atlantic"="Western North Atlantic")) %>%
  # Format dolphin areas
  mutate(area = str_replace(area, "j$", "")) %>% 
  mutate(area=recode(area,
                     "Barataria Bay Estuarine System"  = "Barataria Bay",
                     "Caloosa-hatchee River" = "Caloosahatchee River",                                                   
                     "Caloosahat chee River" = "Caloosahatchee River",
                     "Chocta-whatchee Bay" = "Choctawhatchee Bay",                                                     
                     "Choctawha tchee Bay" = "Choctawhatchee Bay",
                     "Chokolosk ee Bay/Ten Thousand Islands/Gul livan Bay" = "Chokoloskee Bay/Ten Thousand Islands/Gullivan Bay",                     
                     "Chokoloskee Bay, Ten Thousand Islands, Gullivan Bay"= "Chokoloskee Bay/Ten Thousand Islands/Gullivan Bay",                    
                     "Chokoloskee Bay/Ten Thousand Islands/Gullivan Bay" = "Chokoloskee Bay/Ten Thousand Islands/Gullivan Bay",
                     "Copano Bay, Aransas Bay, San Antonio Bay, Redfish Bay, Espiritu Santo Bay" = "Copano Bay/Aransas Bay/San Antonio Bay/Redfish Bay/Espiritu Santo Bay",
                     "Copano Bay/Aransa s Bay/San Antonio Bay/Redfis h Bay/Espirit u Santo Bay" = "Copano Bay/Aransas Bay/San Antonio Bay/Redfish Bay/Espiritu Santo Bay",
                     "Copano Bay/Aransas Bay/San Antonio Bay/Redfish Bay/Espiritu Santo Bay" = "Copano Bay/Aransas Bay/San Antonio Bay/Redfish Bay/Espiritu Santo Bay",
                     "Galveston Bay, East Bay, Trinity Bay" = "Galveston Bay/East Bay/Trinity Bay",
                     
                     # GOM  bay/sound/estuarine
                     "Gulf of Mexico bay, sound, and estuarine" = "Northern GOM Bay, Sound, and Estuary Stocks",                            
                     "Gulf of Mexico bay, sound, and estuarine (32 stocks)" = "Northern GOM Bay, Sound, and Estuary Stocks",
                     "Gulf of Mexico bay, sound, and estuarine (33 stocks)" = "Northern GOM Bay, Sound, and Estuary Stocks",
                     "Gulf of Mexico bay, sound, and estuarine13" = "Northern GOM Bay, Sound, and Estuary Stocks",
                     "Gulf of Mexico bay, sound, and estuary (29 stocks)" = "Northern GOM Bay, Sound, and Estuary Stocks",
                     "Gulf of Mexico, bay, sound and estuary (27 stocks)"  = "Northern GOM Bay, Sound, and Estuary Stocks",                  
                     # GOM shelg
                     "Gulf of Mex. Continental shelf edge and slope" = "Northern GOM Continental Shelf",      
                     "Gulf of Mexico Continental shelf" = "Northern GOM Continental Shelf",                                  
                     "Gulf of Mexico Continental shelf edge and slope" = "Northern GOM Continental Shelf",                     
                     "Gulf of Mexico, Continental shelf" = "Northern GOM Continental Shelf",                                    
                     "Gulf of Mexico, Continental Shelf" = "Northern GOM Continental Shelf",                                    
                     "Gulf of Mexico, Continental shelf edge and slope" = "Northern GOM Continental Shelf",
                     "Northern Gulf of Mexico Continental shelf edge and slope" = "Northern GOM Continental Shelf",
                     "Northern Gulf of Mexico Continental shelf" = "Northern GOM Continental Shelf",  
                     # GOM outer shelf
                     "Gulf of Mexico Outer Continental shelf" = "Northern GOM Outer Continental Shelf", #"Gulf of Mexico Outer Continental Shelf",
                     "Gulf of Mexico, Outer Continental shelf" = "Northern GOM Outer Continental Shelf", # "Gulf of Mexico Outer Continental Shelf",
                     "Northern Gulf of Mexico Outer Continental shelf" = "Northern GOM Outer Continental Shelf",
                     # GOM oceanic
                     "Gulf of Mexico Oceanic" = "Northern GOM Oceanic", 
                     "Gulf of Mexico, Oceanic" = "Northern GOM Oceanic",
                     "Northern Gulf of Mexico Oceanic" = "Northern GOM Oceanic",
                     # GOM eastern
                     "Eastern Gulf of Mexico" = "GOM Eastern Coastal",                                              
                     "Eastern Gulf of Mexico Coastal" = "GOM Eastern Coastal",
                     "Gulf of Mexico, Eastern Coastal" = "GOM Eastern Coastal", 
                     # GOM western
                     "Gulf of Mexico, Western Coastal" = "GOM Western Coastal",
                     "Western Gulf of Mexico" = "GOM Western Coastal",                            
                     "Western Gulf of Mexico Coastal" = "GOM Western Coastal",
                     # GOM northern
                     "Northern Gulf of Mexico" = "GOM Northern Coastal",
                     "Gulf of Mexico, Northern Coastal" = "GOM Northern Coastal",
                     "Northern Gulf of Mexico Coastal" = "GOM Northern Coastal",           
                     "Northern Gulf of Mexico Coastal (3 stocks)" = "GOM Northern Coastal",
                     # Other
                     "Jacksonvill e Estuarine System"  = "Jacksonville Estuarine System",
                     "Matagorda Bay, Tres Palacios Bay, Lavaca Bay" = "Matagorda Bay/Tres Palacios Bay/Lavaca Bay",                      
                     "Matagorda Bay/Tres Palacios Bay/Lavac a Bay" = "Matagorda Bay/Tres Palacios Bay/Lavaca Bay",
                     "Mobile Bay, Bonsecour Bay" = "Mobile Bay/Bonsecour Bay",                                           
                     "Mobile Bay/Bonse cour Bay" = "Mobile Bay/Bonsecour Bay",
                     "Neuces Bay, Corpus Christi Bay" = "Neuces Bay/Corpus Christi Bay",                                    
                     "Neuces Bay/Corpu s Christi Bay" = "Neuces Bay/Corpus Christi Bay",
                     "Northern Georgia, Southern South Carolina Estuarine System" = "Northern Georgia/Southern South Carolina Estuarine System",         
                     "Northern Georgia/ Southern South Carolina Estuarine System" = "Northern Georgia/Southern South Carolina Estuarine System",
                     "Pensacola Bay, East Bay" = "Pensacola Bay/East Bay",
                     "Pine Island Sound, Charlotte Harbor, Gasparilla Sound, Lemon Bay" = "Pine Island Sound/Charlotte Harbor/Gasparilla Sound/Lemon Bay",  
                     "Pine Island Sound/Cha rlotte Harbor/Gas parilla Sound/Lem on Bay" = "Pine Island Sound/Charlotte Harbor/Gasparilla Sound/Lemon Bay",
                     "Puerto Rico and US Virgin Islands" = "Puerto Rico and U.S. Virgin Islands",                                   
                     "Puerto Rico and US Virgin Islands stock" = "Puerto Rico and U.S. Virgin Islands",
                     "Sarasota Bay, Little Sarasota Bay" = "Sarasota Bay/Little Sarasota Bay",
                     "St. Joseph Sound, Clearwater Harbor" = "St. Joseph Sound/Clearwater Harbor",                                
                     "St. Joseph Sound/Clea rwater Harbor" = "St. Joseph Sound/Clearwater Harbor",
                     "St. Vincent Sound, Apalachicola Bay, St. George Sound" = "St. Vincent Sound/Apalachicola Bay/St. George Sound",             
                     "St. Vincent Sound/Apa lachicola Bay/St. George Sound" = "St. Vincent Sound/Apalachicola Bay/St. George Sound", 
                     "Terrebonne Bay/Timba lier Bay" = "Terrebonne Bay/Timbalier Bay",
                     "Terrebonne, Timbalier Bay Estuarine System" = "Terrebonne Bay/Timbalier Bay",
                     "Vermilion Bay, West Cote Blanche Bay, Atchafalaya Bay" = "Vermilion Bay/West Cote Blanche Bay/Atchafalaya Bay",               
                     "Vermilion Bay/West Cote Blanche Bay/Atchaf alaya Bay" = "Vermilion Bay/West Cote Blanche Bay/Atchafalaya Bay",
                     "Waccasass a Bay/Withla coochee Bay/Crysta l Bay" = "Waccasassa Bay/Withlacoochee Bay/Crystal Bay",                    
                     "Waccasassa Bay, Withla-coochee Bay, Crystal Bay" = "Waccasassa Bay/Withlacoochee Bay/Crystal Bay",
                     "Western North Atlantic, S. Carolina, Georgia Coastal" = "WNA South Carolina/Georgia Coastal",              
                     "Western North Atlantic, S. Carolina/G eorgia Coastal" = "WNA South Carolina/Georgia Coastal", 
                     "Western5 North Atlantic, Coastal" = "Western North Atlantic Coastal",
                     "Western North Atlantic, Central Florida Coastal"  = "WNA Central Florida Coastal",   
                     "Western North Atlantic, Coastal" = "Western North Atlantic Coastal",                                       
                     "Western North Atlantic, Coastal, Central Florida" = "WNA Central Florida Coastal",                     
                     "Western North Atlantic, Coastal, Northern Florida" = "WNA Northern Florida Coastal",                    
                     "Western North Atlantic, Coastal, Northern Migratory" = "WNA Northern Migratory Coastal",                 
                     "Western North Atlantic, Coastal, S. Carolina/Georgia" = "WNA South Carolina/Georgia Coastal",                 
                     "Western North Atlantic, Coastal, Southern Migratory" = "WNA Southern Migratory Coastal",                
                     "Western North Atlantic, Northern Florida Coastal" = "WNA Northern Florida Coastal",                     
                     "Western North Atlantic, Northern Migratory Coastal" = "WNA Northern Migratory Coastal",                  
                     "Western North Atlantic, Northern Migratory, Coastal" = "WNA Northern Migratory Coastal",                  
                     "Western North Atlantic, Offshore" = "WNA Offshore",                                      
                     "Western North Atlantic, S. Carolina/Georgia Coastal" = "WNA South Carolina/Georgia Coastal",                 
                     "Western North Atlantic, Southern Migratory Coastal" = "WNA Southern Migratory Coastal")) %>% 
  # Build stock 
  mutate(stock=paste0(comm_name, " (", area, ")")) %>% 
  # Recode stocks
  mutate(stock=recode(stock, 
                      # Humpback
                      "Humpback whale (Western North Atlantic)" = "Humpback whale (Gulf of Maine)",
                      # Sperm whale - nGOM
                      "Sperm whale (Gulf of Mexico Oceanic)" = "Sperm whale (Northern Gulf of Mexico)",                                                                 
                      "Sperm whale (Gulf of Mexico)" = "Sperm whale (Northern Gulf of Mexico)",
                      "Sperm whale (Northern Gulf of Mexico Oceanic)" = "Sperm whale (Northern Gulf of Mexico)",
                      # Sperm whale - NA
                      "Sperm whale (Western North Atlantic)" = "Sperm whale (North Atlantic)",
                      # Sperm whale = PR/USVI
                      "Sperm whale (Puerto Rico and US Virgin Islands stock)" = "Sperm whale (Puerto Rico and U.S. Virgin Islands)",                      
                      "Sperm whale (Puerto Rico and US Virgin Islands)" = "Sperm whale (Puerto Rico and U.S. Virgin Islands)",                                                   
                      # Bryde's whale
                      "Bryde's whale (Gulf of Mexico Oceanic)" = "Bryde's whale (Northern Gulf of Mexico)",                                                             
                      "Bryde's whale (Gulf of Mexico)" = "Bryde's whale (Northern Gulf of Mexico)",                                                                      
                      "Bryde's whale (Northern Gulf of Mexico Oceanic)" = "Bryde's whale (Northern Gulf of Mexico)",
                      # NA right whale
                      "North Atlantic right whale (Western Atlantic)" = "North Atlantic right whale (Western North Atlantic)", 
                      "North Atlantic right whale (Western)" = "North Atlantic right whale (Western North Atlantic)" )) %>% 
  # Recode another way
  mutate(stock=case_when(grepl("Blainville", stock) & grepl("Gulf", stock) ~ "Blainville's beaked whale (Northern Gulf of Mexico)",
                         grepl("Cuvier", stock) & grepl("Gulf", stock) ~ "Cuvier's beaked whale (Northern Gulf of Mexico)",
                         grepl("Cuvier", stock) & grepl("Puerto", stock) ~ "Cuvier's beaked whale (Puerto Rico and U.S. Virgin Islands)",
                         grepl("Dwarf sperm", stock) & grepl("Gulf", stock) ~ "Dwarf sperm whale (Northern Gulf of Mexico)",
                         grepl("False killer", stock) & grepl("Gulf", stock) ~ "False killer whale (Northern Gulf of Mexico)",
                         grepl("Gervais", stock) & grepl("Gulf", stock) ~ "Gervais' beaked whale (Northern Gulf of Mexico)",
                         grepl("Killer whale", stock) & grepl("Gulf", stock) ~ "Killer whale (Northern Gulf of Mexico)",
                         grepl("Melon", stock) & grepl("Gulf", stock) ~ "Melon-headed whale (Northern Gulf of Mexico)",
                         grepl("Pygmy killer", stock) & grepl("Gulf", stock) ~ "Pygmy killer whale (Northern Gulf of Mexico)",
                         grepl("Pygmy sperm", stock) & grepl("Gulf", stock) ~ "Pygmy sperm whale (Northern Gulf of Mexico)",
                         grepl("Short-finned", stock) & grepl("Gulf", stock) ~ "Short-finned pilot whale (Northern Gulf of Mexico)",
                         grepl("Short-finned", stock) & grepl("Puerto", stock) ~ "Short-finned pilot whale (Puerto Rico and U.S. Virgin Islands)",
                         grepl("Clymene", stock) & grepl("Gulf", stock) ~ "Clymene dolphin (Northern Gulf of Mexico)",
                         grepl("Fraser", stock) & grepl("Gulf", stock) ~ "Fraser's dolphin (Northern Gulf of Mexico)",
                         grepl("Pantropical", stock) & grepl("Gulf", stock) ~ "Pantropical spotted dolphin (Northern Gulf of Mexico)",
                         grepl("Risso", stock) & grepl("Gulf", stock) ~ "Risso's dolphin (Northern Gulf of Mexico)",
                         grepl("Rough-toothed", stock) & grepl("Gulf", stock) ~ "Rough-toothed dolphin (Northern Gulf of Mexico)",
                         grepl("Spinner dolphin", stock) & grepl("Gulf", stock) ~ "Spinner dolphin (Northern Gulf of Mexico)",
                         grepl("Spinner dolphin", stock) & grepl("Puerto", stock) ~ "Spinner dolphin (Puerto Rico and U.S. Virgin Islands)",
                         grepl("Striped dolphin", stock) & grepl("Gulf", stock) ~ "Striped dolphin (Northern Gulf of Mexico)",
                         grepl("Atlantic spotted dolphin", stock) & grepl("Gulf", stock) ~ "Atlantic spotted dolphin (Northern Gulf of Mexico)",
                         grepl("Atlantic spotted dolphin", stock) & grepl("Puerto", stock) ~ "Atlantic spotted dolphin (Puerto Rico and U.S. Virgin Islands)",
                         T ~ stock)) %>% 
  # Arrange
  select(filename, year, stock,
         group, comm_name, species, 
         center, region, area,
         n, n_cv, n_min,
         r_max, rf, pbr, msi_total, msi_total_cv, msi_fisheries, strategic_yn,
         revised, 
         everything())

# Confirm only one value per 1 year per stock
data1 %>% count(stock, year) %>% 
  filter(n>1)

# Inspect dolphins
ggplot(data1 %>% filter(group=="Dolphins" & grepl("Common", comm_name)), aes(y=area, x=year, fill=strategic_yn)) +
  geom_tile() +
  theme_bw()

# Large whales, Small whales, Porpoises, Phocids, Dolphins
ggplot(data1 %>% filter(group=="Phocids"), aes(y=stock, x=year, fill=strategic_yn)) +
  geom_tile() +
  theme_bw()

sort(unique(data1$stock))
table(data1$group)

data1 %>% 
  filter(comm_name=="Common bottlenose dolphin" & grepl("Gulf", area)) %>% 
  pull(area) %>% unique() %>% sort()


# Export data
################################################################################

# Export data
saveRDS(data1, file=file.path(outdir, "Atlantic_SARs_parameters.Rds"))



