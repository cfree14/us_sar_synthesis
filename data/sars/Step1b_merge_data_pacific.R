
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/sars/pacific/tables"
outdir <- "data/sars/pacific/processed"

# Species key
species_key <- readxl::read_excel("data/species_key.xlsx")

# Area key
area_key <- readxl::read_excel("data/area_key.xlsx")

# Stock key
stock_key <- readxl::read_excel("data/stock_key_pacific.xlsx")
freeR::which_duplicated(stock_key$stock) # must have no duplicates

# To do list
# - Check for missing values in data_orig; fill in true NAs with N/A; expand workflow to confirm that only true NAs are present
# - Record the SIM modifiers
# - Derive non-fish SIM
# - Mark revision years
# - Extract survey years

# Checks 
# 1) Are PBR calculations aligned?
# 2) Is total SIM larger than fisheries SIM?


# Step 1. Merge
################################################################################

# Loop through files
files2merge <- list.files(indir, pattern=".xlsx")
data_orig <- purrr::map_df(files2merge, function(x){
  df <- readxl::read_excel(file.path(indir, x), na=c("N/A", "n/a", "unk", "undet", "und"), col_types = "text") %>% 
    mutate(filename=x)
})

# Inspect
# YOU WERE PROGRAMMING A WAY OF LOOKING AT THE NUMBER OF MISSING VALUES IN EACH FILE
years <- sort(unique(data_orig$filename))
freeR::complete(data_orig)
x <- years[1]
stats <- purrr::map_df(years, function(x){
  sdata <- data_orig %>% 
    filter(filename==x) 
  df <- freeR::complete(sdata)
})


# Step 2. Basic cleaning
################################################################################

# Format
data1 <- data_orig %>% 
  # Rename
  rename(comm_name=species, 
         sim_tot_orig=sim_total,
         sim_fish_orig=sim_fisheries) %>% 
  # Add year
  mutate(year = str_split(filename, "_", simplify = TRUE)[, 2] %>% as.numeric(.)) %>% 
  # Convert to numeric
  mutate_at(vars(n_est, n_cv, n_min, r_max, rf, pbr,
                 survey1, survey2, survey3, revision_yr), .funs=as.numeric) %>% 
  # Fix strategic (yes/no) 
  mutate(strategic_yn=recode(strategic_yn,
                             "Y"="Strategic",
                             "N"="Non-strategic",
                             "S"="Strategic")) %>% 
  # Split species/stock into common name and area (for ones with that format)
  separate(species_stock, into=c("comm_name1", "area1"), sep=" \\(", remove=F) %>% 
  mutate(area1=gsub("\\)", "", area1), 
         comm_name=ifelse(is.na(comm_name), comm_name1, comm_name), 
         area=ifelse(is.na(area), area1, area)) %>% 
  select(-c(comm_name1, area1, species_stock)) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=gsub("’", "'", comm_name), 
         comm_name=recode(comm_name, 
                          "Monk seal"="Hawaiian monk seal",
                          "Indopacetus pacificus" = "Longman's beaked whale",
                          "Bottlenose dolphin" = "Common bottlenose dolphin",
                          "Common dolphin, long-beaked"="Long-beaked common dolphin",
                          "Common dolphin, short-beaked"="Short-beaked common dolphin",
                          "Northern right-whale dolphin"="Northern right whale dolphin",
                          "Pilot whale, short-finned"="Short-finned pilot whale")) %>% 
  # Format otters
  mutate(comm_name=case_when(comm_name=="Sea otter" & area=="Washington" ~ "Northern sea otter",
                             comm_name=="Sea otter" & area=="Southern" ~ "Southern sea otter",
                             comm_name=="Sea otter" & area=="Northern (Washington)" ~ "Northern sea otter",
                             comm_name=="Sea otter" & area=="Southern (California)" ~ "Southern sea otter",
                             T ~ comm_name)) %>% 
  # Add species and group
  left_join(species_key, by=c("comm_name")) %>% 
  # Format SIM total
  mutate(sim_tot_orig=gsub(" ", "", sim_tot_orig), 
         sim_tot=gsub("|<|>|≥|.*-", "", sim_tot_orig) %>% as.numeric()) %>% 
  # Format SIM fisheries
  mutate(sim_fish_orig=gsub(" ", "", sim_fish_orig), 
         sim_fish=gsub("|<|>|≥|.*-", "", sim_fish_orig) %>% as.numeric()) %>% 
  # Derive PBR
  mutate(pbr_derived=n_min*(r_max/2)*rf,
         pbr_check=round(pbr_derived - pbr, 0)) %>%
  # Format revised (yes/no)
  mutate(revised_yn=ifelse(revised_yn=="yes" & !is.na(revised_yn), "Revised", "Same as previous")) %>% 
  # Arrange
  select(-region) %>% 
  select(filename, year, 
         group, comm_name, species, 
         area, center, 
         n_est, n_cv, n_min, r_max, rf, 
         pbr, pbr_derived, pbr_check,
         sim_tot_orig, sim_tot,
         sim_fish_orig, sim_fish,
         strategic_yn,
         survey1, survey2, survey3, revision_yr, revised_yn, notes,
         everything())

# Inspect
str(data1)
freeR::complete(data1)

# Number of revised stocks per year
revision_stats <- data1 %>% 
  group_by(year) %>% 
  summarize(nrevised=sum(revised_yn=="yes" & !is.na(revised_yn))) %>% 
  ungroup()

# Center
table(data1$year)
table(data1$center)
table(data1$strategic_yn)

# Species key
spp_key <- data1 %>% 
  count(comm_name, species)

table(data1$sim_tot)


# Step 3. Fix areas and stocks ids
################################################################################

# Format
data2 <- data1 %>% 
  # Clean up punctuation
  mutate(area=gsub("’|ʻ","'", area),
         area=gsub("-","-", area),
         area=gsub(" - ", "-", area),
         area=gsub(" ‚Äì ", "-", area),
         area=gsub(" & ", "/", area)) %>% 
  # Remove parenthetical comments
  mutate(area=gsub(" \\(new stock\\)", "", area),
         area=gsub(" \\(new report\\)", "", area)) %>% 
  # Format some capitalization
  mutate(area=gsub("coast", "Coast", area),
         area=gsub("breeding", "Breeding", area),
         area=gsub("inland", "Inland", area),
         area=gsub("offshore", "Offshore", area),
         area=gsub("waters", "Waters", area)) %>% 
  # Harmonize some others
  mutate(area=gsub("Oahu", "O'ahu", area)) %>% 
  # Clean up Kaua'i / Ni'ihau
  mutate(area=recode(area,
                     "Kaua'I and Ni'ihau" = "Kaua'i / Ni'ihau",
                     "Kaua'I / Ni'ihau" = "Kaua'i / Ni'ihau",
                     "Kaua'i / Ni'ihau" = "Kaua'i / Ni'ihau",
                     "Kaua'i and Ni'ihau" = "Kaua'i / Ni'ihau",
                     "KauaI / Niihau" = "Kaua'i / Ni'ihau",
                     "Kauai and Niihau" = "Kaua'i / Ni'ihau")) %>% 
  # Expand abbreviations
  mutate(area=gsub(" N ", " North ", area)) %>% 
  # Shorten abbreviations
  mutate(area=gsub("California", "CA", area),
         area=gsub("Oregon", "OR", area),
         area=gsub("Washington", "WA", area),
         area=gsub("Hawai'i|Hawaii|Hawaiian", "HI", area),
         area=gsub("Eastern Tropical Pacific", "ETP", area),
         area=gsub("Central North Pacific", "CNP", area),
         area=gsub("Eastern North Pacific", "ENP", area),
         area=gsub("United States", "U.S.", area)) %>% 
  # Build stock id
  mutate(stock=paste0(comm_name, " (", area, ")")) %>% 
  # Format stock ids
  mutate(stock=recode(stock, 
                      # Hawaii Pelagic
                      "Blainville's beaked whale (HI Pelagic)"  = "Blainville's beaked whale (HI)",
                      "Cuvier's beaked whale (HI Pelagic)" = "Cuvier's beaked whale (HI)",
                      "Striped dolphin (HI Pelagic)" = "Striped dolphin (HI)",
                      # Bottlenose dolphin
                      "Common bottlenose dolphin (4 Islands Region)" = "Common bottlenose dolphin (Maui Nui)", 
                      # Pantropical spotted dolphin
                      "Pantropical spotted dolphin (4 Islands Region)" = "Pantropical spotted dolphin (Maui Nui)",
                      # Long-beaked dolphin
                      "Long-beaked common dolphin (CA/OR/WA)" = "Long-beaked common dolphin (CA)",
                      # Pacific white-sided dolphin
                      "Pacific white-sided dolphin (CA/OR/WA)" = "Pacific white-sided dolphin (CA/OR/WA-N/S)",
                      # Spinner dolphin
                      # "Spinner dolphin (HI)" = "Spinner dolphin (HI Island)",                       
                      "Spinner dolphin (Kure / Midway)" = "Spinner dolphin (Midway / Kure)",         
                      "Spinner dolphin (O'ahu / 4 Islands Region)" = "Spinner dolphin (O'ahu / 4 Islands)",
                       # Sei whale
                      "Sei whale (CA/OR/WA)" = "Sei whale (ENP)",
                      # Blue whale
                      "Blue whale (HI)" = "Blue whale (CNP)",
                      "Blue whale (CA/Mexico)" = "Blue whale (ENP)",
                      # Bryde's whale
                      "Bryde's whale (CA/OR/WA)" = "Bryde's whale (ETP)",
                      # Humpback whale
                      "Humpback whale (CA/OR/Mexico)" = "Humpback whale (CA/OR/WA)",                              
                      "Humpback whale (CA/OR/WA-Mexico)" = "Humpback whale (CA/OR/WA)",                             
                      "Humpback whale (Central America / Southern Mexico – CA/OR/WA)" = "Humpback whale (Central America / Southern Mexico)",
                      "Humpback whale (ENP)" = "Humpback whale (CA/OR/WA)",                                        
                      "Humpback whale (MaInland Mexico – CA/OR/WA)" = "Humpback whale (Mainland Mexico)",
                      # False killer whales
                      "False killer whale (HI Insular)"= "False killer whale (MHI Insular)",                             
                      "False killer whale (HI)" = "False killer whale (HI Pelagic)",                                     
                      "False killer whale (Main HI Islands Insular)" = "False killer whale (MHI Insular)",                 
                      "False killer whale (Northwest HI Islands)" = "False killer whale (NW HI)",                   
                      "False killer whale (Northwestern HI Islands)" = "False killer whale (NW HI)",                 
                      "False killer whale (NW HI Islands)" = "False killer whale (NW HI)",                           
                      "False killer whale (Palmyra)" = "False killer whale (Palmyra Atoll)", 
                      # Melon-headed whales
                      "Melon-headed whale (HI)" = "Melon-headed whale (HI Stock)",
                      # Killer whales
                      "Killer whale (Southern Resident Stock)" = "Killer whale (ENP Southern Resident)",  
                      # Harbor porpoise
                      "Harbor porpoise (San Francisco -Russian River)"  = "Harbor porpoise (San Francisco-Russian River)",             
                      "Harbor porpoise (San Francisco – Russian River)" = "Harbor porpoise (San Francisco-Russian River)",
                      "Harbor porpoise (Northern CA)" = "Harbor porpoise (Northern CA / Southern OR)",
                      "Harbor porpoise (Northern CA/Southern OR)" = "Harbor porpoise (Northern CA / Southern OR)",
                      "Harbor porpoise (Northern OR/WA Coast)" = "Harbor porpoise (Northern OR / WA Coast)",
                      "Harbor porpoise (OR/WA Coast)" = "Harbor porpoise (Northern OR / WA Coast)",
                      "Harbor porpoise (Inland WA)" = "Harbor porpoise (WA Inland Waters)",
                      # Harbor seals
                      "Harbor seal (Inland WA)" = "Harbor seal (WA Inland Waters)",                                       
                      "Harbor seal (OR/WA Coast)" = "Harbor seal (OR/WA Coastal)",                                    
                      "Harbor seal (WA Northern Inland Waters)"  = "Harbor seal (WA Inland Waters)",
                      # Fur seals
                      "Guadalupe fur seal (Mexico)" = "Guadalupe fur seal (US/Mexico)",
                      "Guadalupe fur seal (Mexico to CA)"="Guadalupe fur seal (US/Mexico)",
                      # Sea otters
                      "Northern sea otter (Northern (WA))" = "Northern sea otter (WA)",                           
                      "Southern sea otter (Southern (CA))" = "Southern sea otter (CA)",                           
                      "Southern sea otter (Southern)"  = "Southern sea otter (CA)",                                
                      "Northern sea otter (WA)" = "Northern sea otter (WA)")) %>% 
  # Add region
  left_join(stock_key %>% select(stock, region), by="stock") %>% 
  # Arrange
  select(filename, year, 
         group, 
         stock, comm_name, species, 
         region, area, center, 
         n_est, n_cv, n_min, r_max, rf, 
         pbr, pbr_derived, pbr_check,
         sim_tot_orig, sim_tot,
         sim_fish_orig, sim_fish,
         strategic_yn,
         survey1, survey2, survey3, revised_yn, notes,
         everything())

# Inspect
str(data2)
freeR::complete(data2)

# Which stocks aren't in stock key
data2$stock[!data2$stock %in% stock_key$stock] %>% unique() %>% sort()

# Make sure that there is only 1 value per stock (comm-name-area) and year
data2 %>% 
  count(stock, year) %>% 
  filter(n!=1)


# Step 3. Expand 1999 stocks
################################################################################

# Build missing 1999 stocks
data99 <- data2 %>% 
  filter(year==1999)
data98 <- data2 %>% 
  filter(year==1998)
data99_not_updated <- data98 %>% 
  # Reduce to stocks excluded from 1999 table
  filter(!stock %in% data99$stock) %>% 
  # Update to reflect 1999
  mutate(year=1999,
         filename="1999 SAR (not included in 1999 SAR table)", 
         revised_yn="Same as previous")

# Add missing 1999 stocks to data
data3 <- bind_rows(data2, data99_not_updated) %>% 
  arrange(year, region, group)


# Check 
################################################################################


#
ggplot(data3 %>% filter(group=="Porpoises"), # Phocids, Otariids, Porpoises, Small whales, Large whales, Dolphins
       aes(y=stock, x=year, fill=strategic_yn)) +
  facet_grid(group+region~., scale="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Export
################################################################################

# Export data
saveRDS(data3, file=file.path(outdir, "Pacific_SARs_parameters.Rds"))

