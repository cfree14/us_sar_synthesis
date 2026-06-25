
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/us_sar_synthesis_data/atlantic/tables"
keydir <- "data/sars/keys"
outdir <- "data/sars/processed"

# Species key
species_key <- readxl::read_excel("data/species_key.xlsx")

# TO DO LIST
# Finish converting values to numeric
#   -- Convert SIM total to numeric
#   -- Convert SIM fisheries to numeric
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
                                "unk for all but 4 stocks", "undet for all but 4 stocks",
                                "unk for all but 6 stocks", "undet for all but 6 stocks")) %>% 
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
                        "Bottlenose dolphin" = "Common bottlenose dolphin",
                        "Short-beaked common dolphin" = "Common dolphin")) %>% 
  # Add species info
  rename(comm_name=species) %>% 
  left_join(species_key, by="comm_name") %>% 
  # Format area
  mutate(area=gsub("\r\n", " ", area) %>% stringr::str_squish(.)) %>% 
  # Convert to numeric
  mutate_at(vars(n, n_cv, n_min, r_max, rf, pbr, msi_fisheries_cv), as.numeric) %>% 
  # Format revised (y/n) before extracting year
  mutate(revised_yn=stringr::str_squish(revised_yn),
         revised_yn=recode(revised_yn,
                           "N (2011" = "N (2011)", 
                           "N 2007" = "N (2007)")) %>% 
  # Extracted revision yr
  mutate(revised_yr1=str_extract(revised_yn, "(?<=\\()\\d+(?=\\))") %>% as.numeric()) %>%
  # Merge provided and extracted revision year
  mutate(revised_yr=ifelse(!is.na(revised_yr), revised_yr, revised_yr1)) %>% 
  select(-revised_yr1) %>% 
  # Clean revised yes/no
  mutate(revised_yn=str_remove_all(revised_yn, "[0-9() ]")) %>%
  # Fill missing revision year for years with revised==yes
  mutate(revised_yr=ifelse(is.na(revised_yr) & revised_yn=="Y", year, revised_yr) ) %>%
  # Add "nothing" to revision notes when no revision occured
  mutate(revised=ifelse(is.na(revised) & revised_yn=="N", "nothing", revised)) %>% 
  # Format revised
  mutate(revised=recode(revised,
                        "p, m" = "m, p",
                        "strandings"="stranding data")) %>% 
  # Format strategic (Y/N)
  mutate(strategic_yn=recode(strategic_yn, 
                             "Y"="Strategic",
                             "N"="Non-strategic")) %>% 
  # Format MSI total
  # rename(msi_total_orig=msi_total) %>% 
  # mutate(msi_total = ifelse(grepl("-", msi_total_orig),
  #                             sub(".*-", "", msi_total_orig),
  #                             msi_total_orig) %>% as.numeric(.)) 
  # Remove useless
  select(-c(id, region)) %>% 
  # Arrange
  select(filename, year, #id, 
         group, comm_name, species, 
         center, area,
         n, n_cv, 
         n_min,
         r_max, rf, pbr, msi_total, 
         msi_fisheries, msi_fisheries_cv,
         strategic_yn,
         revised_yn, revised_yr, revised,
         everything())

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
sort(unique(data$revised_yn))
table(data$revised_yr)
sort(unique(data$revised))

# RF and Rmax
range(data$rf, na.rm=T) # Can RF really bye 0.05?
range(data$r_max, na.rm=T) # Rmax's should not be zero
table(data$rf)
table(data$r_max)

# N values
sort(unique(data$n))
sort(unique(data$n_min))

# Species
spp_key <- data %>% 
  count(comm_name, species)


# Area key
################################################################################


# Build area key
area_key <- data %>% 
  count(comm_name, area)
write.csv(area_key, file.path(keydir, "area_key_atlantic_raw.csv"), row.names=F)

# Read area key
area_key_use <- readxl::read_excel(file.path(keydir, "area_key_atlantic_final.xlsx"))

# Fix areas
################################################################################

# Add area
data1 <- data %>% 
  # Add area
  rename(area_orig=area) %>% 
  left_join(area_key_use, by=c("comm_name", "area_orig")) %>% 
  # Build stock
  mutate(stock=paste0(comm_name, " (", area, ")")) %>% 
  # Arrange
  select(filename, year,  
         group, stock, comm_name, species, 
         center, region, area, area_orig,
         n, n_cv, 
         n_min,
         r_max, rf, pbr, msi_total, 
         msi_fisheries, msi_fisheries_cv,
         strategic_yn,
         revised_yn, revised_yr, revised,
         everything())

# Inspect
freeR::complete(data1)

# Confirm only one value per 1 year per stock - FIX THIS
data1 %>% count(comm_name, area, year) %>% 
  filter(n>1)

# Check non-dolphins
ggplot(data1 %>% filter(group!="Dolphins"), aes(y=stock, x=year, fill=strategic_yn)) +
  facet_wrap(~group, scales="free_y", space="free_y") +
  geom_tile() +
  theme_bw()

# Check GOM dolphins
ggplot(data1 %>% filter(group=="Dolphins" & region=="Gulf of Mexico"), aes(y=area, x=year, fill=strategic_yn)) +
  facet_wrap(~comm_name, scales="free_y", space="free_y") +
  geom_tile() +
  theme_bw()

# Check ATL dolphins
ggplot(data1 %>% filter(group=="Dolphins" & region=="Atlantic"), aes(y=area, x=year, fill=strategic_yn)) +
  facet_wrap(~comm_name, scales="free_y", space="free_y") +
  geom_tile() +
  theme_bw()


# Export data
################################################################################

# Export data
saveRDS(data1, file=file.path(outdir, "Atlantic_SARs_parameters.Rds"))



