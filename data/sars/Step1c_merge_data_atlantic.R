
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/Whales/us_sar_synthesis_data/atlantic/tables"
keydir <- "data/sars/keys"
outdir <- "data/sars/processed"

# Species key
species_key <- readxl::read_excel("data/species_key.xlsx")


# Helper functions
################################################################################

# Function to calculate average of range
avg_range <- function(x) {
  x <- gsub(",", "", x)
  mean(as.numeric(strsplit(x, "-")[[1]]))
}
avg_range("167,000-188,000")

# Function to get max of range
max_range <- function(x) {
  x <- gsub(",", "", x)
  max(as.numeric(strsplit(x, "-")[[1]]))
}
max_range("167,000-188,000")


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
    # Add filenmae
    mutate(filename=x) |> 
    # Fix typos in names
    # Rename n_est as n and cv as n_cv
    rename(any_of(c("n" = "n_est",
                    "n_cv" = "cv")))
})


# Format data
################################################################################

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
  # Replace CV values
  mutate(n_cv=recode(n_cv,
                     "n/a (95% CI 1363-1429)" = "",
                     "n/a (95% CI 360-383)" = "",
                     "varies" = "")) |> 
  # Convert values to numeric
  mutate_at(vars(n, n_min, n_cv, 
                 r_max, rf, pbr, msi_fisheries_cv), as.numeric) %>% 
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
  # Format revised (y/n)
  mutate(revised_yn=recode(revised_yn, "N"="no", "Y"="yes")) %>% 
  # Add "nothing" to revision notes when no revision occured
  mutate(revised=ifelse(is.na(revised) & revised_yn=="no", "nothing", revised)) %>% 
  # Format revised
  mutate(revised=recode(revised,
                        "p, m" = "m, p",
                        "strandings"="stranding data")) %>% 
  # Format strategic (Y/N)
  mutate(strategic_yn=recode(strategic_yn, 
                             "Y"="Strategic",
                             "N"="Non-strategic")) %>% 
  # Format MSI fisheries
  rename(msi_fisheries_orig=msi_fisheries) %>%
  mutate(msi_fisheries_orig=gsub("–", "-", msi_fisheries_orig),
         msi_fisheries=ifelse(grepl("-", msi_fisheries_orig), 
                              avg_range(msi_fisheries_orig), msi_fisheries_orig)) %>% 
  mutate(msi_fisheries=as.numeric(msi_fisheries)) %>%
  # Format MSI total
  rename(msi_total_orig=msi_total) %>%
  mutate(msi_total_orig=gsub("–", "-", msi_total_orig),
         msi_total=ifelse(grepl("-", msi_total_orig), 
                              max_range(msi_total_orig), msi_total_orig)) %>% 
  mutate(msi_total=as.numeric(msi_total)) %>%
  # mutate(msi_fisheries_cv=ifelse(grepl("\\(", msi_fisheries_orig), 
  #                                extract_from_parentheses(msi_fisheries_orig), 
  #                                msi_fisheries_cv)) %>% 
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
         r_max, rf, pbr, 
         msi_total_orig, msi_total,
         msi_fisheries_orig, msi_fisheries, msi_fisheries_cv,
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

# Confirm that nothing is listed as revised when revised_yn=="no"
data %>% 
  count(revised_yn, revised)

# Recovery factor (RF)
# The Pygmy killer whale (Northern Gulf of Mexico) RF was 0.05 from 1995-2002
# 0.16 RF is real: The recovery factor was set at 0.16 because of the stock's 
# status relative to OSP is unknown and the minimum population estimate is 11 
# years older than the latest fishery-related mortality estimate
range(data$rf, na.rm=T) 
table(data$rf)

# Rmax
range(data$r_max, na.rm=T) # The North Atlantic right whale Rmax was 0 from 2000-2009
table(data$r_max)

# N values
sort(unique(data$n)) # N=0 appears to be true for some dolphin stocks
sort(unique(data$n_min))
sort(unique(data$n_cv)) # N_cv=0 appears to be true for NA right whale and Gulf of ME humpback whale

# MSI
sort(unique(data$msi_total))
sort(unique(data$msi_fisheries))

# Species
spp_key <- data %>% 
  count(comm_name, species)


# Build area key
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
  # Make Bryde's whale actually Rice's whale (Balaenoptera ricei)
  mutate(comm_name=recode(comm_name, "Bryde's whale" = "Rice's whale"),
         species=recode(species, "Balaenoptera brydei" = "Balaenoptera ricei")) %>% 
  # Update Tamanend's bottlenose dolphin
  mutate(comm_name=ifelse(comm_name=="Common bottlenose dolphin" & area %in% c("WNA Central Florida Coastal", "WNA Northern Florida Coastal", "WNA South Carolina-Georgia Coastal"), 
                          "Tamanend's bottlenose dolphin", comm_name),
         species=ifelse(comm_name=="Tamanend's bottlenose dolphin", "Tursiops erebennus", species)) |> 
         
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

# Confirm only one value per 1 year per stock
data1 %>% count(comm_name, area, year) %>% 
  filter(n>1)


# Add 1997 and 2004
################################################################################

# 1997 data
data97 <- data1 |> 
  # Reduce to 1996
  filter(year==1996) |> 
  # Make 1997 and not revised
  mutate(year=1997,
         revised_yn="no",
         revised="nothing")

# 2004 data
data04 <- data1 |> 
  # Reduce to 2003
  filter(year==2003) |> 
  # Make 2004 and not revised
  mutate(year=2004,
         revised_yn="no",
         revised="nothing")

# Merge
data2 <- bind_rows(data1, data97, data04) |> 
  arrange(group, comm_name, area, year)


# Visual time series checks
################################################################################

# Check non-dolphins
ggplot(data2 %>% filter(group!="Dolphins"), aes(y=stock, x=year, fill=strategic_yn)) +
  facet_wrap(~group, scales="free_y", space="free_y") +
  geom_tile() +
  theme_bw()

# Check GOM dolphins
ggplot(data2 %>% filter(group=="Dolphins" & region=="Gulf of Mexico"), aes(y=area, x=year, fill=strategic_yn)) +
  facet_wrap(~comm_name, scales="free_y", space="free_y") +
  geom_tile() +
  theme_bw()

# Check ATL dolphins
ggplot(data2 %>% filter(group=="Dolphins" & region=="Atlantic"), aes(y=area, x=year, fill=strategic_yn)) +
  facet_wrap(~comm_name, scales="free_y", space="free_y") +
  geom_tile() +
  theme_bw()


# Export data
################################################################################

# Export data
saveRDS(data2, file=file.path(outdir, "Atlantic_SARs_parameters.Rds"))



