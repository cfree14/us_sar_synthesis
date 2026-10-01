
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/abundance/raw"
outdir <- "data/abundance/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "species_abundance.xlsx"), sheet="U.S.", na="-")

# Read species key
spp_key <- readxl::read_excel("data/species_key.xlsx")

# Get official stock key
stock_key <- readRDS(file="data/sars/processed/US_sars_stocks.Rds")


# Tell Yutian to fix years
# Tell Yutian to fix harbor porpoise spelling mistake


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(comm_name=species,
         "source_type"="source_type (digitize/actual)",
         n_units=abundance_units,
         n=abundance,
         n_lo=abundance_low,
         n_hi=abundance_hi) %>% 
  # Format reference
  mutate(reference=gsub("\\(|\\)|,", "", reference)) %>% 
  # Fix common names
  mutate(comm_name=recode(comm_name, 
                          "Habor porpoise"="Harbor porpoise",
                          "Beluga"="Beluga whale",
                          "Mesoplodont beaked whale"="Mesoplodont beaked whales")) %>% 
  # Add species
  left_join(spp_key, by="comm_name") %>% 
  # Format year
  mutate(year=round(year)) %>% 
  # Format survey method
  mutate(survey_method=stringr::str_to_sentence(survey_method),
         survey_method=gsub("counts", "count", survey_method),
         survey_method=recode(survey_method, 
                              "Photo mark-recapture"="Photo mark recapture")) %>% 
  # Format units
  mutate(n_units=recode(n_units, 
                        "Non-pup"="Non-pups",
                        "Total Number"="Total number",
                        "Total Number (Females)" = "Total number of females",
                        "Total Number (Females)**" = "Total number of females",
                        "Total Number (Males)" = "Total number of males",    
                        "Total Number (Males)**" = "Total number of males",
                        "Total Number (raw)" = "Total number (raw)",     
                        "Total Number**"="Total number",           
                        "Total Numbers" ="Total number")) %>% 
  # FORMAT AREA
  # Remove "Alaska - " prefix
  mutate(area=gsub("Alaska - ", "", area)) %>% 
  # Fix some
  mutate(area=recode(area, 
                     "Aleutians" = "Aleutian Islands",
                     "Eastern North Pacific Southern resident" = "ENP Southern Resident",
                     "US West Coast" = "United States",
                     "Hawaii Islands"="Hawaii",
                     "Eastern U.S. (Eastern Stock)" = "Eastern U.S.",
                     "Western U.S. (Alaska)" = "Western U.S.",
                     "Eastern Pacific (Overall)"="Eastern North Pacific",
                     "California-Oregon-Washignton"="California-Oregon-Washington",
                     "California (Channel Islands)"="California", 
                     "Washington (Inland)"="Washington Inland",
                     "Northern California/Southern Oregon"="Northern California-Southern Oregon")) %>% 
  # Fix specific areas
  mutate(area=case_when(comm_name=="Cuvier's beaked whale" & area=="California-Oregon-Washington" ~ "California-Oregon-Washington Offshore",
                        comm_name=="North Atlantic right whale" & area=="Western Atlantic" ~ "Western Stock",
                        T ~ area)) %>% 
  # Arrange
  select(reference, country, stock_id, area, group, comm_name, species, 
         survey_method, n_units, source_type, 
         year, n, n_lo, n_hi, notes, citation, ref_link, everything())

# Inspect
str(data)
freeR::complete(data)

# Reference
freeR::uniq(data$reference)

# Country
freeR::uniq(data$country)

# Area
freeR::uniq(data$area)

# Common name
freeR::uniq(data$comm_name)

# Stock id
freeR::uniq(data$stock_id)

# Check species
spp_key_check <- data %>% 
  select(comm_name, species) %>% unique()

# Year
freeR::uniq(data$year)

# Survey method
freeR::uniq(data$survey_method)

# Source type
freeR::uniq(data$source_type)

# Abundance units
freeR::uniq(data$n_units)

# Harmonize with SARS stock areas / stock ids
################################################################################

# 
stocks <- data %>% 
  select(comm_name, area) %>% 
  unique() %>% 
  left_join(stock_key) %>% 
  filter(is.na(region))


# Export
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "abundance_time_series_database_yutian.Rds"))



# Examine frequency
################################################################################

stats <- data %>% 
  # Number of years in each time series
  group_by(stock_id, group, comm_name, area, source_type, survey_method, n_units) %>% 
  summarize(nyrs=n_distinct(year)) %>% 
  ungroup() %>% 
  # Longest time series for each stock
  arrange(stock_id, desc(nyrs)) %>% 
  group_by(stock_id) %>% 
  slice(1) %>% 
  ungroup()

# Number longer than 20 years
sum(stats$nyrs>=20)

ggplot(stats, aes(y=group, x=nyrs)) +
  geom_violin()

