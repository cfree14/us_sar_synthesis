
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


# Merge data
################################################################################

# Read data
files2merge <- list.files(indir, pattern=".xlsx")
data_orig <- purrr::map_df(files2merge, function(x){
  df <- readxl::read_excel(file.path(indir, x), na=c("-", "unk", "undet", "n/a", "N/A", "NA"), col_types = "text") %>% 
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
  mutate(area=gsub("\r\n", " ", area)) %>% 
  # Format N CV
  mutate(n_cv=gsub(" k", "", n_cv)) %>% 
  # Convert to numeric
  mutate_at(vars(rf), as.numeric) %>% # n_cv
  # Format Rmax
  mutate(r_max=recode(r_max,
                      "0.02a" = "0.02",
                      "0.04a"="0.04") %>% as.numeric(.)) %>%
  # Remove useless
  select(-id) %>% 
  # Arrange
  select(filename, year, group, comm_name, species, 
         center, region, area,
         n, n_cv, n_min,
         r_max, rf, pbr, msi_total, msi_total_cv, msi_fisheries, strategic_yn,
         revised, 
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

# Update (Y/N)
table(data$updated_yn)

# RF and Rmax
table(data$rf)
table(data$r_max)

# Species
spp_key <- data %>% 
  count(comm_name, species)

cv_key <- data %>% 
  count(n_cv)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "Atlantic_SARs_parameters.Rds"))



