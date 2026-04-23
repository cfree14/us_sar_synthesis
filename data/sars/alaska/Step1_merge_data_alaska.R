
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/sars/alaska/tables"
outdir <- "data/sars/alaska/processed"

# Species key
species_key <- readxl::read_excel("data/species_key.xlsx")

# Area key
area_key <- readxl::read_excel("data/area_key.xlsx")

# Stock key
stock_key <- readxl::read_excel("data/stock_key_pacific.xlsx")
freeR::which_duplicated(stock_key$stock) # must have no duplicates


# Merge data
################################################################################

# Read data
files2merge <- list.files(indir, pattern=".xlsx")
data_orig <- purrr::map_df(files2merge, function(x){
  df <- readxl::read_excel(file.path(indir, x), na=c("-", "unk", "undet", "n/a", "N/A"), col_types = "text") %>% 
    mutate(filename=x)
})


# Format data
data <- data_orig %>%
  # Add region
  mutate(region="Alaska") %>% 
  # Add year
  mutate(year = str_split(filename, "_", simplify = TRUE)[, 2] %>% as.numeric(.)) %>% 
  # Split species and area
  separate(species_stock, into=c("comm_name", "area"), sep=" \\(") %>% 
  # Format species
  mutate(comm_name=stringr::str_squish(comm_name)) %>% 
  # Add species info
  left_join(species_key, by="comm_name") %>% 
  # Format area
  mutate(area=gsub("\\)", "", area)) %>%
  # Format strategic
  mutate(strategic_yn=recode(strategic_yn, 
                             "NS"="Non-strategic",
                             "S"="Strategic")) %>% 
  # Arrange
  select(filename, year, region, group, comm_name, species, everything())

# Inspect
str(data)
freeR::complete(data)

# Areas
sort(unique(data$area))

# Strategic
table(data$strategic_yn)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "Alaska_SARs_parameters.Rds"))




