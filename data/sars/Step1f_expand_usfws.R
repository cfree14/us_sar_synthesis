
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/Whales/us_sar_synthesis_data/usfws"
keydir <- "data/sars/keys"
outdir <- "data/sars/processed"

# Species key
species_key <- readxl::read_excel("data/species_key.xlsx")


# Merge
################################################################################

# Files
files2merge <- list.files(indir, pattern=".xlsx")

# Merge
data_orig <- purrr::map_df(files2merge, function(x){
  df <- readxl::read_excel(file.path(indir, x), na = c("N/A", "NA")) %>% 
    mutate(filename=x)
})


# Format
################################################################################

# Format data
data <- data_orig %>% 
  # Add not revised
  mutate(revised_yn=ifelse(is.na(revised_yn), "no", revised_yn)) %>% 
  # Fill
  group_by(filename) %>% 
  fill(everything(), .direction="down") %>% 
  ungroup() %>% 
  # Add species
  rename(comm_name=species) %>% 
  left_join(species_key) %>% 
  # Add stock
  mutate(stock=paste0(comm_name, " (", area, ")")) %>% 
  # Arrange
  select(filename, 
         stock, group, comm_name, species, region, area, 
         revised_yn, year, n_est, n_cv, n_min, r_max, rf, pbr,
         sim_total, sim_fisheries, sim_native, sim_other,
         strategic_yn, mmpa_status, esa_status, trend, notes, last_survey_yr,
         everything())

# Inspect
str(data)
freeR::complete(data)


# Plot
################################################################################

# Plot
ggplot(data, aes(x=year, y=stock, fill=revised_yn)) + # strategic_yn
  geom_tile()


# Export
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "USFWS_SARs_parameters.Rds"))

