
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

# Read data 
data_orig <- readxl::read_excel("data/sars/atlantic/sars/Atlantic_2023_AppendixIV.xlsx", na=c("unknown", "-", "na", "unk"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Format species
  mutate(species=stringr::str_to_sentence(species)) %>% 
  # Extract year
  mutate(year1=substr(year, 1, 4) %>% as.numeric(),
         year2=substr(year, 6, 9) %>% as.numeric()) %>% 
  # Format N
  mutate(n=gsub(",", "", n) ) %>% 
  separate(n, sep="–", into=c("n1", "n2"), remove=F, convert = T) %>% 
  mutate(n1=as.numeric(n1)) %>% 
  # Format N CV
  separate(n_cv, sep="–", into=c("n_cv1", "n_cv2"), remove=F, convert = T) %>% 
  # Add stock
  mutate(stock=paste0(species, " (", area, ")")) %>% 
  # Arrange
  select(stock, species, area, 
         year, year1, year2, 
         n, n1, n2, 
         n_cv, n_cv1, n_cv2, 
         everything())

# Inspect
str(data)

# Plot
ggplot(data %>% filter(species=="Minke whale"), aes(x=year1, y=n1)) +
  facet_wrap(~stock, ncol=6, scales="free") +
  geom_line() +
  geom_point() +
  theme_bw()





