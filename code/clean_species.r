
# Read data
data_orig <- readxl::read_excel("data/species.xlsx")

freeR::which_duplicated(data_orig$comm_name)
freeR::which_duplicated(data_orig$species)

library(tidyverse)
freeR::check_names(data_orig$species)
