
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


# Merge data
################################################################################

# Read data
files2merge <- list.files(indir, pattern=".xlsx")
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read file
  df <- readxl::read_excel(file.path(indir, x), 
                           na=c("-", "unk", "undet", "n/a", "N/A", "UNDET", "b"), 
                           col_types = "text") %>% 
    # Add filename
    mutate(filename=x) %>% 
    # Fix typos in names
    rename(any_of(c("survey_interval" = "sruvey_interval",
                    "sim_fisheries" = "sim_fisheres",
                    "sim_fisheries" = "sim_fishery",
                    "rf" = "fr",
                    "strategic_yn" = "stragetig_yn",
                    "survey_interval" = "years_since")))
  
})


# Format data
data <- data_orig %>%
  # Rename
  rename(comm_name=species,
         area=stock) %>% 
  # Add region
  mutate(region="Alaska") %>% 
  # Add year
  mutate(year = str_split(filename, "_", simplify = TRUE)[, 2] %>% as.numeric(.)) %>% 
  # Split stock
  separate(species_stock, into=c("comm_name1", "area1"), sep=" \\(", remove=F) %>% 
  mutate(comm_name=ifelse(!is.na(comm_name), comm_name, comm_name1),
         area=ifelse(!is.na(area), area, area1)) %>% 
  select(-c(comm_name1, area1, species_stock)) %>% 
  # Format species
  mutate(comm_name=stringr::str_squish(comm_name),
         comm_name=recode(comm_name,
                          "Right whale"="North Pacific right whale")) %>% 
  # Add species info
  left_join(species_key, by="comm_name") %>% 
  # Format area
  mutate(area=gsub("\\)", "", area)) %>%
  # Format strategic
  mutate(strategic_yn=recode(strategic_yn, 
                             "NS"="Non-strategic",
                             "S"="Strategic")) %>% 
  # Format revised
  mutate(revised=recode(revised, 
                        "N/A (New SAR in 2022)"="2022", 
                        "N/A (New SAR in 2023)"="2023") %>% as.numeric(.)) %>% 
  # Format SIM fisheries
  mutate(sim_fisheries=recode(sim_fisheries, 
                              "51.6a"="51.6")) %>% 
  # Convert numeric
  # Neet to format N_MIN, N_CV, PBR, survey_interval, etc
  mutate_at(vars(r_max, rf, n_cv, sim_fisheries, sim_native), as.numeric) %>% #
  # Format areas
  mutate(area=gsub("\r", " ", area), 
         area=gsub("E. ", "Eastern ", area),
         area=gsub("W. ", "Western ", area),
         area=gsub("N. ", "North ", area),
         area=gsub("SE", "Southeast", area),
         area=gsub("U. S.", "U.S.", area),
         area=gsub("/ ", "/", area),
         area=recode(area,
                     "AT1 transient"="AT1 Transient",
                     "Cook Inlet/Shelikof"="Cook Inlet/Shelikof Strait",
                     "Sitka/Chatham"="Sitka/Chatham Strait",
                     "Lynn Canal/Stephens"="Lynn Canal/Stephens Passage",
                     "Unidentified stock"="Unidentified",
                     "Eastern US only"="Eastern U.S.",
                     "Western"="Western U.S.",
                     "Eastern Pacific"="Eastern North Pacific",
                     "Cent. North Pacific"="North Pacific",
                     # Humpback craziness
                     # ENP Alaska Resident
                     "Alaska Resident" = "ENP Alaska Resident",
                     "Eastern North Pacific Alaska Resident" = "ENP Alaska Resident",
                     # ENP GOA/BSAI Transient
                     "GOA, AI, BS Transient" = "ENP GOA/BSAI Transient",
                     "Eastern North Pacific Gulf of Alaska, Aleutian Islands, and Bering Sea Transient" = "ENP GOA/BSAI Transient",
                     # ENP Northern Resident (British Columbia)
                     "Northern Resident (British Columbia" = "ENP Northern Resident (British Columbia)",
                     "Eastern North Pacific Northern Resident" = "ENP Northern Resident (British Columbia)",
                     "Eastern North Pacific Northern Resident (British Columbia" = "ENP Northern Resident (British Columbia)",                      
                     "Eastern North Pacific Northern Resident [British Columbia]" = "ENP Northern Resident (British Columbia)",
                     ),
         area=case_when(comm_name=="Spotted seal" & area %in% c("Bering", "Alaska") ~ "Bering stock",
                        comm_name=="Ringed seal" & area %in% c("Arctic", "Alaska") ~ "Arctic stock",
                        comm_name=="Bearded seal" & area %in% c("Beringia", "Alaska") ~ "Beringia stock",
                        T ~ area)) %>% 
  # Arrange
  select(filename, year, region, group, comm_name, species, area,
         n_est, n_cv, n_min, r_max, rf, pbr,
         sim_fisheries, sim_native, sim_total, strategic_yn, 
         last_survey, survey_interval, updated_yn, revised, comments, everything())

# Inspect
str(data)
freeR::complete(data)

# Areas
sort(unique(data$area))

# Strategic
table(data$strategic_yn)

sort(unique(data$n_est))
sort(unique(data$n_min))
sort(unique(data$sim_total))
sort(unique(data$sim_native))

# Last survey
sort(unique(data$last_survey))
sort(unique(data$survey_interval))

# Updated
sort(unique(data$updated_yn))

# Revised year
sort(unique(data$revised))


# Visualize stocks
################################################################################

# Confirm 1 row per stock
count(data, comm_name, area, year) %>% filter(n>1)

# Done: Otariids, Porpoises, Small whales, Dolphins, Phocids
# Working: Large whales
ggplot(data, #%>% filter(group=="Large whales"), 
       aes(x=year, 
           y=paste(comm_name, area, sep="-"), fill=strategic_yn)) +
  facet_grid(group~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Year", y="") +
  # Theme
  theme_bw()


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "Alaska_SARs_parameters.Rds"))




