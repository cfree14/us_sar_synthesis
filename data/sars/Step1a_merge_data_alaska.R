
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/Whales/us_sar_synthesis_data/alaska/tables"
outdir <- "data/sars/processed"

# Species key
species_key <- readxl::read_excel("data/species_key.xlsx")

# Killer whale stocks are super confusing - review carefully


# Merge data
################################################################################

# Read data
files2merge <- list.files(indir, pattern=".xlsx")
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read file
  df <- readxl::read_excel(file.path(indir, x), 
                           na=c("-", "unk", "undet", "n/a", "N/A", "UNDET", "UNK", "b", "see txt", "seetxt", "NA"), 
                           col_types = "text") %>% 
    # Add filename
    mutate(filename=x) %>% 
    # Fix typos in names
    rename(any_of(c("survey_interval" = "sruvey_interval",
                    "sim_fisheries" = "sim_fisheres",
                    "sim_fisheries" = "sim_fishery",
                    "rf" = "fr",
                    "strategic_yn" = "stragetig_yn",
                    "survey_interval" = "years_since",
                    "updated_yn"="revised_yn")))
  
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
                          "Pac white-sided dolphin"="Pacific white-sided dolphin",
                          "Pacific white- sided dolphin"="Pacific white-sided dolphin",
                          "Northern right whale"="North Pacific right whale",
                          "Right whale"="North Pacific right whale")) %>% 
  # Add species info
  left_join(species_key, by="comm_name") %>% 
  # Format area
  mutate(area=gsub("\\)", "", area)) %>%
  # Format strategic
  mutate(strategic_yn=recode(strategic_yn, 
                             "NS"="Non-strategic",
                             "S"="Strategic")) %>% 
  # Format statuses
  mutate(osp_status=stringr::str_to_sentence(osp_status)) %>% 
  mutate(esa_status=stringr::str_to_sentence(esa_status)) %>% 
  # Format revised
  mutate(revised=recode(revised, 
                        "N/A (New SAR in 2022)"="2022", 
                        "N/A (New SAR in 2023)"="2023") %>% as.numeric(.)) %>% 
  # Format updated
  mutate(updated_yn=recode(updated_yn, "N"="no", "Y"="yes")) %>% 
  # Format N_est
  mutate(n_est=gsub("\r|,", "", n_est),
         n_est=gsub("[A-Za-z]+$", "", n_est)) %>% 
  # Format N Min
  mutate(n_min=gsub(",", "", n_min),
         pbr=gsub(",", "", pbr)) %>% 
  # Format SIM fisheries
  mutate(sim_fisheries=recode(sim_fisheries, 
                              "51.6a"="51.6")) %>% 
  # Convert numeric
  # Neet to format N_MIN, N_CV, PBR, survey_interval, n_cv, sim_fisheries, sim_native, etc
  mutate_at(vars(r_max, rf, n_est, n_cv, n_min, pbr,
                 sim_total, sim_native, sim_fisheries), as.numeric) %>% 
  # Fix Rmax
  mutate(r_max=ifelse(year<=2005, r_max*2, r_max)) %>% 
  # Format areas
  mutate(area=gsub("\r", " ", area), 
         area=gsub("E. ", "Eastern ", area),
         area=gsub("East. ", "Eastern ", area),
         area=gsub("W. ", "Western ", area),
         area=gsub("West. ", "Western ", area),
         area=gsub("N. ", "North ", area),
         area=gsub("SE", "Southeast", area),
         area=gsub("U. S.", "U.S.", area),
         area=gsub("/ ", "/", area),
         area=gsub("transient", "Transient", area),
         area=gsub("resident", "Resident", area),
         area=recode(area,
                     "Norton Sound" = "Eastern Bering Sea",
                     "Unidentified stock"="Unidentified",
                     # Make longer
                     "Cook Inlet/Shelikof"="Cook Inlet/Shelikof Strait",
                     "Sitka/Chatham"="Sitka/Chatham Strait",
                     "Lynn Canal/Stephens"="Lynn Canal/Stephens Passage",
                     "Beaufort" = "Beaufort Sea",
                     # Eastern North Pacific
                     "Eastern Pacific"="Eastern North Pacific",
                     "Eastern North Pac."= "Eastern North Pacific",
                     "Western North Pac."= "Western North Pacific",
                     # Central North Pacific
                     "Cent.North Pacific"="North Pacific",
                     "Cent. North Pacific"="North Pacific",
                     "Central North Pacific"="North Pacific",
                     # Harbor porpoise
                     "Alaska-aerial" ="Alaska (aerial survey)",
                     "Alaska-vessel" ="Alaska (vessel survey)",
                     # Eastern/Western U.S.
                     "East. U.S." = "Eastern U.S.",
                     "Eastern US only"="Eastern U.S.",
                     "Eastern" = "Eastern U.S.",
                     "W.U.S." ='Western U.S.',
                     "Western"="Western U.S.",
                     # Humpback craziness
                     "CNorth - entire stock" = "Central North Pacific",
                     "Central North Pacific - entire stock" = "Central North Pacific",
                     "CNorth - SoutheastAK feeding area" = "CNP-SEAK/NBC feeding area",
                     "CNorth - SoutheastAK/NBC feeding area" = "CNP-SEAK/NBC feeding area",
                     "CNorth - BS/AI feeding area" = "CNP-BS/AI feeding area",               
                     "CNorth - GOA feeding area" = "CNP-GOA feeding area", 
                     "Hawaiʻi" = "Hawaii",
                     # ENP Alaska Resident
                     "Alaska Resident" = "ENP Alaska Resident",
                     "Eastern North Pacific Alaska Resident" = "ENP Alaska Resident",
                     # ENP GOA/BSAI Transient
                     "GOA, AI, BS Transient" = "ENP GOA/BSAI Transient",
                     "Eastern North Pacific Gulf of Alaska, Aleutian Islands, and Bering Sea Transient" = "ENP GOA/BSAI Transient",
                     # ENP Transient
                     "Transient" = "ENP Transient",
                     "Eastern North Pacific Transient" = "ENP Transient",
                     # ENP Northern Resident (British Columbia)
                     "Resident" = "ENP Northern Resident (British Columbia)", 
                     "Northern resident (British Columbia" = "ENP Northern Resident (British Columbia)",
                     "Northern Resident (British Columbia" = "ENP Northern Resident (British Columbia)",
                     "Eastern North Pacific North Resident" = "ENP Northern Resident (British Columbia)",
                     "Eastern North Pacific Northern Resident" = "ENP Northern Resident (British Columbia)",
                     "Eastern North Pacific Northern Resident (British Columbia" = "ENP Northern Resident (British Columbia)",                      
                     "Eastern North Pacific Northern Resident [British Columbia]" = "ENP Northern Resident (British Columbia)",
                     ),
         # Species-specific fixes
         area=case_when(comm_name=="Humpback whale" & area %in% c("North Pacific") ~ "Central North Pacific",
                        comm_name=="Sperm whale" & area %in% c("Alaska") ~ "North Pacific",
                        comm_name=="North Pacific right whale" & area %in% c("North Pacific") ~ "Eastern North Pacific",
                        comm_name=="Fin whale" & area %in% c("Alaska", "North Pacific") ~ "Northeast Pacific",
                        comm_name=="Spotted seal" & area %in% c("Bering", "Alaska") ~ "Bering stock",
                        comm_name=="Ringed seal" & area %in% c("Arctic", "Alaska") ~ "Arctic stock",
                        comm_name=="Bearded seal" & area %in% c("Beringia", "Alaska") ~ "Beringia stock",
                        T ~ area)) %>% 
  # Check PBR
  mutate(pbr_calc=n_min*r_max/2*rf,
         pbr_diff=round(pbr-pbr_calc, 2)) %>% 
  # Use calculated PBR when available?
  # mutate(pbr=ifelse(is.na(pbr), round(pbr_calc, 1), pbr)) %>% 
  # Arrange
  select(filename, year, region, group, comm_name, species, area,
         n_est, n_est_notes, n_cv, n_min, n_min_notes, r_max, rf, 
         pbr, pbr_calc, pbr_diff, pbr_notes,
         sim_fisheries, sim_native, sim_total, sim_total_notes, strategic_yn, 
         last_survey, survey_interval, updated_yn, revised, comments, everything()) %>% 
  # Remove records that aren't really stocks
  filter(!grepl("feeding area|survey", area))

# Inspect
str(data)
freeR::complete(data)

# Check PBRs
# The 2006 Eastern Steller sea lion PBR was calculated incorrectly (2000 instead of 2004)
sum(abs(data$pbr_diff)>1, na.rm=T)

# Inspect simple version
data_simple <- data %>% select(filename:n_est, n_cv, n_min, r_max, rf, pbr, 
                               sim_fisheries, sim_native, sim_total, strategic_yn, updated_yn)
freeR::complete(data_simple)
# 3 missing status are true: 1995, pending co-mgmt
# 3 missing RFs are for the same stocks


# Areas
sort(unique(data$area))

# Strategic
table(data$strategic_yn)
table(data$osp_status)
table(data$esa_status)

# Rmax
freeR::uniq(data$r_max)

# RF
freeR::uniq(data$rf)

# MNPL
freeR::uniq(data$mnpl)

# Last survey
sort(unique(data$last_survey))
sort(unique(data$survey_interval))

# Updated
sort(unique(data$updated_yn))

# Revised year
sort(unique(data$revised))

# N values
freeR::uniq(data$n_est)
freeR::uniq(data$n_cv)
freeR::uniq(data$n_min)
freeR::uniq(data$pbr)

# SIM
freeR::uniq(data$sim_total)
freeR::uniq(data$sim_native)
freeR::uniq(data$sim_fisheries)


# Visualize stocks
################################################################################

# Confirm 1 row per stock
count(data, comm_name, area, year) %>% filter(n>1)

# Done: Otariids, Porpoises, Dolphins, Phocids, Large whales
# Working: Small whales
ggplot(data, #%>% filter(group=="Small whales"), 
       aes(x=year, 
           y=paste(comm_name, area, sep="-"), fill=strategic_yn)) +
  facet_grid(group~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Year", y="", fill='Status') +
  scale_x_continuous(breaks=seq(1992, 2026,2)) +
  # Legend
  # Theme
  theme_bw() +
  theme(legend.position = "top")

# PBR
############
ggplot(data, #%>% filter(group=="Small whales"), 
       aes(x=year, 
           y=paste(comm_name, area, sep="-"), fill=pbr/1000)) +
  facet_grid(group~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Year", y="", fill='Status') +
  scale_x_continuous(breaks=seq(1992, 2026,2)) +
  # Legend
  scale_fill_gradientn(name="PBR", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  # Theme
  theme_bw() +
  theme(legend.position = "top")

# SIM total
############
ggplot(data, #%>% filter(group=="Small whales"), 
       aes(x=year, 
           y=paste(comm_name, area, sep="-"), fill=sim_total)) +
  facet_grid(group~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Year", y="", fill='Status') +
  scale_x_continuous(breaks=seq(1992, 2026,2)) +
  # Legend
  scale_fill_gradientn(name="M/SI total", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  # Theme
  theme_bw() +
  theme(legend.position = "top")

# SIM native
############
ggplot(data, #%>% filter(group=="Small whales"), 
       aes(x=year, 
           y=paste(comm_name, area, sep="-"), fill=sim_native)) +
  facet_grid(group~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Year", y="", fill='Status') +
  scale_x_continuous(breaks=seq(1992, 2026,2)) +
  # Legend
  scale_fill_gradientn(name="M/SI total", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  # Theme
  theme_bw() +
  theme(legend.position = "top")   



# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "Alaska_SARs_parameters.Rds"))




