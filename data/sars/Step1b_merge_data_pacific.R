
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/Whales/us_sar_synthesis_data/pacific/tables"
keydir <- "data/sars/keys"
outdir <- "data/sars/processed"

# Species key
species_key <- readxl::read_excel("data/species_key.xlsx")

# Area key
# area_key <- readxl::read_excel("data/area_key.xlsx")

# Stock key
# stock_key <- readxl::read_excel("data/stock_key_pacific.xlsx")
# freeR::which_duplicated(stock_key$stock) # must have no duplicates

# To do list
# - Check for missing values in data_orig; fill in true NAs with N/A; expand workflow to confirm that only true NAs are present
# - Record the SIM modifiers
# - Derive non-fish SIM
# - Mark revision years
# - Extract survey years

# Checks 
# 1) Are PBR calculations aligned?
# 2) Is total SIM larger than fisheries SIM?


# Step 1. Merge
################################################################################

# Loop through files
files2merge <- list.files(indir, pattern=".xlsx")
data_orig <- purrr::map_df(files2merge, function(x){
  df <- readxl::read_excel(file.path(indir, x), na=c("N/A", "n/a", "unk", "undet", "und"), col_types = "text") %>% 
    mutate(filename=x) %>% 
    # Fix typos in names
    rename(any_of(c("notes" = "comments")))
})

# Inspect
# YOU WERE PROGRAMMING A WAY OF LOOKING AT THE NUMBER OF MISSING VALUES IN EACH FILE
years <- sort(unique(data_orig$filename))
freeR::complete(data_orig)
x <- years[1]
stats <- purrr::map_df(years, function(x){
  sdata <- data_orig %>% 
    filter(filename==x) 
  df <- freeR::complete(sdata)
})


# Step 2. Basic cleaning
################################################################################

# Format
data1 <- data_orig %>% 
  # Rename
  rename(comm_name=species, 
         sim_tot_orig=sim_total,
         sim_fish_orig=sim_fisheries) %>% 
  # Add year
  mutate(year = str_split(filename, "_", simplify = TRUE)[, 2] %>% as.numeric(.)) %>% 
  # Convert to numeric
  mutate_at(vars(n_est, n_cv, n_min, r_max, rf, pbr,
                 survey1, survey2, survey3, revision_yr), .funs=as.numeric) %>% 
  # Fix strategic (yes/no) 
  mutate(strategic_yn=recode(strategic_yn,
                             "Y"="Strategic",
                             "N"="Non-strategic",
                             "NS"="Non-strategic",
                             "S"="Strategic")) %>% 
  # Split species/stock into common name and area (for ones with that format)
  separate(species_stock, into=c("comm_name1", "area1"), sep=" \\(", remove=F) %>% 
  mutate(area1=gsub("\\)", "", area1), 
         comm_name=ifelse(is.na(comm_name), comm_name1, comm_name), 
         area=ifelse(is.na(area), area1, area)) %>% 
  select(-c(comm_name1, area1, species_stock)) %>% 
  # Fix some crazy characters in areas to make merge below go better
  mutate(area=gsub("’|ʻ|'","", area),
         area=gsub(" ‚Äì ", " - ", area),
         area=case_when(grepl("Russian", area) ~ "San Francisco-Russian River",
                        grepl("Central America", area) ~ "Central America/Southern Mexico-California/Oregon/Washington",
                        grepl("Mainland Mexico", area) ~ "Mainland Mexico-California/Oregon/Washington",
                        T ~ area)) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=gsub("’", "'", comm_name), 
         comm_name=recode(comm_name, 
                          "Monk seal"="Hawaiian monk seal",
                          "Indopacetus pacificus" = "Longman's beaked whale",
                          "Bottlenose dolphin" = "Common bottlenose dolphin",
                          "Common dolphin, long-beaked"="Long-beaked common dolphin",
                          "Common dolphin, short-beaked"="Short-beaked common dolphin",
                          "Northern right-whale dolphin"="Northern right whale dolphin",
                          "Pilot whale, short-finned"="Short-finned pilot whale")) %>% 
  # Format otters
  mutate(comm_name=case_when(comm_name=="Sea otter" & area=="Washington" ~ "Northern sea otter",
                             comm_name=="Sea otter" & area=="Southern" ~ "Southern sea otter",
                             comm_name=="Sea otter" & area=="Northern (Washington)" ~ "Northern sea otter",
                             comm_name=="Sea otter" & area=="Southern (California)" ~ "Southern sea otter",
                             T ~ comm_name)) %>% 
  # Add species and group
  left_join(species_key, by=c("comm_name")) %>% 
  # Format SIM total
  mutate(sim_tot_orig=gsub(" ", "", sim_tot_orig), 
         sim_tot=gsub("|<|>|≥|.*-", "", sim_tot_orig) %>% as.numeric()) %>% 
  # Format SIM fisheries
  mutate(sim_fish_orig=gsub(" ", "", sim_fish_orig), 
         sim_fish=gsub("|<|>|≥|.*-", "", sim_fish_orig) %>% as.numeric()) %>% 
  # Derive PBR
  mutate(pbr_derived=n_min*(r_max/2)*rf,
         pbr_check=round(pbr_derived - pbr, 0)) %>%
  # Format revised (yes/no)
  mutate(revised_yn=ifelse(revised_yn=="yes" & !is.na(revised_yn), "Revised", "Same as previous")) %>% 
  # Arrange
  select(-region) %>% 
  select(filename, year, 
         group, comm_name, species, 
         area, center, 
         n_est, n_cv, n_min, r_max, rf, 
         pbr, pbr_derived, pbr_check,
         sim_tot_orig, sim_tot,
         sim_fish_orig, sim_fish,
         strategic_yn,
         survey1, survey2, survey3, revision_yr, revised_yn, notes,
         everything())

# Inspect
str(data1)
freeR::complete(data1)

# Number of revised stocks per year
revision_stats <- data1 %>% 
  group_by(year) %>% 
  summarize(nrevised=sum(revised_yn=="yes" & !is.na(revised_yn))) %>% 
  ungroup()

# Center
table(data1$year)
table(data1$center)
table(data1$strategic_yn)

# Species key
spp_key <- data1 %>% 
  count(comm_name, species)

table(data1$sim_tot)


# Build area keys
################################################################################

# Area key
area_key <- data1 %>% 
  count(comm_name, area) %>% 
  rename(area_orig=area)
write.csv(area_key, file=file.path(keydir, "area_key_pacific_raw.csv"), row.names=F)

# Read area key
area_key_use <- readxl::read_excel(file.path(keydir, "area_key_pacific_final.xlsx"))


# Step 3. Add areas
################################################################################

# Add
data2 <- data1 %>% 
  # Add area
  rename(area_orig=area) %>% 
  left_join(area_key_use, by=c("comm_name", "area_orig")) %>% 
  # Build stock
  mutate(stock=paste0(comm_name, " (", area, ")")) %>% 
  # Arrange
  select(filename, year, 
         group, 
         stock, comm_name, species, 
        # region, 
         area, area_orig, center, 
         n_est, n_cv, n_min, r_max, rf, 
         pbr, pbr_derived, pbr_check,
         sim_tot_orig, sim_tot,
         sim_fish_orig, sim_fish,
         strategic_yn,
         survey1, survey2, survey3, revised_yn, notes,
         everything())

# Inspect
str(data2)
freeR::complete(data2)

# Make sure that there is only 1 value per stock (comm-name-area) and year
data2 %>% 
  count(stock, year) %>% 
  filter(n!=1)


# Step 3. Expand 1999 stocks
################################################################################

# Build missing 1999 stocks
data99 <- data2 %>% 
  filter(year==1999)
data98 <- data2 %>% 
  filter(year==1998)
data99_not_updated <- data98 %>% 
  # Reduce to stocks excluded from 1999 table
  filter(!stock %in% data99$stock) %>% 
  # Update to reflect 1999
  mutate(year=1999,
         filename="1999 SAR (not included in 1999 SAR table)", 
         revised_yn="Same as previous")

# Add missing 1999 stocks to data
data3 <- bind_rows(data2, data99_not_updated) %>% 
  arrange(year, region, group)


# Check 
################################################################################


#
ggplot(data3,#%>% filter(group=="Porpoises"), # Phocids, Otariids, Porpoises, Small whales, Large whales, Dolphins
       aes(y=stock, x=year, fill=strategic_yn)) +
  facet_grid(group+region~., scale="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 
ggplot(data3 %>% filter(group=="Dolphins"), # Phocids, Otariids, Porpoises, Small whales, Large whales, Dolphins
       aes(y=stock, x=year, fill=strategic_yn)) +
  facet_grid(group+region~., scale="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Export
################################################################################

# Export data
saveRDS(data3, file=file.path(outdir, "Pacific_SARs_parameters.Rds"))

