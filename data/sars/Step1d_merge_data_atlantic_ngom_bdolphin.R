
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/Whales/us_sar_synthesis_data/atlantic/tables_ngom_bse_bottlenose_dolphins"
keydir <- "data/sars/keys"
outdir <- "data/sars/processed"


# Merge data
################################################################################

# Mere data
files2merge <- list.files(indir)
data_orig <- purrr::map_df(files2merge, function(x){
  df <- readxl::read_excel(file.path(indir, x), col_types = "text", na="unk") %>% 
    mutate(filename=x)
})


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Convert to numeric
  mutate_at(vars(n_est, n_est_cv, n_min, r_max, fr, pbr, msi_total, msi_fisheries), as.numeric) %>% 
  # Format strategic
  mutate(strategic_yn=recode(strategic_yn, "Y"="yes")) %>% 
  # Extract year
  mutate(year=substr(filename, 1, 4) %>% as.numeric()) %>% 
  # Rename a few areas
  mutate(area=recode(area, 
                     "Mississippi Sound, Lake Borgne, Bay Boudreau" = "Bay Boudreau, Mississippi Sound, Lake Borgne")) %>% 
  # Add species
  rename(comm_name=species) %>% 
  mutate(species="Tursiops truncatus",
         group="Dolphins",
         region="Gulf of Mexico",
         center="SEC",
         stock=paste0(comm_name, "(", area, ")")) %>% 
  # Arrange
  select(filename, year, group, stock, comm_name, species, center, region, area, everything())

# Inspect
str(data)
freeR::complete(data)

# Species
table(data$species)

# Area
freeR::uniq(data$area)

# Strategic
table(data$strategic_yn)

# Revised
table(data$revised_yn)

# Plot data
################################################################################

ggplot(data, aes(x=year, y=area, fill=revised_yn)) +
  geom_tile() +
  # Legend
  scale_fill_manual(name="Revised?", values=c("grey70", "darkred")) +
  # Theme
  theme_bw()


# Expand data
################################################################################

# Use 1996 data for 1997 and 1998
data96 <- data %>% 
  filter(year==1996)
data97 <- data96 %>% 
  mutate(year=1997)
data98 <- data96 %>% 
  mutate(year=1998)

# Use 2003 data for 2004
data03 <- data %>% 
  filter(year==2003)
data04 <- data03 %>% 
  mutate(year=2004)

# Merge
data_expanded <- bind_rows(data, data97, data98, data04) %>% 
  arrange(year, area)

ggplot(data_expanded, aes(x=year, y=area, fill=revised_yn)) +
  geom_tile() +
  # Legend
  scale_fill_manual(name="Revised?", values=c("grey70", "darkred")) +
  # Theme
  theme_bw()

# Confirm 1 row per stock year
data_expanded %>% 
  group_by(stock, year) %>% 
  summarize(n=n()) %>% filter(n>1)

# Export data
################################################################################

# Export data
saveRDS(data_expanded, file.path(outdir, "Atlantic_NGOM_bdolphin_SARs_parameters.Rds"))








