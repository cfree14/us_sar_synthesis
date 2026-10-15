
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
                     "Mississippi Sound, Lake Borgne, Bay Boudreau" = "Bay Boudreau, Mississippi Sound, Lake Borgne"))

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

# Export data
saveRDS(data, file.path(outdir, "Atlantic_NGOM_bdolphin_SARs_parameters.Rds"))


# Plot data
################################################################################

ggplot(data, aes(x=year, y=area, fill=strategic_yn)) +
  geom_tile() +
  theme_bw()

ggplot(data, aes(x=year, y=area, fill=revised_yn)) +
  geom_tile() +
  # Legend
  scale_fill_manual(name="Revised?", values=c("grey70", "darkred")) +
  # Theme
  theme_bw()





