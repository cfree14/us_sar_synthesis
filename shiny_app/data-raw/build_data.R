
# Read data
data <- readRDS("data/sars/processed/US_sars_data.Rds")

# Export data
saveRDS(data, file="shiny_app/data/US_sars_data.Rds")