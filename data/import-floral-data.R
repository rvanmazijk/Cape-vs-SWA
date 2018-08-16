# Import floral occurrence data
# Cape vs SWA publication
# Ruan van Mazijk

flora_dir <- here::here("data/derived-data/flora")

trimmed_GCFR_clean_flora_spdf_species <- read_rds(glue(
  "{flora_dir}/trimmed_GCFR_clean_flora_spdf_species"
))
trimmed_SWAFR_clean_flora_spdf_species <- read_rds(glue(
  "{flora_dir}/trimmed_SWAFR_clean_flora_spdf_species"
))
