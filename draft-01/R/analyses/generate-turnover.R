# Generate species richness and turnover data
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))
output_path <- here("outputs/turnover")

# Compute HDS richness, mean QDS richness and turnover -------------------------

# If not already computed and saved to disc
# (I re-ran this script many times, and wanted to avoid repeating
# the heavy computations.)

GCFR_species_path  <- glue("{output_path}/GCFR_species_2018-08-14/")
SWAFR_species_path <- glue("{output_path}/SWAFR_species_2018-08-14")

# TODO: try computing again, to check reproducible
#GCFR_species_path <- glue("{output_path}/GCFR_spp_2018-08-14")
#SWAFR_species_path <- glue("{output_path}/SWAFR_spp_2018-08-14")

if (!file.exists(GCFR_species_path)) {
  source(here("R/setup.R"))
  # Read in processed GBIF occurence data as SpatialPointsDataFrames
  trimmed_GCFR_clean_flora_spdf_species <- read_rds(here(
    "data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_species"
  ))
  GCFR_species <- calc_richness_turnover(
    flora_points = trimmed_GCFR_clean_flora_spdf_species,
    QDS_polygon  = GCFR_QDS,
    output_path  = output_path,
    region_name  = "GCFR", date = "2018-08-14"
  )
} else {
  GCFR_species <- readOGR(GCFR_species_path)
}

if (!file.exists(SWAFR_species_path)) {
  source(here("R/setup.R"))
  trimmed_SWAFR_clean_flora_spdf_species <- read_rds(here(
    "data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_species"
  ))
  SWAFR_species <- calc_richness_turnover(
    flora_points = trimmed_SWAFR_clean_flora_spdf_species,
    QDS_polygon  = SWAFR_QDS,
    output_path  = output_path,
    region_name  = "SWAFR", date = "2018-08-14"
  )
} else {
  SWAFR_species <- readOGR(SWAFR_species_path)
}
