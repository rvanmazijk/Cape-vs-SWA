# Process floral occurrence data
# Importing and cleaning plant occurence record data (GBIF)
# Cape vs SWA publication
# Ruan van Mazijk

# Create polygons to query occs in GBIF ----------------------------------------
# (Uses "Well Known Text" format)

SWAFR_border_buffered %>%
  make_wkt() %>%
  write(file = here::here("Data/SWAFR_POLYGON_WKT.txt"))
GCFR_border_buffered %>%
  make_wkt() %>%
  write(file = here::here("Data/GCFR_POLYGON_WKT.txt"))

# e.g. <URL>
#   http://www.gbif.org/occurrence/search?
#       TAXON_KEY=7707728&
#       TAXON_KEY=6&
#       HAS_COORDINATE=true&
#       GEOMETRY=
#           118.90000445+-34.50000126%2C
#           118.90000445+-35.50000127%2C
#           ...
#           118.90000445+-34.50000126
# </URL>
# as derived from `make_wkt()`'s .txt outputs above

# Import GBIF query results (raw) from disc ------------------------------------

# These two `.tsv`s are large, so bear with
GBIF_GCFR <- read_tsv(paste(
  giswd, "GBIF", "GBIF_GCFRquery_2017-07-24.csv",
  sep = "/"
))
GBIF_SWAFR <- read_tsv(paste(
  giswd, "GBIF", "GBIF_SWAFRquery_2017-07-24.csv",
  sep = "/"
))

GBIF_GCFR_tidy <- tidy_gbif(GBIF_GCFR)
GBIF_SWAFR_tidy <- tidy_gbif(GBIF_SWAFR)

# Summarise the possible species sets based on the different columns
GCFR_spp_sets <- summarise_spp_sets(GBIF_GCFR_tidy)
summary(GCFR_spp_sets)
SWAFR_spp_sets <- summarise_spp_sets(GBIF_SWAFR_tidy)
summary(SWAFR_spp_sets)

# Save copies of the species lists to local repo
saveRDS(GCFR_spp_sets, here::here(
  "Data",
  "derived-data",
  "flora",
  "GCFR_spp_sets_local.rds"
))
GCFR_spp_sets <- readRDS(here::here(
  "Data",
  "derived-data",
  "flora",
  "GCFR_spp_sets_local.rds"
))
saveRDS(SWAFR_spp_sets, here::here(
  "Data",
  "derived-data",
  "flora",
  "SWAFR_spp_sets_local.rds"
))
SWAFR_spp_sets <- readRDS(here::here(
  "Data",
  "derived-data",
  "flora",
  "SWAFR_spp_sets_local.rds"
))
