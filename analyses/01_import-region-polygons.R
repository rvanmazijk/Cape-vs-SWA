# Import region polygons
# Cape vs SWA publication
# Ruan van Mazijk

# GCFR -------------------------------------------------------------------------

GCFR_border <- readRDS(here::here("data/derived-data/borders/GCFR_border.rds"))
stopifnot(proj4string(GCFR_border) == std_CRS)
GCFR_box <- readRDS(here::here("data/derived-data/borders/GCFR_box.rds"))
GCFR_QDS <- readRDS(here::here("data/derived-data/borders/GCFR_QDS.rds"))

# SWAFR ------------------------------------------------------------------------

SWAFR_border <- readOGR(here::here("data/derived-data/borders/SWBP_Mike-Cramer"))
stopifnot(proj4string(SWAFR_border) == std_CRS)
SWAFR_box <- readRDS(here::here("data/derived-data/borders/SWAFR_box.rds"))
SWAFR_QDS <- readRDS(here::here("data/derived-data/borders/SWAFR_QDS.rds"))
