# Import floral occurrence data
# Cape vs SWA publication
# Ruan van Mazijk

# Raw flora --------------------------------------------------------------------

GCFR_clean_flora <- read_csv(here::here("data/derived-data/flora/GCFR_clean_flora_2017-09-14.csv"))
SWAFR_clean_flora <- read_csv(here::here("data/derived-data/flora/SWAFR_clean_flora_2017-09-14.csv"))

# Richness rasters -------------------------------------------------------------

GCFR_richness_QDS <- raster(here::here("data/derived-data/flora/GCFR_richness_QDS_2017-09-16.tif"))
stopifnot(proj4string(GCFR_richness_QDS) == std_CRS)
GCFR_richness_HDS <- raster(here::here("data/derived-data/flora/GCFR_richness_HDS_2017-09-16.tif"))
GCFR_richness_3QDS <- raster(here::here("data/derived-data/flora/GCFR_richness_3QDS_2017-09-16.tif"))

SWAFR_richness_QDS <- raster(here::here("data/derived-data/flora/SWAFR_richness_QDS_2017-09-16.tif"))
stopifnot(proj4string(SWAFR_richness_QDS) == std_CRS)
SWAFR_richness_HDS <- raster(here::here("data/derived-data/flora/SWAFR_richness_HDS_2017-09-16.tif"))
SWAFR_richness_3QDS <- raster(here::here("data/derived-data/flora/SWAFR_richness_3QDS_2017-09-16.tif"))
