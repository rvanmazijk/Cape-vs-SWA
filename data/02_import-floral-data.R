# Import floral occurrence data
# Cape vs SWA publication
# Ruan van Mazijk

flora_dir <- here::here("data/derived-data/flora")

# Richness rasters -------------------------------------------------------------

GCFR_richness_QDS <-
  raster(glue("{flora_dir}/GCFR_richness_QDS_2017-09-16.tif"))
stopifnot(proj4string(GCFR_richness_QDS) == std_CRS)
GCFR_richness_HDS <-
  raster(glue("{flora_dir}/GCFR_richness_HDS_2017-09-16.tif"))
GCFR_richness_3QDS <-
  raster(glue("{flora_dir}/GCFR_richness_3QDS_2017-09-16.tif"))

SWAFR_richness_QDS <-
  raster(glue("{flora_dir}/SWAFR_richness_QDS_2017-09-16.tif"))
stopifnot(proj4string(SWAFR_richness_QDS) == std_CRS)
SWAFR_richness_HDS <-
  raster(glue("{flora_dir}/SWAFR_richness_HDS_2017-09-16.tif"))
SWAFR_richness_3QDS <-
  raster(glue("{flora_dir}/SWAFR_richness_3QDS_2017-09-16.tif"))

# Floral data sets as SpatialPointsDataFrames ----------------------------------

GCFR_spp <- readOGR(glue(
  "{flora_dir}/GCFR_spp_2018-08-14"
))
SWAFR_spp <- readOGR(glue(
  "{flora_dir}/SWAFR_spp_2018-08-14"
))
