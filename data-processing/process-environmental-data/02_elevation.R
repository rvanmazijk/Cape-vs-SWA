# Processing environmental data: Elevation
# DEMs processing from SRTM for my two regions
# Cape vs SWA publication
# Ruan van Mazijk

# GCFR -------------------------------------------------------------------------

# Import SRTM raster
SRTM_ZA <- raster::getData(
  "alt",
  country = "ZAF",
  path = paste0(giswd, "SRTM/")
)[[1]]

# Reproject to std_CRS
SRTM_ZA %<>% projectRaster(crs = std_CRS)
proj4string(SRTM_ZA)

# Crop-mask to GCFR_box
GCFR_box <- readRDS(here::here("data/derived-data/borders/GCFR_box.rds"))
SRTM_ZA %<>%
  crop(GCFR_box) %>%
  mask(GCFR_box)

# Resample to 0.05deg resolution
SRTM_ZA_0.05 <- raster(res = 0.05, crs = std_CRS)
SRTM_ZA %<>% resample(SRTM_ZA_0.05, method = "bilinear")

# Save
writeRaster(
  SRTM_ZA,
  overwrite = TRUE,
  here::here("data/derived-data/elevation/elevation_GCFR_box.tiff")
)

# SWAFR ------------------------------------------------------------------------

# Import SRTM raster
SRTM_AU <- raster::getData(
  "alt",
  country = "AUS",
  path = paste0(giswd, "SRTM/")
)[[1]]

# Reproject to std_CRS
SRTM_AU %<>% projectRaster(crs = std_CRS)
proj4string(SRTM_AU)

# Crop-mask to SWAFR_box
SWAFR_box <- readRDS(here::here("data/derived-data/borders/SWAFR_box.rds"))
SRTM_AU %<>% crop(SWAFR_box) %>% mask(SWAFR_box)

# Resample to 0.05deg resolution
SRTM_AU_0.05 <- raster(res = 0.05, crs = std_CRS)
SRTM_AU %<>% resample(SRTM_AU_0.05, method = "bilinear")

# Save
writeRaster(
  SRTM_AU,
  overwrite = TRUE,
  here::here("data/derived-data/elevation/elevation_SWAFR_box.tiff")
)
