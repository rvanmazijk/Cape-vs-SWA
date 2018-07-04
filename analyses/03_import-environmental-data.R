# Import environmental data
# Cape vs SWA publication
# Ruan van Mazijk

# Elevation --------------------------------------------------------------------

GCFR_elev <- raster(here::here("data/derived-data/elevation/elevation_GCFR_box.tif"))
stopifnot(
  (proj4string(GCFR_elev) == std_CRS) ||
  (round(res(GCFR_elev), 2) == 0.05)
)

SWAFR_elev <- raster(here::here("data/derived-data/elevation/elevation_SWAFR_box.tif"))
stopifnot(
  (proj4string(SWAFR_elev) == std_CRS) ||
  (round(res(SWAFR_elev), 2) == 0.05)
)

# Rainfall ---------------------------------------------------------------------

GCFR_MAP <- raster(here::here("data/derived-data/rainfall/MAP_GCFR_box.tif"))
stopifnot(
  (proj4string(GCFR_MAP) == std_CRS) ||
  (round(res(GCFR_MAP), 2) == 0.05)
)
GCFR_PWQ <- raster(here::here("data/derived-data/rainfall/GCFR_PWQ_box.tif"))
GCFR_PDQ <- raster(here::here("data/derived-data/rainfall/GCFR_PDQ_box.tif"))
GCFR_PCV <- raster(here::here("data/derived-data/rainfall/GCFR_PCV_box.tif"))

SWAFR_MAP <- raster(here::here("data/derived-data/rainfall/MAP_SWAFR_box.tif"))
stopifnot(
  (proj4string(SWAFR_MAP) == std_CRS) ||
  (round(res(SWAFR_MAP), 2) == 0.05)
)
SWAFR_PWQ <- raster(here::here("data/derived-data/rainfall/SWAFR_PWQ_box.tif"))
SWAFR_PDQ <- raster(here::here("data/derived-data/rainfall/SWAFR_PDQ_box.tif"))
SWAFR_PCV <- raster(here::here("data/derived-data/rainfall/SWAFR_PCV_box.tif"))

# Land surface temperature -----------------------------------------------------

GCFR_MLST <- raster(here::here("data/derived-data/temperature/MODIS_annual_mean_GCFR_0.05_buffered.grd"))
stopifnot(
  (proj4string(GCFR_MLST) == std_CRS) ||
  (round(res(GCFR_MLST), 2) == 0.05)
)
GCFR_TWQ <- raster(here::here("data/derived-data/temperature/GCFR_TWQ_buffered.tif"))
GCFR_TCQ <- raster(here::here("data/derived-data/temperature/GCFR_TCQ_buffered.tif"))

SWAFR_MLST <- raster(here::here("data/derived-data/temperature/MODIS_annual_mean_SWAFR_0.05_buffered.grd"))
stopifnot(
  (proj4string(SWAFR_MLST) == std_CRS) ||
  (round(res(SWAFR_MLST), 2) == 0.05)
)
SWAFR_TWQ <- raster(here::here("data/derived-data/temperature/SWAFR_TWQ_buffered.tif"))
SWAFR_TCQ <- raster(here::here("data/derived-data/temperature/SWAFR_TCQ_buffered.tif"))

# NDVI -------------------------------------------------------------------------

GCFR_NDVI <- raster(here::here("data/derived-data/NDVI/GCFR_NDVI.tif"))
stopifnot(
  (proj4string(GCFR_NDVI) == std_CRS) ||
  (round(res(GCFR_NDVI), 2) == 0.05)
)

SWAFR_NDVI <- raster(here::here("data/derived-data/NDVI/SWAFR_NDVI.tif"))
stopifnot(
  (proj4string(SWAFR_NDVI) == std_CRS) ||
  (round(res(SWAFR_NDVI), 2) == 0.05)
)

# Soil -------------------------------------------------------------------------

GCFR_soils <- stack_soils("GCFR")
stopifnot(
  (proj4string(GCFR_soils) == std_CRS) ||
  (round(res(GCFR_soils), 2) == 0.05)
)
# Re-crop to box, because when read-in the extent pops back global
GCFR_soils %<>% crop(GCFR_box)

SWAFR_soils <- stack_soils("SWAFR")
stopifnot(
  (proj4string(SWAFR_soils) == std_CRS) ||
  (round(res(SWAFR_soils), 2) == 0.05)
)
SWAFR_soils %<>% crop(SWAFR_box)

# All variables together -------------------------------------------------------

var_names <- c(
  "Elevation",
  "MAP",
  "PDQ",
  "Surface T",
  "NDVI",
  "CEC",
  "Clay",
  "Soil C",
  "pH"
)
GCFR_variables <- list(
  GCFR_elev,
  GCFR_MAP,
  GCFR_PDQ,
  GCFR_MLST,
  GCFR_NDVI,
  GCFR_soils$GCFR_GCFR_CECSOL_M_250m_std_CRS_0.05_0.05,
  GCFR_soils$GCFR_GCFR_CLYPPT_M_250m_std_CRS_0.05_0.05,
  GCFR_soils$GCFR_GCFR_OCDENS_M_250m_std_CRS_0.05_0.05,
  GCFR_soils$GCFR_GCFR_PHIKCL_M_250m_std_CRS_0.05_0.05
)
SWAFR_variables <- list(
  SWAFR_elev,
  SWAFR_MAP,
  SWAFR_PDQ,
  SWAFR_MLST,
  SWAFR_NDVI,
  SWAFR_soils$SWAFR_SWAFR_CECSOL_M_250m_std_CRS_0.05_0.05,
  SWAFR_soils$SWAFR_SWAFR_CLYPPT_M_250m_std_CRS_0.05_0.05,
  SWAFR_soils$SWAFR_SWAFR_OCDENS_M_250m_std_CRS_0.05_0.05,
  SWAFR_soils$SWAFR_SWAFR_PHIKCL_M_250m_std_CRS_0.05_0.05
)
GCFR_variables %<>%
  map(crop, GCFR_variables[[4]]) %>%
  map(mask, GCFR_border)
SWAFR_variables %<>%
  map(crop, SWAFR_variables[[4]]) %>%
  map(mask, SWAFR_border)
names(GCFR_variables) <- var_names
names(SWAFR_variables) <- var_names

GCFR_variables_QDS <- GCFR_variables %>%
  map(resample, GCFR_richness_QDS, method = "bilinear") %>%
  map(mask, GCFR_border)
GCFR_variables_HDS <- GCFR_variables %>%
  map(resample, GCFR_richness_HDS, method = "bilinear") %>%
  map(mask, GCFR_border)
GCFR_variables_3QDS <- GCFR_variables %>%
  map(resample, GCFR_richness_3QDS, method = "bilinear") %>%
  map(mask, GCFR_border)

SWAFR_variables_QDS <- SWAFR_variables %>%
  map(resample, SWAFR_richness_QDS, method = "bilinear") %>%
  map(mask, SWAFR_border)
SWAFR_variables_HDS <- SWAFR_variables %>%
  map(resample, SWAFR_richness_HDS, method = "bilinear") %>%
  map(mask, SWAFR_border)
SWAFR_variables_3QDS <- SWAFR_variables %>%
  map(resample, SWAFR_richness_3QDS, method = "bilinear") %>%
  map(mask, SWAFR_border)

# Tidy up
rm(
  GCFR_elev,
  GCFR_MAP,
  GCFR_PDQ,
  GCFR_PCV,
  GCFR_PWQ,
  GCFR_MLST,
  GCFR_TCQ,
  GCFR_TWQ,
  GCFR_NDVI,
  GCFR_soils,

  SWAFR_elev,
  SWAFR_MAP,
  SWAFR_PDQ,
  SWAFR_PCV,
  SWAFR_PWQ,
  SWAFR_MLST,
  SWAFR_TCQ,
  SWAFR_TWQ,
  SWAFR_NDVI,
  SWAFR_soils
)
