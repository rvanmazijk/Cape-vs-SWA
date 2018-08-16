# Import environmental data
# Cape vs SWA publication
# Ruan van Mazijk

data_dir <- here::here("data/derived-data")

# Elevation --------------------------------------------------------------------

GCFR_elev <- raster(glue(
  "{data_dir}/elevation/elevation_GCFR_box.tif"
))
stopifnot(exprs = {
  proj4string(GCFR_elev) == std_CRS
  round(res(GCFR_elev), 2) == 0.05
})

SWAFR_elev <- raster(glue(
  "{data_dir}/elevation/elevation_SWAFR_box.tif"
))
stopifnot(exprs = {
  proj4string(SWAFR_elev) == std_CRS
  round(res(SWAFR_elev), 2) == 0.05
})

# Rainfall ---------------------------------------------------------------------

GCFR_MAP <- raster(glue(
  "{data_dir}/rainfall/MAP_GCFR_box.tif"
))
stopifnot(exprs = {
  proj4string(GCFR_MAP) == std_CRS
  round(res(GCFR_MAP), 2) == 0.05
})
GCFR_PDQ <- raster(glue(
  "{data_dir}/rainfall/GCFR_PDQ_box.tif"
))
stopifnot(exprs = {
  proj4string(GCFR_PDQ) == std_CRS
  round(res(GCFR_PDQ), 2) == 0.05
})

SWAFR_MAP <- raster(glue(
  "{data_dir}/rainfall/MAP_SWAFR_box.tif"
))
stopifnot(exprs = {
  proj4string(SWAFR_MAP) == std_CRS
  round(res(SWAFR_MAP), 2) == 0.05
})
SWAFR_PDQ <- raster(glue(
  "{data_dir}/rainfall/SWAFR_PDQ_box.tif"
))
stopifnot(exprs = {
  proj4string(GCFR_PDQ) == std_CRS
  round(res(GCFR_PDQ), 2) == 0.05
})


# Land surface temperature -----------------------------------------------------

GCFR_MLST <- raster(glue(
  "{data_dir}/temperature/MODIS_annual_mean_GCFR_0.05_buffered.grd"
))
stopifnot(exprs = {
  proj4string(GCFR_MLST) == std_CRS
  round(res(GCFR_MLST), 2) == 0.05
})

SWAFR_MLST <- raster(glue(
  "{data_dir}/temperature/MODIS_annual_mean_SWAFR_0.05_buffered.grd"
))
stopifnot(exprs = {
  proj4string(SWAFR_MLST) == std_CRS
  round(res(SWAFR_MLST), 2) == 0.05
})

# NDVI -------------------------------------------------------------------------

GCFR_NDVI <- raster(glue(
  "{data_dir}/NDVI/GCFR_NDVI.tif"
))
stopifnot(exprs  = {
  proj4string(GCFR_NDVI) == std_CRS
  round(res(GCFR_NDVI), 2) == 0.05
})

SWAFR_NDVI <- raster(glue(
  "{data_dir}/NDVI/SWAFR_NDVI.tif"
))
stopifnot(exprs = {
  proj4string(SWAFR_NDVI) == std_CRS
  round(res(SWAFR_NDVI), 2) == 0.05
})

# Soil -------------------------------------------------------------------------

GCFR_soils <- stack_soils("GCFR")
stopifnot(exprs = {
  proj4string(GCFR_soils) == std_CRS
  round(res(GCFR_soils), 2) == 0.05
})
# Re-crop to box, because when read-in the extent pops back global
GCFR_soils %<>% crop(GCFR_box)

SWAFR_soils <- stack_soils("SWAFR")
stopifnot(exprs = {
  proj4string(SWAFR_soils) == std_CRS
  round(res(SWAFR_soils), 2) == 0.05
})
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
  map(crop, GCFR_variables[[4]]) %>%  # Choose a layer with the cleanest extent
  map(mask, GCFR_border_buffered)
SWAFR_variables %<>%
  map(crop, SWAFR_variables[[4]]) %>%
  map(mask, SWAFR_border_buffered)
names(GCFR_variables) <- var_names
names(SWAFR_variables) <- var_names

# Tidy up
rm(
  GCFR_elev,
  GCFR_MAP,
  GCFR_PDQ,
  GCFR_MLST,
  GCFR_NDVI,
  GCFR_soils,
  SWAFR_elev,
  SWAFR_MAP,
  SWAFR_PDQ,
  SWAFR_MLST,
  SWAFR_NDVI,
  SWAFR_soils
)
