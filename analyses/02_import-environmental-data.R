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

GCFR_soils <- stack_soils(
    region = "GCFR",
    regiondir = paste(sep = "\\",
        giswd,
        "SoilGrids250m",
        "GCFR"
    )
)
stopifnot(
    (proj4string(GCFR_soils) == std_CRS) ||
    (round(res(GCFR_soils), 2) == 0.05)
)
# Re-crop to box, because when read-in the extent pops back global
GCFR_soils %<>% crop(GCFR_box)

SWAFR_soils <- stack_soils(
    region = "SWAFR",
    regiondir = paste(sep = "\\",
        giswd,
        "SoilGrids250m",
        "SWAFR"
    )
)
stopifnot(
    (proj4string(SWAFR_soils) == std_CRS) ||
    (round(res(SWAFR_soils), 2) == 0.05)
)
SWAFR_soils %<>% crop(SWAFR_box)

# Stack all variables together
# TODO
