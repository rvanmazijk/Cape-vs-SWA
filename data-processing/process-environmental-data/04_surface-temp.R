# Processing environmental data: Land surface temperature
# MODIS processing from NASA for my two regions
# Cape vs SWA publication
# Ruan van Mazijk

# Mean annual land surface temp ------------------------------------------------

# TODO: where are the original MODIS rasters before mean below?

# Quarterly-derived bioclimatic variables --------------------------------------

# .... Import monthly LST stacks -----------------------------------------------

GCFR_monthly_LST <- import_raster_stack(
    "temperature",
    "MODIS_monthly_means_GCFR_0.05_buffered.grd",
    n_bands = 12
)
proj4string(GCFR_monthly_LST) == std_CRS  # TRUE
res(GCFR_monthly_LST)
SWAFR_monthly_LST <- import_raster_stack(
    "temperature",
    "MODIS_monthly_means_SWAFR_0.05_buffered.grd",
    n_bands = 12
)
proj4string(SWAFR_monthly_LST) == std_CRS  # TRUE
res(SWAFR_monthly_LST)

# .... Make LST in the warmest quarter and coolest quarter ---------------------

GCFR_TWQ <- biovars_TWQ(GCFR_monthly_LST)
GCFR_TCQ <- biovars_TCQ(GCFR_monthly_LST)

SWAFR_TWQ <- biovars_TWQ(SWAFR_monthly_LST)
SWAFR_TCQ <- biovars_TCQ(SWAFR_monthly_LST)

# Save
writeRaster(
    GCFR_TWQ,
    here::here("data/derived-data/temperature/GCFR_TWQ_buffered.tif")
)
writeRaster(
    GCFR_TCQ,
    here::here("data/derived-data/temperature/GCFR_TCQ_buffered.tif")
)
writeRaster(
    SWAFR_TWQ,
    here::here("data/derived-data/temperature/SWAFR_TWQ_buffered.tif")
)
writeRaster(
    SWAFR_TCQ,
    here::here("data/derived-data/temperature/SWAFR_TCQ_buffered.tif")
)
