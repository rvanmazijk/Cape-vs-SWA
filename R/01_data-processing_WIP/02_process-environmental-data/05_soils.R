# Processing environmental data: Soil
# Downloading SoilGrids250m GEOTIFFs in 5x5 degree blocks, and subsequent processing
# Cape vs SWA publication
# Ruan van Mazijk

# Download variables in blocks from <soilgrids.org> ----------------------------
# See READMEs and paper notes for explanation of the "blocks"

soil_variables <- c(
  "CECSOL",  # Cation exchange capacity
  "BLDFIE",  # Soil bulk density
  "CLYPPT",  # Clay %
  "CRFVOL",  # Clay volumetric fraction
  "OCDENS",  # Soil organic carbon
  "PHIKCL",  # Soil pH (KCl)
  "SLTPPT",  # Silt %
  "SNDPPT"   # Sand %
)

# .... GCFR --------------------------------------------------------------------

GCFR_blocks <- list(
  c(16, 21, -32, -27),
  c(16, 21, -37, -32),
  c(21, 26, -37, -32),
  c(26, 31, -37, -32),
  c(26, 31, -32, -27),
  c(21, 26, -32, -27)
)

# Below, do each soil depth layer (e.g. sl1, sl2, etc.) in a loop,
# within a loop for each soil variable (CECSOL, BLDFIE, etc.).
# Note, internet connection may interrupt the loops,
# so try each depth or variable in sequence in console if need be.

for (soil_variable in soil_variables) {
  download_soil(soil_variable,
    depths = c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"),
    blocks = GCFR_blocks,
    destdir = paste(sep = "/",
      giswd,
      "SoilGrids250m",
      "GCFR"
    )
  )
}

# .... SWAFR -------------------------------------------------------------------

SWAFR_blocks <- list(
  c(112, 117, -30, -25),
  c(112, 117, -35, -30),
  c(117, 122, -35, -30),
  c(122, 127, -35, -30),
  c(117, 122, -30, -25),
  c(117, 122, -36, -35)  # Note how weird block 06 is... lol
)

for (soil_variable in soil_variables) {
  download_soil(soil_variable,
    depths = c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"),
    blocks = SWAFR_blocks,
    destdir = paste(sep = "/",
      giswd,
      "SoilGrids250m",
      "SWAFR"
    )
  )
}

# Stitch together the blocks ---------------------------------------------------

# .... GCFR --------------------------------------------------------------------

merge_soils(
  variables = c(
    "CECSOL",
    "BLDFIE",
    "CLYPPT",
    "CRFVOL",
    "OCDENS",
    "PHIKCL",
    "SLTPPT",
    "SNDPPT"
  ),
  depths = c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"),
  do_parallel = TRUE,
  sourcedir = paste(sep = "/",
    giswd,
    "SoilGrids250m",
    "GCFR"
  )
)

# .... SWAFR -------------------------------------------------------------------

merge_soils(
  variables = c(
    "CECSOL",
    "BLDFIE",
    "CLYPPT",
    "CRFVOL",
    "OCDENS",
    "PHIKCL",
    "SLTPPT",
    "SNDPPT"
  ),
  depths = c("sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7"),
  do_parallel = TRUE,
  sourcedir = paste(sep = "/",
    giswd,
    "SoilGrids250m",
    "SWAFR"
  )
)

# Depth-weighted averages ------------------------------------------------------

# Now know that trapezoidal rule is basically the same as the weighted avg approach!
# Same result, framed more geometrically and generally.
# E.g.
if (FALSE) {
  weighted.mean(
    brick(lapply(
      list_shape[, 1],
      FUN = resample_raster
    )),
    w = depth_weightings,
    na.rm = TRUE
  )
}

# .... GCFR --------------------------------------------------------------------

integrate_soils(
  variables = c(
    "CECSOL",
    "BLDFIE",
    "CLYPPT",
    "CRFVOL",
    "OCDENS",
    "PHIKCL",
    "SLTPPT",
    "SNDPPT",
    "AWCh1"
  ),
  do_parallel = TRUE,
  sourcedir = paste(sep = "/",
    giswd,
    "SoilGrids250m",
    "GCFR"
  )
)

# .... SWAFR -------------------------------------------------------------------

integrate_soils(
  variables = c(
    "CECSOL",
    "BLDFIE",
    "CLYPPT",
    "CRFVOL",
    "OCDENS",
    "PHIKCL",
    "SLTPPT",
    "SNDPPT",
    "AWCh1"
  ),
  do_parallel = TRUE,
  sourcedir = paste(sep = "/",
    giswd,
    "SoilGrids250m",
    "SWAFR"
  )
)

# Trim -Inf etc. ---------------------------------------------------------------

# TODO: need this?
# TODO: do this for all other environmental vars too! consistently!

# Mask & crop to boxes ---------------------------------------------------------

GCFR_box <- readRDS(here::here("data/derived-data/borders/GCFR_box.rds"))
GCFR_soils %<>% mask(GCFR_box)
GCFR_soils %<>% crop(GCFR_box)
extent(GCFR_soils)

SWAFR_box <- readRDS(here::here("data/derived-data/borders/SWAFR_box.rds"))
SWAFR_soils %<>% mask(SWAFR_box)
SWAFR_soils %<>% crop(SWAFR_box)
extent(SWAFR_soils)

# Reproject to std_CRS ---------------------------------------------------------

GCFR_soils <- stack_soils(
  region = "GCFR",
  regiondir = paste(sep = "/",
    giswd,
    "SoilGrids250m",
    "GCFR"
  )
)
proj4string(GCFR_soils) == std_CRS  # TRUE
res(GCFR_soils)

GCFR_soils %<>% projectRaster(crs = std_CRS)
for (i in 1:9) {
  writeRaster(
    GCFR_soils[[i]],
    overwrite = TRUE,
    glue("
      {giswd}\\
      SoilGrids250m/\\
      GCFR/\\
      GCFR_{names(GCFR_soils[[i]])}_std_CRS.tif
    ")
  )
}

SWAFR_soils <- stack_soils(
  region = "SWAFR",
  regiondir = paste(sep = "/",
    giswd,
    "SoilGrids250m",
    "SWAFR"
  )
)
proj4string(SWAFR_soils) == std_CRS  # TRUE

SWAFR_soils %<>% projectRaster(crs = std_CRS)
for (i in 1:9) {
  writeRaster(
    SWAFR_soils[[i]],
    overwrite = TRUE,
    glue("
      {giswd}\\
      SoilGrids250m/\\
      SWAFR/\\
      SWAFR_{names(SWAFR_soils[[i]])}_std_CRS.tif
    ")
  )
}

# Resample to 0.05deg resolution -----------------------------------------------

GCFR_soils_0.5 <- raster(res = 0.05, crs = std_CRS)
GCFR_soils %<>% resample(GCFR_soils_0.5, method = "bilinear")
for (i in 1:9) {
  writeRaster(
    GCFR_soils[[i]],
    overwrite = TRUE,
    here::here(
      "data/derived-data/soils/",
      glue("GCFR_{names(GCFR_soils[[i]])}_0.05.tif")
    )
  )
}
res(GCFR_soils)

SWAFR_soils_0.5 <- raster(res = 0.05, crs = std_CRS)
SWAFR_soils %<>% resample(SWAFR_soils_0.5, method = "bilinear")
for (i in 1:9) {
  writeRaster(
    SWAFR_soils[[i]],
    overwrite = TRUE,
    here::here(
      "data/derived-data/soils/",
      glue("SWAFR_{names(SWAFR_soils[[i]])}_0.05.tif")
    )
  )
}
res(SWAFR_soils)
