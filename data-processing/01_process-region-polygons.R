# Regional border polygons, buffered and unbuffered, and the QDGC grid poly
# Cape vs SWA publication
# Ruan van Mazijk

# GCFR border ------------------------------------------------------------------

# From SANParks, via SANBI-GIS portal

GCFR_border <-
    readOGR(here::here("data/raw-data/borders/CCAB_current_biome"),
            layer = "Current_biome") %>%
    subset(!is.na(LA_CURRENT)) %>%  # removes this one pesky NA
    subset(LA_CURRENT %in% c("Fynbos", "Succulent Karoo"))
saveRDS(GCFR_border, here::here("data/derived-data/borders/GCFR_border.rds"))

GCFR_box <-
    matrix(nrow = 4, ncol = 2, data = c(
        # From "blocks" used to download SoilGrids250m
        16,   16,  31,  31,
        -27, -37, -37, -27)) %>%
    Polygon(hole = TRUE) %>%
    list() %>%
    Polygons(ID = 1) %>%
    list() %>%
    SpatialPolygons(proj4string = crs(std_CRS)) %>%
    SpatialPolygonsDataFrame(data = data.frame(value = 1))
saveRDS(GCFR_box, here::here("data/derived-data/borders/GCFR_box.rds"))

# SWAFR border -----------------------------------------------------------------

# From Mike
# TODO: get exact details of how he cobbled this together

SWAFR_border <- readOGR(here::here("data/derived-data/borders/SWBP_Mike-Cramer"))

SWAFR_box <-
    matrix(nrow = 4, ncol = 2, data = c(
        # From "blocks" used to download SoilGrids250m
        112, 112, 127, 127,
        -25, -36, -36, -25)) %>%
    Polygon(hole = TRUE) %>%
    list() %>%
    Polygons(ID = 1) %>%
    list() %>%
    SpatialPolygons(proj4string = crs(std_CRS)) %>%
    SpatialPolygonsDataFrame(data = data.frame(value = 1))
saveRDS(SWAFR_box, here::here("data/derived-data/borders/SWAFR_box.rds"))

# Regional Larsen QDS grids ----------------------------------------------------

# .... Import shapefiles -------------------------------------------------------

# GCFR
GCFR_QDS <-
    readOGR(here::here("data/raw-data/borders/qdgc_zaf"),
            layer = "qdgc_02_zaf") %>%
    crop(GCFR_box)
saveRDS(GCFR_QDS, here::here("data/derived-data/borders/GCFR_QDS.rds"))
GCFR_HDS <-
    readOGR(here::here("data/raw-data/borders/qdgc_zaf"),
            layer = "qdgc_01_zaf") %>%
    crop(GCFR_box)
saveRDS(GCFR_HDS, here::here("data/derived-data/Borders/GCFR_HDS.rds"))

# SWAFR
SWAFR_QDS <-
    readOGR(here::here("data/raw-data/borders/qdgc_aus"),
            layer = "qdgc_02_aus") %>%
    crop(SWAFR_box)
saveRDS(SWAFR_HDS, here::here("data/derived-data/borders/SWAFR_QDS.rds"))
SWAFR_HDS <-
    readOGR(here::here("data/raw-data/borders/qdgc_aus"),
            layer = "qdgc_01_aus") %>%
    crop(SWAFR_box)
saveRDS(SWAFR_HDS, here::here("data/derived-data/borders/SWAFR_HDS.rds"))

# .... Convert shapefiles to rasters -------------------------------------------

# GCFR
GCFR_QDS_raster <- QDSpolydf2raster(
    GCFR_QDS,
    region_border = GCFR_box
)
writeRaster(
    GCFR_QDS_raster,
    here::here("data/derived-data/borders/GCFR_QDS_raster.tif")
)
GCFR_HDS_raster <- QDSpolydf2raster(
    GCFR_HDS,
    region_border = GCFR_box,
    resolution = 0.5
)
writeRaster(
    GCFR_HDS_raster,
    here::here("data/derived-data/borders/GCFR_HDS_raster.tif")
)
GCFR_3QDS_raster <-
    GCFR_QDS@polygons %>%
    SpatialPolygons(proj4string = CRS(std_CRS)) %>%
    rasterize(
        raster(resolution = 0.75, crs = std_CRS) %>%
            crop(GCFR_box) %>%
            mask(GCFR_box)
    )
writeRaster(
    GCFR_3QDS_raster,
    here::here("data/derived-data/borders/GCFR_3QHDS_raster.tif")
)
#plot(GCFR_3QDS_raster)
#plot(GCFR_QDS, add = TRUE)
#plot(extent(GCFR_3QDS_raster))
#plot(extent(GCFR_QDS_raster), add = TRUE)


# SWAFR
SWAFR_QDS_raster <- QDSpolydf2raster(
    SWAFR_QDS,
    region_border = SWAFR_box
)
writeRaster(
    SWAFR_QDS_raster,
    here::here("data/derived-data/borders/SWAFR_QDS_raster.tif")
)
SWAFR_HDS_raster <- QDSpolydf2raster(
    SWAFR_HDS,
    region_border = SWAFR_box,
    resolution = 0.5
)
writeRaster(
    SWAFR_HDS_raster,
    here::here("data/derived-data/borders/SWAFR_HDS_raster.tif")
)
SWAFR_3QDS_raster <- rasterize(
    SpatialPolygons(SWAFR_QDS@polygons, proj4string = CRS(std_CRS)),
    raster(resolution = 0.75, crs = std_CRS) %>%
        crop(SWAFR_box) %>%
        mask(SWAFR_box)
)
writeRaster(
    SWAFR_3QDS_raster,
    here::here("data/derived-data/borders/SWAFR_3QHDS_raster.tif")
)
