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
writeOGR(
  GCFR_border,
  here::here("data/derived-data/borders/GCFR_border/"),
  layer = "LA_CURRENT",
  driver = "ESRI Shapefile"
)

GCFR_box <-
  matrix(nrow = 4, ncol = 2,
         data = c( 16,  16,  31,  31,
                  -27, -37, -37, -27)) %>%
                  # From "blocks" used to download SoilGrids250m
  Polygon(hole = TRUE) %>%
  list() %>%
  Polygons(ID = 1) %>%
  list() %>%
  SpatialPolygons(proj4string = crs(std_CRS)) %>%
  SpatialPolygonsDataFrame(data = data.frame(value = 1))
writeOGR(
  GCFR_box,
  here::here("data/derived-data/borders/GCFR_box/"),
  layer = "value",
  driver = "ESRI Shapefile"
)

# SWAFR border -----------------------------------------------------------------

# From Mike
# TODO: get exact details of how he cobbled this together

SWAFR_border <- readOGR(here::here("data/derived-data/borders/SWBP_Mike-Cramer"))

SWAFR_box <-
  matrix(nrow = 4, ncol = 2,
         data = c(112, 112, 127, 127,
                  -25, -36, -36, -25)) %>%
                  # From "blocks" used to download SoilGrids250m
  Polygon(hole = TRUE) %>%
  list() %>%
  Polygons(ID = 1) %>%
  list() %>%
  SpatialPolygons(proj4string = crs(std_CRS)) %>%
  SpatialPolygonsDataFrame(data = data.frame(value = 1))
writeOGR(
  SWAFR_box,
  here::here("data/derived-data/borders/SWAFR_box/"),
  layer = "value",
  driver = "ESRI Shapefile"
)


# Regional Larsen QDS grids ----------------------------------------------------

# GCFR
GCFR_QDS <-
  readOGR(here::here("data/raw-data/borders/qdgc_zaf"),
          layer = "qdgc_02_zaf") %>%
  crop(GCFR_box)
for (layer in names(GCFR_QDS)) {
  writeOGR(
    GCFR_QDS,
    here::here("data/derived-data/borders/GCFR_QDS/"),
    layer = layer,
    driver = "ESRI Shapefile"
  )
}

# SWAFR
SWAFR_QDS <-
  readOGR(here::here("data/raw-data/borders/qdgc_aus"),
          layer = "qdgc_02_aus") %>%
  crop(SWAFR_box)
for (layer in names(SWAFR_QDS)) {
  writeOGR(
    SWAFR_QDS,
    here::here("data/derived-data/borders/SWAFR_QDS/"),
    layer = layer,
    driver = "ESRI Shapefile"
  )
}

