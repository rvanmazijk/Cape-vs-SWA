# Import region polygons
# Cape vs SWA publication
# Ruan van Mazijk

# GCFR -------------------------------------------------------------------------

GCFR_border <- readRDS(here::here("data/derived-data/borders/GCFR_border.rds"))
writeOGR(
  GCFR_border,
  here::here("data/derived-data/borders/GCFR_border/"),
  layer = "LA_CURRENT",
  driver = "ESRI Shapefile"
)
stopifnot(proj4string(GCFR_border) == std_CRS)
GCFR_box <- readRDS(here::here("data/derived-data/borders/GCFR_box.rds"))
writeOGR(
  GCFR_box,
  here::here("data/derived-data/borders/GCFR_box/"),
  layer = "value",
  driver = "ESRI Shapefile"
)
GCFR_QDS <- readRDS(here::here("data/derived-data/borders/GCFR_QDS.rds"))
for (layer in names(GCFR_QDS)) {
  writeOGR(
    GCFR_QDS,
    here::here("data/derived-data/borders/GCFR_QDS/"),
    layer = layer,
    driver = "ESRI Shapefile"
  )
}

# SWAFR ------------------------------------------------------------------------

SWAFR_border <- readOGR(here::here("data/derived-data/borders/SWBP_Mike-Cramer"))
stopifnot(proj4string(SWAFR_border) == std_CRS)
SWAFR_box <- readRDS(here::here("data/derived-data/borders/SWAFR_box.rds"))
writeOGR(
  SWAFR_box,
  here::here("data/derived-data/borders/SWAFR_box/"),
  layer = "value",
  driver = "ESRI Shapefile"
)
SWAFR_QDS <- readRDS(here::here("data/derived-data/borders/SWAFR_QDS.rds"))
for (layer in names(SWAFR_QDS)) {
  writeOGR(
    SWAFR_QDS,
    here::here("data/derived-data/borders/SWAFR_QDS/"),
    layer = layer,
    driver = "ESRI Shapefile"
  )
}
