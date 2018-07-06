# Import region polygons
# Cape vs SWA publication
# Ruan van Mazijk

# GCFR -------------------------------------------------------------------------

GCFR_border <- readOGR(here::here("data/derived-data/borders/GCFR_border/"))
stopifnot(proj4string(GCFR_border) == std_CRS)
GCFR_border_buffered <- readOGR(here::here("data/derived-data/borders/GCFR_border_buffered/"))
GCFR_box <- readOGR(here::here("data/derived-data/borders/GCFR_box/"))
GCFR_QDS <- readOGR(here::here("data/derived-data/borders/GCFR_QDS/"))

# SWAFR ------------------------------------------------------------------------

SWAFR_border <- readOGR(here::here("data/derived-data/borders/SWBP_Mike-Cramer/"))
stopifnot(proj4string(SWAFR_border) == std_CRS)
SWAFR_border_buffered <- readOGR(here::here("data/derived-data/borders/SWAFR_border_buffered/"))
SWAFR_box <- readOGR(here::here("data/derived-data/borders/SWAFR_box/"))
SWAFR_QDS <- readOGR(here::here("data/derived-data/borders/SWAFR_QDS/"))
