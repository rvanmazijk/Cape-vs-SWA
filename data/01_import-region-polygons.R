# Import region polygons
# Cape vs SWA publication
# Ruan van Mazijk

borders_dir <- here::here("data/derived-data/borders")

# GCFR -------------------------------------------------------------------------

GCFR_border <-
  readOGR(glue("{borders_dir}/GCFR_border/"))
stopifnot(proj4string(GCFR_border) == std_CRS)
GCFR_border_buffered <-
  readOGR(glue("{borders_dir}/GCFR_border_buffered/"))
GCFR_box <-
  readOGR(glue("{borders_dir}/GCFR_box/"))
GCFR_QDS <-
  readOGR(glue("{borders_dir}/GCFR_QDS/"))

# SWAFR ------------------------------------------------------------------------

SWAFR_border <-
  readOGR(glue("{borders_dir}/SWBP_Mike-Cramer/"))
stopifnot(proj4string(SWAFR_border) == std_CRS)
SWAFR_border_buffered <-
  readOGR(glue("{borders_dir}/SWAFR_border_buffered/"))
SWAFR_box <-
  readOGR(glue("{borders_dir}/SWAFR_box/"))
SWAFR_QDS <-
  readOGR(glue("{borders_dir}/SWAFR_QDS/"))

# FIXME: Why are these shapefile imports throwing errors/warnings?
