import_region_polygons <- function(borders_dir = here("data/derived-data/borders")) {
  # Read in and assign all polygon objects to global environment

  # GCFR -------------------------------------------------------------------------

  GCFR_border <- readOGR(glue(
    "{borders_dir}/GCFR_border/"
  ))
  assign_global(GCFR_border)
  GCFR_border_buffered <- readOGR(glue(
    "{borders_dir}/GCFR_border_buffered/"
  ))
  assign_global(GCFR_border_buffered)
  GCFR_box <- readOGR(glue(
    "{borders_dir}/GCFR_box/"
  ))
  assign_global(GCFR_box)
  GCFR_QDS <- readOGR(glue(
    "{borders_dir}/GCFR_QDS/"
  ))
  assign_global(GCFR_QDS)

  # SWAFR ------------------------------------------------------------------------

  SWAFR_border <- readOGR(glue(
    "{borders_dir}/SWBP_Mike-Cramer/"
  ))
  assign_global(SWAFR_border)
  SWAFR_border_buffered <- readOGR(glue(
    "{borders_dir}/SWAFR_border_buffered/"
  ))
  assign_global(SWAFR_border_buffered)
  SWAFR_box <- readOGR(glue(
    "{borders_dir}/SWAFR_box/"
  ))
  assign_global(SWAFR_box)
  SWAFR_QDS <- readOGR(glue(
    "{borders_dir}/SWAFR_QDS/"
  ))
  assign_global(SWAFR_QDS)

  # FIXME: Why are these shapefile imports throwing non-fatal errors/warnings?
  # TODO: Add GIS-std-checkers here too

}
