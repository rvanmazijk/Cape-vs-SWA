import_environmental_data <- function(data_dir = here("data/derived-data")) {
  # Read in and assign all data objects to global environment

  import_region_polygons()

  # Elevation ------------------------------------------------------------------

  GCFR_elev <- raster(glue(
    "{data_dir}/elevation/elevation_GCFR_box.tif"
  ))
  SWAFR_elev <- raster(glue(
    "{data_dir}/elevation/elevation_SWAFR_box.tif"
  ))

  # Rainfall -------------------------------------------------------------------

  GCFR_MAP <- raster(glue(
    "{data_dir}/rainfall/MAP_GCFR_box.tif"
  ))
  GCFR_PDQ <- raster(glue(
    "{data_dir}/rainfall/GCFR_PDQ_box.tif"
  ))
  SWAFR_MAP <- raster(glue(
    "{data_dir}/rainfall/MAP_SWAFR_box.tif"
  ))
  SWAFR_PDQ <- raster(glue(
    "{data_dir}/rainfall/SWAFR_PDQ_box.tif"
  ))

  # Land surface temperature ---------------------------------------------------

  GCFR_MLST <- raster(glue(
    "{data_dir}/temperature/MODIS_annual_mean_GCFR_0.05_buffered.grd"
  ))
  SWAFR_MLST <- raster(glue(
    "{data_dir}/temperature/MODIS_annual_mean_SWAFR_0.05_buffered.grd"
  ))

  # NDVI -----------------------------------------------------------------------

  GCFR_NDVI <- raster(glue(
    "{data_dir}/NDVI/GCFR_NDVI.tif"
  ))
  SWAFR_NDVI <- raster(glue(
    "{data_dir}/NDVI/SWAFR_NDVI.tif"
  ))

  # Soil -----------------------------------------------------------------------

  GCFR_soils <- stack_soils("GCFR")
  SWAFR_soils <- stack_soils("SWAFR")
  # Re-crop to box, because when read-in the extent pops back global
  GCFR_soils %<>% crop(GCFR_box)
  SWAFR_soils %<>% crop(SWAFR_box)

  # All variables together -----------------------------------------------------

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
  #if (!is_all_at_std_CRS(GCFR_variables)) {
  #  warning(glue(
  #    "GCFR environmental data not all at {std_CRS}"
  #  ))
  #}
  #if (!is_all_at_0.05_res(GCFR_variables)) {
  #  warning(glue(
  #    "GCFR environmental data not all at 0.05º resolution"
  #  ))
  #}

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
  #if (!is_all_at_std_CRS(SWAFR_variables)) {
  #  warning(glue(
  #    "SWAFR environmental data not all at {std_CRS}"
  #  ))
  #}
  #if (!is_all_at_0.05_res(SWAFR_variables)) {
  #  warning(glue(
  #    "SWAFR environmental data not all at 0.05º resolution"
  #  ))
  #}

  GCFR_variables %<>%
    map(crop, GCFR_variables[[4]]) %>%  # Choose a layer with the cleanest extent
    map(mask, GCFR_border_buffered)
  SWAFR_variables %<>%
    map(crop, SWAFR_variables[[4]]) %>%
    map(mask, SWAFR_border_buffered)
  names(GCFR_variables) <- var_names
  names(SWAFR_variables) <- var_names

  # Return by assigning final lists of variables to global environment
  assign_global(GCFR_variables)
  assign_global(SWAFR_variables)

}
