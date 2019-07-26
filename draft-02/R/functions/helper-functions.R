assign_global <- function(x) {
  # Assign a variable to the global environment,
  # not simply the parent environment (as with <<-),
  # with the same name
  assign(
    x     = deparse(substitute(x)),
    value = x,
    envir = .GlobalEnv
  )
}

import_region_polygons <- function(borders_dir =
                                     here("data/derived-data/borders")) {
  # Read in and assign all polygon objects to global environment

  # GCFR -----------------------------------------------------------------------

  GCFR_border          <- readOGR(glue("{borders_dir}/GCFR_border/"))
  GCFR_border_buffered <- readOGR(glue("{borders_dir}/GCFR_border_buffered/"))
  GCFR_box             <- readOGR(glue("{borders_dir}/GCFR_box/"))
  GCFR_QDS             <- readOGR(glue("{borders_dir}/GCFR_QDS/"))
  assign_global(GCFR_border)
  assign_global(GCFR_border_buffered)
  assign_global(GCFR_box)
  assign_global(GCFR_QDS)

  # SWAFR ----------------------------------------------------------------------

  SWAFR_border          <- readOGR(glue("{borders_dir}/SWBP_Mike-Cramer/"))
  SWAFR_border_buffered <- readOGR(glue("{borders_dir}/SWAFR_border_buffered/"))
  SWAFR_box             <- readOGR(glue("{borders_dir}/SWAFR_box/"))
  SWAFR_QDS             <- readOGR(glue("{borders_dir}/SWAFR_QDS/"))
  assign_global(SWAFR_border)
  assign_global(SWAFR_border_buffered)
  assign_global(SWAFR_box)
  assign_global(SWAFR_QDS)

  # FIXME: Why are these shapefile imports throwing non-fatal errors/warnings?
  # TODO: Add GIS-std-checkers here too
}

stack_soils <- function(region = c("GCFR", "SWAFR"),
                        variables = c("CECSOL",
                                      "BLDFIE",
                                      "CLYPPT",
                                      "CRFVOL",
                                      "OCDENS",
                                      "PHIKCL",
                                      "SLTPPT",
                                      "SNDPPT")) {
  # Create a RasterStack of all the soil variables together, after processing
  soils <- stack()
  for (variable in variables) {
    x <- raster(here(
      "data/derived-data/soils/",
      glue("{region}_{region}_{variable}_M_250m_std_CRS_0.05_0.05.tif")
    ))
    soils %<>% stack(x)
  }
  soils
}

import_environmental_data <- function(data_dir = here("data/derived-data")) {
  # Read in and assign all data objects to global environment

  import_region_polygons()

  # Elevation ------------------------------------------------------------------

  GCFR_elev  <- raster(glue("{data_dir}/elevation/elevation_GCFR_box.tif"))
  SWAFR_elev <- raster(glue("{data_dir}/elevation/elevation_SWAFR_box.tif"))

  # Rainfall -------------------------------------------------------------------

  GCFR_MAP  <- raster(glue("{data_dir}/rainfall/MAP_GCFR_box.tif"))
  GCFR_PDQ  <- raster(glue("{data_dir}/rainfall/GCFR_PDQ_box.tif"))
  SWAFR_MAP <- raster(glue("{data_dir}/rainfall/MAP_SWAFR_box.tif"))
  SWAFR_PDQ <- raster(glue("{data_dir}/rainfall/SWAFR_PDQ_box.tif"))

  # Land surface temperature ---------------------------------------------------

  GCFR_MLST  <- raster(glue(
    "{data_dir}/temperature/MODIS_annual_mean_GCFR_0.05_buffered.grd"
  ))
  SWAFR_MLST <- raster(glue(
    "{data_dir}/temperature/MODIS_annual_mean_SWAFR_0.05_buffered.grd"
  ))

  # NDVI -----------------------------------------------------------------------

  GCFR_NDVI  <- raster(glue("{data_dir}/NDVI/GCFR_NDVI.tif"))
  SWAFR_NDVI <- raster(glue("{data_dir}/NDVI/SWAFR_NDVI.tif"))

  # Soil -----------------------------------------------------------------------

  GCFR_soils  <- stack_soils("GCFR")
  SWAFR_soils <- stack_soils("SWAFR")
  # Re-crop to box, because when read-in the extent pops back global
  GCFR_soils  %<>% crop(GCFR_box)
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
    map(crop, GCFR_variables[[4]]) %>%  # [[4]] has cleanest extent
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

prompt_continue <- function() {
  continue <- readline(message(glue(
    "Only run the code below if you haven't already & saved the results to disc.
    Are you sure you want to continue? [y]
    (Press any other key to cancel.)"
  )))
  continue == "y"
}

qdgc2hdgc <- function(x) {
  # QDS -> HDS or QDS -> EDS by dropping the last letter
  substr(x, 1, nchar(x) - 1)
}

get_geocodes <- function(flora_points, QDS_polygon) {
  flora_points@data$qdgc <- over(flora_points, QDS_polygon)[[1]]
  flora_points@data$hdgc <- map_chr(flora_points@data$qdgc, ~
    .x %>%
      as.character() %>%
      qdgc2hdgc()
  )
  flora_points
}

my_AIC_table <- function(..., caption = "...") {
  AIC(...) %>%
    mutate(
      delta_AIC = AIC - min(AIC),
      w_Akaike  = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC))
    ) %>%
    mutate(model = c("No region", "Add. region", "Int. region")) %>%
    mutate_if(is.numeric, ~format(round(.x, digits = 3), nsmall = 3)) %>%
    dplyr::select(model, AIC, delta_AIC, w_Akaike) #%>%
    #knitr::kable(
    #  caption = caption,
    #  col.names = c(
    #    "Model",
    #    "$AIC$",
    #    "$\\Delta AIC$",
    #    "$w_{\\mathrm{Akaike}}$"
    #  ),
    #  align = c("lrrr")
    #)
}
