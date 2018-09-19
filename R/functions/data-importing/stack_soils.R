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
