stack_soils <- function(region = c("GCFR", "SWAFR"),
                        variables = c("CECSOL",
                                      "BLDFIE",
                                      "CLYPPT",
                                      "CRFVOL",
                                      "OCDENS",
                                      "PHIKCL",
                                      "SLTPPT",
                                      "SNDPPT",
                                      "AWCh1")) {
    # Creates a stack of all the soil variables together
    soils <- raster::stack()
    for (variable in variables) {
        x <- raster::raster(here::here(
            "data/derived-data/soils/",
            glue("{region}_{region}_{variable}_M_250m_std_CRS_0.05_0.05.tif")
        ))
        soils <- raster::stack(soils, x)
    }
    return(soils)
}
