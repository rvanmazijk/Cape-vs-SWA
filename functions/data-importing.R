folder_is_empty <- function(..., ignore = ".R") {
  paths <- c(...)
  out <- vector(length = length(paths))
  for (i in seq_along(paths)) {
    path_exists <- file.exists(paths[[i]])
    files_in_path <- list.files(
      paths[[i]],
      pattern = glue::glue("[^\\{ignore}]$")
    )
    out[[i]] <- (!path_exists) & (length(files_in_path) == 0)
  }
  out
}

import_all_objects_auto <- function(path, ignore_RDS = FALSE) {
  files <- list.files(path, full.names = TRUE)
  if (folder_is_empty(path, ignore = ".R")) {
    message("No objects in folder")
  } else {
    for (file in files) {
      object_name <- xfun::sans_ext(basename(file))
      ext <- tolower(xfun::file_ext(file))
      object <- switch(ext,
        "csv" = readr::read_csv(file),
        "rds" = if (!ignore_RDS) readr::read_rds(file) else next
      )
      assign(object_name, object, envir = .GlobalEnv)
    }
  }
}

source_if_needed <- function(output_path, source_path, import = TRUE) {
  if (folder_is_empty(output_path)) {
    source(source_path)
  }
  if (import) {
    import_all_objects_auto(output_path)
  }
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
  # Creates a stack of all the soil variables together
  soils <- raster::stack()
  for (variable in variables) {
    x <- raster::raster(here::here(
      "data/derived-data/soils/",
      glue("{region}_{region}_{variable}_M_250m_std_CRS_0.05_0.05.tif")
    ))
    soils <- raster::stack(soils, x)
  }
  soils
}
