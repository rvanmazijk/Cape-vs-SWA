#' Check if folders are empty
#'
#' @param ... Character, folder path names
#' @param ignore Character, file extensions to ignore in the check
#'
#' @return A logical vector
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

#' Import all CSV and RDS files into R
#'
#' @description Assigns objects into the global environment with
#'     the name of the file but without the file extension
#'
#' @param path Character, path to folder with objects
#' @param ignore_RDS Logical, whether or not RDS files are to be excluded.
#'     Useful if RDS files in your path are known to be large.
#' @param ... Other parametes to pass to \code{folder_is_empty}
import_objects <- function(path, ignore_RDS = FALSE,
                           ignore = ".R") {
  files <- list.files(
    path,
    pattern = glue::glue("[^\\{ignore}]$"),
    full.names = TRUE
  )
  if (folder_is_empty(path, ignore = ignore)) {
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

#' Source a given R script if it has yet to produce its outputs
#'
#' @param output_path Character, the folder path where the R script's
#'     outputs should be
#' @param source_path Character, path the to R script
#' @param import Logical, whether or not to automatically also
#' @param ... Other parameters to pass to \code{folder_is_empty()} and
#'     \code{import_objects()}
#'     import the outputs after sourcing
source_if_needed <- function(output_path, source_path,
                             import = TRUE,
                             ignore = ".R", ignore_RDS = FALSE) {
  if (folder_is_empty(output_path, ignore = ignore)) {
    source(source_path)
  }
  if (import) {
    import_objects(output_path, ignore_RDS = ignore_RDS)
  }
}

#' Create a RasterStack of all the soil variables together
#'
#' @param region Character, which region (GCFR or SWAFR) to search for
#' @param variables Character, name of soil variable
#'     (permissable values in function head) (defaults to all)
#'
#' @return A RasterStack of all your soil variables
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
