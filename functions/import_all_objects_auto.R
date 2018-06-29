import_all_objects_auto <- function(path, ignore_RDS = FALSE) {
  files <- list.files(path, full.names = TRUE)
  if (length(files) == 0) {
    message("Empty folder")
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
