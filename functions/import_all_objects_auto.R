import_all_objects_auto <- function(path) {
  files <- list.files(path, full.names = TRUE)
  if (length(files) == 0) {
    message("Empty folder")
  }
  for (file in files) {
    if (str_detect(file, "\\.csv")) {
      assign(
        # Trim path to only filename, w/o file-extention
        x = str_remove(str_extract(file, "[^\\/]+\\.csv"), "\\.csv"),
        value = read_csv(file),
        envir = .GlobalEnv
      )
    } else if (str_detect(file, "\\.RDS")) {
      assign(
        x = str_remove(str_extract(file, "[^\\/]+\\.RDS"), "\\.RDS"),
        value = read_rds(file),
        envir = .GlobalEnv
      )
    } else {
      print(glue("{file} neither a CSV nor RDS!"))
    }
  }
}
