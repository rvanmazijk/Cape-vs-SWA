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
