split_matrix <- function(x, n_rows_per_split = 1000, file = NULL) {
  # Split the matrix -----------------------------------------------------------
  n_splits <- ceiling(nrow(x) / n_rows_per_split)
  message(glue("Splitting matrix into {n_splits} sub-matrices..."))
  splits <- vector("list", length = n_splits)
  start <- 1
  for (i in seq(n_splits)) {
    end <- start + n_rows_per_split - 1
    if (end > nrow(x)) {
      end <- nrow(x)
    }
    splits[[i]] <- x[start:end, ]
    start <- end + 1
  }
  # Save the sub-matrices ------------------------------------------------------
  if (!is.null(file)) {
    if (!str_detect(file, "\\{i\\}")) {
      stop("'{i}' needed in filename to save each sub-matrix separately")
    }
    message("Saving sub-matrices...")
    for (i in seq(n_splits)) {
      write_csv(as.data.frame(splits[[i]]), glue(file))
    }
    saved <- TRUE
  }
  # Return ---------------------------------------------------------------------
  if (n_splits > 5 && saved) {
    message(glue(
      "{n_splits} sub-matrices is a lot!
      Returning summary instead"
    ))
    return(summary(splits))
  } else {
    return(splits)
  }
}
