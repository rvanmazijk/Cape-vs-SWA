# Define roughness **here** as the mean absolute difference of a focal cell's
# value (in a 3 x 3 neighbourhood) to it's ca. 8 neighbours
roughness <- function(x, ...) {
  focal(
    x = x,
    w = matrix(1, nrow = 3, ncol = 3),
    function(x, ...) {
      focal_cell <- x[[5]]
      if (is.na(focal_cell) | is.nan(focal_cell)) {
        return(NA)
      } else {
        x <- x[!is.na(x) & !is.nan(x)]
        diffs <- vector(length = length(x))
        for (i in seq_along(diffs)) {
          diffs[[i]] <-
            if (!is.na(x[[i]]) & !is.nan(x[[i]])) {
              abs(focal_cell - x[[i]])
            } else if (x[[i]] == focal_cell) {
              NA
            }
        }
        mean(diffs, na.rm = TRUE)
      }
    }
  )
}

# Define roughness **here** as the mean of sub-cells' mean absolute differences
# from their con-cellulars (i.e. & e.g.: other QDS in an HDS)
roughness_cells <- function(x) {
  out <- vector(length = length(x))
  for (i in seq_along(x)) {
    out[[i]] <- mean(abs(x[i] - x[-i]))
  }
  mean(out)
}
