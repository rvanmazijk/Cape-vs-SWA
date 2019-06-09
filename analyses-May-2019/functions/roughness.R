# Define roughness **here** as the mean absolute difference of a focal cell's
# value (in a 3 x 3 neighbourhood) to it's ca. 8 neighbours
roughness <- function(x) {
  focal(x, matrix(1, nrow = 3, ncol = 3), function(x) {
    focal_cell <- x[5]
    neighbour_cells <- x[
      !is.na(x) &
      !is.nan(x) &
      x != focal_cell
    ]
    ifelse(!is.na(focal_cell) & !is.nan(focal_cell),
      mean(abs(focal_cell - neighbour_cells)),
      NA
    )
  })
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
