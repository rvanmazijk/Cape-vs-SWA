# Implement our roughness function (equation in manuscript)
roughness <- function(x, ...) {

  focal(
    x = x,
    w = matrix(1, nrow = 3, ncol = 3),
    function(x, ...) {
      focal_cell <- x[[5]]
      if (is.na(focal_cell) | is.nan(focal_cell)) {
        return(NA)
      }
      x <- x[!is.na(x) & !is.nan(x)]
      diffs <- vector(length = length(x))
      for (i in seq_along(diffs)) {
        diffs[[i]] <-
          if (!is.na(x[[i]]) & !is.nan(x[[i]])) {
            (focal_cell - x[[i]]) ^ 2
          } else if (x[[i]] == focal_cell) {
            NA
          }
      }
      mean(sqrt(diffs), na.rm = TRUE)
    }
  )
}

# Aggregates a layer to the specified resolution (assuming base res is 0.05º)
# and calculates the roughness layer of that, return a vector of roughness values
get_roughness_values <- function(x, resolution) {
  x %<>%
    aggregate(fact = resolution / 0.05) %>%
    roughness() %>%
    getValues() %>%
    na.exclude()
  if (resolution == 0.05) {
    x %<>% base::sample(size = 5000)  # max n U-test accepts
  }
  x
}
