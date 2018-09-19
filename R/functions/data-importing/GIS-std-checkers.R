is_all_at_std_CRS <- function(x) {
  # Check whether all rasters in a list of rasters are projected at the std CRS
  stopifnot(is.list(x))
  all(map_lgl(
    .x = x,
    .f = function(.x) proj4string(.x) == std_CRS
  ))
}
is_all_at_0.05_res <- function(x) {
  # Check whether all rasters in a list of rasters are at 0.05º resolution
  stopifnot(is.list(x))
  all(map_lgl(
    .x = x,
    .f = function(.x) round(res(.x), 2) == 0.05
  ))
}
