#' Implement our roughness function (equation in manuscript)
#'
#' @param x RasterLayer
#' @param ...
#'
#' @return A RasterLayer
focal_sd <- function(x, ...) {
  focal(
    x = x,
    w = matrix(1, nrow = 3, ncol = 3),
    function(x, ...) {
      diffs <- vector(length = 8)
      diffs[1] <- (x[5] - x[1]) ^ 2
      diffs[2] <- (x[5] - x[2]) ^ 2
      diffs[3] <- (x[5] - x[3]) ^ 2
      diffs[4] <- (x[5] - x[4]) ^ 2
      diffs[5] <- (x[5] - x[6]) ^ 2
      diffs[6] <- (x[5] - x[7]) ^ 2
      diffs[7] <- (x[5] - x[8]) ^ 2
      diffs[8] <- (x[5] - x[9]) ^ 2
      sqrt(mean(diffs))
    }
  )
}

#' Aggregate a layer to 0.05º, run \code{focal_sd()}
#'
#' @description And sub-sample cells if need be
#'
#' @param x RasterLayer
#' @param ...
#'
#' @return A RasterLayer
prep_layer <- function(x, ...) {
  x %<>%
    aggregate(fact = resolution / 0.05) %>%
    focal_sd() %>%
    `[`()
  if (resolution == 0.05) {
    x %<>% base::sample(size = 5000)  # maxi sample size Wilcox test accepts
  }
  x
}

#' Title
#'
#' @param x
#' @param y
#' @param resolution
#' @param raw
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
compare_roughness <- function(x, y, resolution, raw = FALSE, ...) {
  x %<>% prep_layer()
  y %<>% prep_layer()
  test <- compare_samples(x, y, "two.sided", ...)$test
  if (raw) {
    return(test)
  } else {
    return(broom::tidy(test))
  }
}

#' Title
#'
#' @param x
#' @param y
#' @param resolution
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
describe_roughness <- function(x, y, resolution, ...) {
  x %<>% prep_layer()
  y %<>% prep_layer()
  compare_samples(x, y, "two.sided", ...)$assumptions
}

IQ99R <- function(x) quantile(x, 0.99) - quantile(x, 0.01)
IQ95R <- function(x) quantile(x, 0.95) - quantile(x, 0.05)
