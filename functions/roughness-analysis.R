#' Implement our roughness function (equation in manuscript)
#'
#' @param x RasterLayer
#' @param ...
#'
#' @return A RasterLayer
focal_sd <- function(x, ...) {focal(x = x,
                                    w = matrix(1, nrow = 3, ncol = 3),
                                    function(x, ...) {
  focal_cell <- x[[5]]
  if (is.na(focal_cell) | is.nan(focal_cell)) return(NA)
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
})}
# TODO: rename as "roughness()"

#' Aggregate a layer to 0.05º, run \code{focal_sd()}
#'
#' @description And sub-sample cells if need be
#'
#' @param x RasterLayer
#' @param ...
#'
#' @return A data-frame
prep_layer <- function(x, ...) {
  x %<>%
    aggregate(fact = resolution / 0.05) %>%
    focal_sd() %>%
    getValues()
  if (resolution == 0.05) {
    x %<>% base::sample(size = 5000)  # max n Wilcox test accepts
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
  x %<>%
    na.omit() %>%
    prep_layer()
  y %<>%
    na.omit() %>%
    prep_layer()
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

# Jackknife-version of analysis
prep_layer2 <- function(x, resolution) {
  x %>%
    aggregate(fact = resolution / 0.05) %>%
    focal_sd() %>%
    getValues()
}
jackknife_sample <- function(x, size, n) {
  print(glue(
    "Taking {n} jackknife-samples of size {size}"
  ))
  return(t(replicate(n, {
    sample(x, size, replace = FALSE)
  })))
  print(glue(
    "Done"
  ))
}
CLES_jackknife <- function(x, y, n, size) {
  pw <- matrix(nrow = length(rows), ncol = length(cols))
  rownames(pw) <- x
  colnames(pw) <- y
  pw_comparisons <- pw
  for (i in 1:nrow(pw)) {
    for (j in 1:ncol(pw)) {
      pw_comparisons[i, j] <- rownames(pw)[[i]] < colnames(pw)[[j]]
    }
  }
  CLES_values <- vector(length = nrow(pw) * ncol(pw))
  # Jackknife-sample the pairwise matrix
  for (n in 1:1000) {
    rows <- sample(1:nrow(pw), size, replace = FALSE)
    cols <- sample(1:ncol(pw), size, replace = FALSE)
    jackknifed_pw <- pw_comparisons[rows, cols]
    jackknifed_pw %<>% as.vector()
    x_gt_y <- sum(jackknifed_pw, na.rm = TRUE)
    total <- length(jackknifed_pw)
    CLES_values[[n]] <- x_gt_y / total
  }
  CLES_values
}
# E.g.:
#  CLES_jackknife(
#    SWA, Cape,
#    n = 1000,
#    size = length(SWA) * length(GCFR_3QDS)
#  )
#
