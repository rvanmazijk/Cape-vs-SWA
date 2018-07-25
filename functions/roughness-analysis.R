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
prep_layer2 <- function(x, resolution) {
  x %>%
    aggregate(fact = resolution / 0.05) %>%
    focal_sd() %>%
    getValues()
}

# Bootstrap sampler
bootstrap_sample <- function(x, n = 1000, quietly = FALSE, ...) {
  if (!quietly) {
    print(glue("Taking {n_samples} bootstrap samples of size {length(x)}"))
    pb <- txtProgressBar(0, n)
  }
  samples <- matrix(nrow = n, ncol = size)
  for (i in 1:n) {
    samples[i, ] <- sample(x, size = length(x), replace = TRUE)
    if (!quietly) {
      setTxtProgressBar(pb, i)
    }
  }
  if (!quietly) {
    close(pb)
    print(glue("Done"))
  }
  samples
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

# Alternate version for bootstrapped U and CLES
compare_roughness_bootstrapped <- function(x, y,
                                           resolution, n_samples,
                                           force_mann_whitney_u,
                                           quietly = FALSE, ...) {
  if (!quietly) {
    print(glue("Comparing x and y at resolution = {resolution}"))
    print(glue("Prepping layers"))
    pb <- txtProgressBar(0, n_samples + 2)
  }
  x %<>%
    na.omit() %>%
    prep_layer2(resolution = resolution) %>%
    sample_layer(n_samples, quietly = TRUE)
  if (!quietly) {
    setTxtProgressBar(pb, 1)
    print(glue("Prepped layer x"))
  }
  y %<>%
    na.omit() %>%
    prep_layer2(resolution = resolution) %>%
    sample_layer(n_samples, quietly = TRUE)
  if (!quietly) {
    setTxtProgressBar(pb, 2)
    print(glue("Prepped layer y"))
    print(glue("{n_samples} bootstrap samples of both x and y taken"))
    print(glue("Running Mann-Whitney U tests and CLES on samples"))
  }
  tests <- vector("list", length = n_samples)
  for (i in 1:n_samples) {
    test <- compare_samples(
      x[i, ], y[i, ],
      "two.sided",
      force_mann_whitney_u = force_mann_whitney_u
    )
    test <- broom::tidy(test$test)
    CLES <- canprot::CLES(na.omit(x[i, ]), na.omit(y[i, ]))
    tests[[i]] <- cbind(test, CLES = CLES)
    if (!quietly) {
      setTxtProgressBar(pb, i + 2)
    }
  }
  if (!quietly) {
    print(glue("Done"))
  }
  tests
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
