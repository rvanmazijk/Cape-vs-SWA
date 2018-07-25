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

# Alternate version for bootstrapped U and CLES
compare_roughness_bootstrapped <- function(x, y,
                                           resolution, n_samples = 1000,
                                           force_mann_whitney_u,
                                           quietly = FALSE,
                                           use_disc = FALSE, ...) {
  # Define inner functions -----------------------------------------------------
  prep_and_bootstrap <- function(x, resolution, n_samples,
                                 quietly = FALSE, use_disc = FALSE,
                                 invisible = TRUE) {
    prep_layer2 <- function(x, resolution) {
      x %>%
        aggregate(fact = resolution / 0.05) %>%
        focal_sd() %>%
        getValues()
    }
    bootstrap_sample <- function(x, n = 1000, quietly = FALSE, ...) {
      if (!quietly) {
        print(glue("Taking {n_samples} bootstrap samples of size {length(x)}"))
        pb <- txtProgressBar(0, n)
      }
      samples <- matrix(nrow = n, ncol = length(x))
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
    # Orchestrate prep_layer2() & bootstrap_sample() ---------------------------
    x %<>%
      na.omit() %>%
      prep_layer2(resolution = resolution) %>%
      bootstrap_sample(n_samples, quietly = TRUE)
    if (!quietly) {
      print(glue("Bootstrap-sampled layer {name_of(x)}"))
    }
    if (use_disc) {
      path <- here::here(glue("outputs/temp/{name_of(x)}_bootstraps.csv"))
      write_csv(x, path)
      if (!quietly) {
        print(glue("Saved {name_of(x)} bootstrap samples to disc"))
      }
    }
    if (use_disc && invisible) {
      rm(x, envir = parent.frame(1))
      return(path)
    } else {
      return(x)
    }
  }
  # Bootstrap-sample x & y -----------------------------------------------------
  if (!quietly) {
    print(glue(
      "[Comparing {name_of(x)} and {name_of(y)} \\
      at resolution = {resolution}]"
    ))
    print(glue("Bootstrap-sampling layers..."))
  }
  if (use_disc) {
    x_name <- name_of(x)
    y_name <- name_of(y)
  }
  x %<>% prep_and_bootstrap(resolution, n_samples, use_disc = use_disc)
  y %<>% prep_and_bootstrap(resolution, n_samples, use_disc = use_disc)
  # Run Mann-Whitney & CLES on bootstraps --------------------------------------
  if (!quietly) {
    print(glue(
       "Taken {n_samples} bootstrap-samples of both \\
       {name_of(x)} and {name_of(y)}"
    ))
    print(glue("Running Mann-Whitney U tests and CLES on samples"))
    pb <- txtProgressBar(0, n_samples)
  }
  if (use_disc) {
    x <- read_csv(x)
    y <- read_csv(x)
    if (!quietly) {
      print(glue("Read {name_of(x)} and {name_of(y)} back from disc"))
    }
  }
  # Informatively name *_u-test.csv's
  var_name <- c(x_name, y_name) %>%
    str_extract("\\$.+$") %>%
    str_remove("\\$") %>%
    unique()
  if (length(var_name) == 1) {
    stop(glue(
      "Comparing different environmental variables.
      No single name for *_u-test.csv"
    ))
  }
  tests <- vector("list", length = n_samples)
  for (i in 1:n_samples) {
    # Mann-Whitney U tests -----------------------------------------------------
    u_test <- compare_samples(
      x[i, ], y[i, ],
      "two.sided",
      force_mann_whitney_u = force_mann_whitney_u
    )
    u_test <- broom::tidy(u_test$test)
    CLES <- canprot::CLES(na.omit(x[i, ]), na.omit(y[i, ]))
    tests[[i]] <- cbind(test, CLES = CLES)
    if (!quietly) {
      setTxtProgressBar(pb, i)
    }
  }
  if (!quietly) {
    close(pb)
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
