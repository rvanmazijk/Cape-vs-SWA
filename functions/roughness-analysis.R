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
compare_roughness_bootstrapped <- function(x, y, x_region_name, y_region_name,
                                           variable,
                                           resolution, n_samples,
                                           force_mann_whitney_u,
                                           use_disc = FALSE, ...) {
  # Define inner functions -----------------------------------------------------
  prep_and_bootstrap <- function(x, x_name,
                                 resolution, n_samples,
                                 use_disc = FALSE) {
    prep_layer2 <- function(x, resolution) {
      x %>%
        aggregate(fact = resolution / 0.05) %>%
        focal_sd() %>%
        getValues()
    }
    bootstrap_sample <- function(x, n = 1000, quietly = FALSE, ...) {
      print(glue(
        "Taking {n_samples} bootstrap samples of size {length(x)}"
      ))
      return(replicate(n = n, {
        sample(x, size = length(x), replace = TRUE)
      }))
      print(glue(
        "Done"
      ))
    }
    # Orchestrate prep_layer2() & bootstrap_sample() ---------------------------
    x %<>%
      na.omit() %>%
      prep_layer2(resolution = resolution) %>%
      na.omit() %>%
      bootstrap_sample(n_samples, quietly = TRUE)
    if (use_disc) {
      path <- here::here(glue(
        "outputs/compare-roughness-bootstrap/{x_name}_bootstraps.csv"
      ))
      write_csv(as.data.frame(x), path)
      print(glue(
        "Saved {x_name} bootstrap samples to disc"
      ))
    }
    if (!use_disc) {
      return(x)
    }
  }
  # Bootstrap-sample x & y -----------------------------------------------------
  print(glue(
    "[Comparing {x_region_name} and {y_region_name} {variable} \\
    at resolution = {resolution}]"
  ))
  print(glue(
    "Bootstrap-sampling layers..."
  ))
  x_name <- glue("{x_region_name}_{variable}_{resolution}")
  y_name <- glue("{y_region_name}_{variable}_{resolution}")
  x <- prep_and_bootstrap(x, x_name, resolution, n_samples, use_disc = use_disc)
  y <- prep_and_bootstrap(y, y_name, resolution, n_samples, use_disc = use_disc)
  # Run Mann-Whitney & CLES on bootstraps --------------------------------------
  print(glue(
     "Taken {n_samples} bootstrap-samples of both \\
     {x_region_name} and {y_region_name}"
  ))
  print(glue(
    "Running Mann-Whitney U tests and CLES on bootstrap-samples..."
  ))
  pb <- txtProgressBar(0, n_samples)
  if (use_disc) {
    print(glue(
      "Reading bootstrap-samples back from disc at {x} and {y}..."
    ))
    x <- as.matrix(read_csv(
      here::here(glue(
        "outputs/compare-roughness-bootstrap/{x_name}_bootstraps.csv"
      )),
      col_types = cols()  # Suppress col_type messages
    ))
    y <- as.matrix(read_csv(
      here::here(glue(
        "outputs/compare-roughness-bootstrap/{y_name}_bootstraps.csv"
      )),
      col_types = cols()
    ))
    print(glue(
      "Read bootstrap-samples back from disc"
    ))
  }
  print(glue(
    "Running U-tests and CLES"
  ))
  if (use_disc) {
    print(glue(
       "(Saving to disc too)"
    ))
  }
  tests <- vector("list", length = n_samples)
  for (i in 1:n_samples) {
    sample_number <- str_pad(i, nchar(n_samples), pad = "0")
    # .... Mann-Whitney U tests ------------------------------------------------
    u_test <- wilcox.test(
      x[i, ], y[i, ],
      "two.sided"
    )
    u_test %<>% broom::tidy()
    if (use_disc) {
      write_csv(
        u_test,
        here::here(glue(
          "outputs/compare-roughness-bootstrap/\\
          {variable}_{resolution}_u-test_{sample_number}.csv"
        ))
      )
    }
    # .... CLES ----------------------------------------------------------------
    CLES_test <- data.frame(CLES =
        canprot::CLES(na.omit(x[i, ]), na.omit(y[i, ]))
    )
    if (use_disc) {
      write_csv(
        CLES_test,
        here::here(glue(
          "outputs/compare-roughness-bootstrap/\\
          {variable}_{resolution}_CLES-test_{sample_number}.csv"
        ))
      )
    }
    # .... Store in list if not saving to disc ---------------------------------
    if (!use_disc) {
      tests[[i]] <- cbind(u_test, CLES_test)
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  # .... Read tests back from disk if saved to disc ----------------------------
  if (use_disc) {
    print(glue(
      "Reading {variable} U-test and CLES CSVs back from disc"
    ))
    pb <- txtProgressBar(0, n_samples)
    for (i in 1:n_samples) {
      sample_number <- str_pad(i, nchar(n_samples), pad = "0")
      tests[[i]] <- cbind(
        read_csv(
          here::here(glue(
            "outputs/compare-roughness-bootstrap/\\
            {variable}_{resolution}_u-test_{sample_number}.csv"
          )),
          col_types = cols()
        ),
        read_csv(
          here::here(glue(
            "outputs/compare-roughness-bootstrap/\\
            {variable}_{resolution}_CLES-test_{sample_number}.csv"
          )),
          col_types = cols()
        )
      )
      setTxtProgressBar(pb, i)
    }
  }
  close(pb)
  print(glue(
    "Done"
  ))
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
