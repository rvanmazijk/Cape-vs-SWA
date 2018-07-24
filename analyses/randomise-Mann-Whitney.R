# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
map(pre_analysis_import_paths, source)

set.seed(1234)

# Redefine prep_layer() to _not_ sample 5000 points
prep_layer2 <- function(x, resolution) {
  x %>%
    aggregate(fact = resolution / 0.05) %>%
    focal_sd() %>%
    getValues()
}

# Create bootstrap sampler
sample_layer <- function(x, n_samples = 1000, size = length(x),
                         quietly = FALSE, ...) {
  if (!quietly) {
    print(glue("Taking {n_samples} bootstrap samples of size {size}"))
    pb <- txtProgressBar(0, n_samples)
  }
  samples <- matrix(nrow = n_samples, ncol = size)
  for (i in 1:n_samples) {
    samples[i, ] <- sample(x, size = size, replace = TRUE)
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

# Redefine compare_roughness()
compare_roughness_randomised <- function(x, y,
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
summarise_comparisons <- function(x) {
  x %>%
    map(mutate, sig = p.value < 0.05) %>%
    map(dplyr::select, variable, sig) %$%
    tibble(
      variable = var_names,
      `0.05º` = .$`0.05º`$sig,
      QDS = .$QDS$sig,
      HDS = .$HDS$sig,
      `3QDS` = .$`3QDS`$sig
    )
}

# Test
if (FALSE) {
  foo <- sample_layer(prep_layer2(GCFR_variables$Elevation, resolution = 0.25))
  foo2 <- compare_roughness_randomised(
    SWAFR_variables$Elevation,
    GCFR_variables$Elevation,
    resolution = 0.25,
    n_samples = 1000,
    force_mann_whitney_u = TRUE
  )
  sd(unlist(map(foo2, "CLES")))
}
