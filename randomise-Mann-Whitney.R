# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
map(pre_analysis_import_paths, source)

# Redefine prep_layer() to _not_ sample 5000 points
prep_layer2 <- function(x, resolution) {
  x %>%
    aggregate(fact = resolution / 0.05) %>%
    focal_sd() %>%
    getValues()
}
# Create sampler
sample_layer <- function(x, n_samples = 1000, size = 1000,
                         quietly = FALSE, ...) {
  if (!quietly) {
    pb <- txtProgressBar(0, n_samples)
  }
  samples <- matrix(nrow = n_samples, ncol = size)
  for (i in 1:n_samples) {
    samples[i, ] <- sample(x, size)
    if (!quietly) {
      setTxtProgressBar(pb, i)
    }
  }
  if (!quietly) {
    close(pb)
  }
  samples
}
# Redefine compare_roughness()
compare_roughness_randomised <- function(x, y,
                                         resolution, n_samples, size,
                                         force_mann_whitney_u, ...) {
  x %<>%
    na.omit() %>%
    prep_layer2(resolution = resolution) %>%
    sample_layer(n_samples = n_samples, size = size)
  y %<>%
    na.omit() %>%
    prep_layer2(resolution = resolution) %>%
    sample_layer(n_samples = n_samples, size = size)
  tests <- vector("list", length = n_samples)
  for (i in 1:n_samples) {
    test <- compare_samples(
      x, y,
      "two.sided",
      force_mann_whitney_u = force_mann_whitney_u
    )
    test <- broom::tidy(test$test)
    CLES <- canprot::CLES(na.omit(x), na.omit(y))
    tests[[i]] <- cbind(test, CLES = CLES)
  }
  tests
}

# Test
if (FALSE) {
  foo <- sample_layer(prep_layer2(GCFR_variables$Elevation, resolution = 0.25))
  foo <- compare_roughness_randomised(
    GCFR_variables$Elevation,
    SWAFR_variables$Elevation,
    resolution = 0.05,
    n_samples = 100,
    size = 100,
    force_mann_whitney_u = TRUE
  )
}
