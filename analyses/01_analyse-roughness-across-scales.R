# Analyse environmental roughness varying across spatial scales
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("data/01_import-region-polygons.R"))
source(here::here("data/02_import-floral-data.R"))
source(here::here("data/03_import-environmental-data.R"))

out_dir <- here::here("outputs/roughness-across-scales")

set.seed(1234)

# Test 0.05deg, QDS, HDS, 3QDS comparisons -------------------------------------

# Using Mann-Whitney U tests to compare roughness values for GCFR vs SWAFR
test_results <- foreach(resolution = list(0.05, 0.25, 0.50, 0.75)) %do% {
  test_results_at_a_res <-
    map2_df(
      .x = GCFR_variables,
      .y = SWAFR_variables,
      .f = ~ compare_roughness(
        .x, .y,
        resolution = resolution,
        force_mann_whitney_u = TRUE
      )
    ) %>%
    cbind(variable = var_names, .) %>%
    as_tibble()
}
names(test_results) <- c("0.05º", "QDS", "HDS", "3QDS")

test_results_summary <- test_results %>%
  map(mutate, sig = p.value < 0.05) %>%
  map(dplyr::select, variable, sig) %$%
  tibble(
    variable = var_names,
    `0.05º` = .$`0.05º`$sig,
    QDS = .$QDS$sig,
    HDS = .$HDS$sig,
    `3QDS` = .$`3QDS`$sig
  )

# Save to disc
write_csv(
  test_results_summary,
  glue("{out_dir}/test_results_summary.csv")
)

# CLES for those tests ---------------------------------------------------------

# <https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test#Effect_sizes>
test_results_CLES <- foreach(resolution = list(0.05, 0.25, 0.50, 0.75)) %do% {
  out <- foreach(GCFR = GCFR_variables, SWAFR = SWAFR_variables) %do% {
    canprot::CLES(
      na.omit(prep_layer(GCFR)[]),
      na.omit(prep_layer(SWAFR)[])
    )
  }
  names(out) <- var_names
  out
}
names(test_results_CLES) <- c("0.05º", "QDS", "HDS", "3QDS")

test_results_CLES_for_plot <- test_results_CLES %>%
  map(as_tibble) %$%
  rbind(.$`0.05º`, .$QDS, .$HDS, .$`3QDS`) %>%
  cbind(resolution = names(test_results_CLES), .) %>%
  gather(variable, CLES, -resolution) %>%
  as_tibble()

# Save to disc
write_csv(
  test_results_CLES_for_plot,
  glue("{out_dir}/test_results_CLES_for_plot.csv")
)
