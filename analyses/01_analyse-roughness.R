# Analyse environmental roughness how varies across spatial scales
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("data/01_import-region-polygons.R"))
source(here::here("data/02_import-floral-data.R"))
source(here::here("data/03_import-environmental-data.R"))

output_path <- here::here("outputs/roughness")

# U-test cannot run for > 10,000 obs,
# so when running at 0.05º resolution,
# need to sub-sample to 5000 points per region.
# Thus, for replicability:
set.seed(1234)

# Compare roughness values between Cape and SWA --------------------------------

# Using Mann-Whitney U-tests at each resolution (0.05º, QDS, HDS and 3QDS)

U_tests <- foreach(resolution = list(0.05, 0.25, 0.50, 0.75)) %do% {
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
names(U_tests) <- c("0.05º", "QDS", "HDS", "3QDS")

U_tests_summary <- U_tests %>%
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
  U_tests_summary,
  glue("{output_path}/U_tests_summary.csv")
)

# Describe differences in roughness values between Cape and SWA ----------------

# Using common language effect size (CLES) at each resolution

CLES_results <- foreach(resolution = list(0.05, 0.25, 0.50, 0.75)) %do% {
  out <- foreach(GCFR = GCFR_variables, SWAFR = SWAFR_variables) %do% {
    canprot::CLES(
      na.omit(prep_layer(GCFR)[]),
      na.omit(prep_layer(SWAFR)[])
    )
  }
  names(out) <- var_names
  out
}
names(CLES_results) <- c("0.05º", "QDS", "HDS", "3QDS")

CLES_results %<>%
  map(as_tibble) %$%
  rbind(.$`0.05º`, .$QDS, .$HDS, .$`3QDS`) %>%
  cbind(resolution = c("0.05º", "QDS", "HDS", "3QDS"), .) %>%
  gather(variable, CLES, -resolution) %>%
  as_tibble()

# Save to disc
write_csv(
  test_results_CLES_for_plot,
  glue("{output_path}/CLES_results.csv")
)
