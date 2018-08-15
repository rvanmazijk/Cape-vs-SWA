# Analyse environmental roughness how varies across spatial scales
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("data/03_import-environmental-data.R"))

output_path <- here::here("outputs/roughness")

# Compare roughness values between Cape and SWA --------------------------------

# Using Mann-Whitney U-tests at each resolution (0.05º, QDS, HDS and 3QDS)

# U-test cannot run for > 10,000 obs,  so when running at 0.05º resolution
# we need to sub-sample to 5000 points per region.

set.seed(1234)

GCFR_roughness_data <- list(
  "0.05º" = GCFR_roughness,
  "QDS"   = GCFR_roughness_QDS,
  "HDS"   = GCFR_roughness_HDS,
  "3QDS"  = GCFR_roughness_3QDS
)
SWAFR_roughness_data <- list(
  "0.05º" = SWAFR_roughness,
  "QDS"   = SWAFR_roughness_QDS,
  "HDS"   = SWAFR_roughness_HDS,
  "3QDS"  = SWAFR_roughness_3QDS
)

U_tests <- as_tibble(map2_df(
  .x = GCFR_roughness_data, .y = SWAFR_roughness_data,
  .id = "resolution",
  .f = ~ map2_df(
    .x = .x, .y = .y,
    .id = "variable",
    .f = ~ tidy(wilcox.test(.x, .y, alternative = "two.sided"))
  )
))

# Save to disc
write_csv(
  U_tests,
  glue("{output_path}/U_tests.csv")
)

# Describe differences in roughness values between Cape and SWA ----------------

# Using common language effect size (CLES) of Cape verse SWA at each resolution

CLES_results <- as_tibble(map2_df(
  .x = GCFR_roughness_data, .y = SWAFR_roughness_data,
  .id = "resolution",
  .f = ~ map2_df(
    .x = .x, .y = .y,
    .id = "variable",
    .f = ~ data.frame(CLES = CLES(.y, .x))  # order NB
  )
))

# Save to disc
write_csv(
  CLES_results,
  glue("{output_path}/CLES_results.csv")
)
