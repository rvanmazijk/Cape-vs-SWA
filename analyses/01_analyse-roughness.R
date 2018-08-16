# Analyse environmental roughness how varies across spatial scales
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
source(here::here("data/03_import-environmental-data.R"))

output_path <- here::here("outputs/roughness")

# Compute roughness data -------------------------------------------------------

GCFR_roughness <- map_df(
  .x = GCFR_variables,
  .id = "variable",
  .f = ~ get_roughness_values(.x)
)
GCFR_roughness_QDS <- map_df(
  .x = GCFR_variables,
  .id = "variable",
  .f = ~ get_roughness_values(.x, resolution = 0.25)
)
GCFR_roughness_HDS <- map_df(
  .x = GCFR_variables,
  .id = "variable",
  .f = ~ get_roughness_values(.x, resolution = 0.50)
)
GCFR_roughness_3QDS <- map_df(
  .x = GCFR_variables,
  .id = "variable",
  .f = ~ get_roughness_values(.x, resolution = 0.75)
)

SWAFR_roughness <- map_df(
  .x = SWAFR_variables,
  .id = "variable",
  .f = ~ get_roughness_values(.x)
)
SWAFR_roughness_QDS <- map_df(
  .x = SWAFR_variables,
  .id = "variable",
  .f = ~ get_roughness_values(.x, resolution = 0.25)
)
SWAFR_roughness_HDS <- map_df(
  .x = SWAFR_variables,
  .id = "variable",
  .f = ~ get_roughness_values(.x, resolution = 0.50)
)
SWAFR_roughness_3QDS <- map_df(
  .x = SWAFR_variables,
  .id = "variable",
  .f = ~ get_roughness_values(.x, resolution = 0.75)
)

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

# Perform U-tests and CLES -----------------------------------------------------

# Using Mann-Whitney U-tests at each resolution (0.05º, QDS, HDS and 3QDS)
# to test for significance,
# and common language effect size (CLES) of Cape versus SWA to
# describe the difference

# U-test cannot run for > 10,000 obs, so when running at 0.05º resolution
# we need to sub-sample to 5000 points per region.

# Pseudo-code:
#   for each resolution {
#     for each variable {
#       Mann-Whitney-U-test(Cape, SWA)
#       CLES(Cape, SWA)
#     }
#   }

set.seed(1234)

U_CLES_results <- map2_df(
  # For every spatial resolution,
  .x = GCFR_roughness_data, .y = SWAFR_roughness_data,
  .id = "resolution",
  .f = ~ map2_df(
    # For every variable in each region,
    .x = .x, .y = .y,
    .id = "variable",
    .f = function(.x, .y) {
      # Compare with a Mann-Whitney U-test (called wilcox.test in R),
      U_p_value <- tidy(wilcox.test(.x, .y, alternative = "two.sided"))$p.value
      # and describe with CLES
      CLES_value <- CLES(.y, .x)
      message("Done")
      tibble(U_p_value, CLES_value)
    }
  )
)

# Save to disc
write_csv(
  U_CLES_results,
  glue("{output_path}/U_CLES_results.csv")
)
