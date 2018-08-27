# Analyse environmental roughness how varies across spatial scales
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
source(here::here("analyses/generate-roughness.R"))

output_path <- here::here("outputs/roughness")

# Collarye roughness data ------------------------------------------------------

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
      .x %<>% getValues()
      .y %<>% getValues()
      # Compare with a Mann-Whitney U-test (called wilcox.test in R),
      U_p_value <- tidy(wilcox.test(.x, .y, alternative = "two.sided"))$p.value
      # and describe with CLES
      CLES_value <- CLES(.y, .x)
      message("Done")
      tibble(U_p_value, CLES_value)
    }
  )
)

# Save results to disc
write_csv(
  U_CLES_results,
  glue("{output_path}/U_CLES_results.csv")
)

# Save roughness distribution data to disc for use in Figure 1 -----------------

# Format into table
GCFR_roughness_data %<>% map_df(
  .id = "resolution",
  .f = ~ map_df(
    .x = .x,
    .id = "variable",
    .f = ~ data.frame(roughness = .x)
  )
)
SWAFR_roughness_data %<>% map_df(
  .id = "resolution",
  .f = ~ map_df(
    .x = .x,
    .id = "variable",
    .f = ~ data.frame(roughness = .x)
  )
)
roughness_data <- rbind(
  cbind(region = "Cape", GCFR_roughness_data),
  cbind(region = "SWA", SWAFR_roughness_data)
)

# Save to disc
write_csv(
  roughness_data,
  glue("{output_path}/roughness_data.csv")
)
