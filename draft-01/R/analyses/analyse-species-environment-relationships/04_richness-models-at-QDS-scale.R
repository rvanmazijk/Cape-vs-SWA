# Fitting BRTs of richness at QDS-scale
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here(
  "R/analyses/analyse-species-environment-relationships/01_collate-data.R"
))

output_path <- here(
  "outputs/species-environment-relationships",
  "from-local-machines/QDS-richness-models_1000-reps"
)

# Run 1000 times to account for subtle differences -----------------------------
# (inherent randomness in BRT-algorithm)

for (i in 1:1000) {
#for (i in 655:1000) {
#for (i in 892:1000) {

  set.seed(i)

  GCFR_model <- gbm.step(
    data = GCFR_data_QDS,
    gbm.x = GCFR_predictor_names_QDS,
    gbm.y = "log_QDS_richness",
    tree.complexity = 3,
    learning.rate = 0.001,
    max.trees = 10000,
    family = "gaussian",
    plot.main = TRUE
  )
  write_rds(
    GCFR_model,
    glue("{output_path}/GCFR_QDS-richness-gbm-step_BRT_{Sys.Date()}_{i}.RDS")
  )
  write_csv(
    select(my_BRT_summary(GCFR_model), -contribs),
    glue("{output_path}/GCFR_QDS-richness-gbm-step_summary_{Sys.Date()}_{i}.csv")
  )
  write_csv(
    as.data.frame(my_BRT_summary(GCFR_model)$contribs),
    glue("{output_path}/GCFR_QDS-richness-gbm-step_contribs_{Sys.Date()}_{i}.csv")
  )
  rm(GCFR_model)

  SWAFR_model <- gbm.step(
    data = SWAFR_data_QDS,
    gbm.x = SWAFR_predictor_names_QDS,
    gbm.y = "log_QDS_richness",
    tree.complexity = 3,
    learning.rate = 0.001,
    max.trees = 10000,
    family = "gaussian",
    plot.main = TRUE
  )
  write_rds(
    SWAFR_model,
    glue("{output_path}/SWAFR_QDS-richness-gbm-step_BRT_{Sys.Date()}_{i}.RDS")
  )
  write_csv(
    select(my_BRT_summary(SWAFR_model), -contribs),
    glue("{output_path}/SWAFR_QDS-richness-gbm-step_summary_{Sys.Date()}_{i}.csv")
  )
  write_csv(
    as.data.frame(my_BRT_summary(SWAFR_model)$contribs),
    glue("{output_path}/SWAFR_QDS-richness-gbm-step_contribs_{Sys.Date()}_{i}.csv")
  )
  rm(SWAFR_model)

}
