# Fitting BRTs of permuted richness at HDS-scale
# Cape vs SWA publication
# Ruan van Mazijk

output_path <- here(
  "outputs/species-environment-relationships",
  "from-local-machines/HDS-richness-models_999-permuted-reps"
)

GCFR_data_HDS <- GCFR_variables_HDS_stack %>%
  as.data.frame() %>%
  na.exclude() %>%
  mutate(log_HDS_richness = log(HDS_richness))

SWAFR_data_HDS <- SWAFR_variables_HDS_stack %>%
  as.data.frame() %>%
  na.exclude() %>%
  mutate(log_HDS_richness = log(HDS_richness))

for (i in 1:999) {

  set.seed(i)

  GCFR_data_HDS_permuted <- GCFR_data_HDS
  GCFR_data_HDS_permuted$log_HDS_richness <- permute_wo_nas(
    GCFR_data_HDS_permuted$log_HDS_richness
  )
  GCFR_model <- gbm.step(
    data = GCFR_data_HDS_permuted,
    gbm.x = GCFR_predictor_names_HDS,
    gbm.y = "log_HDS_richness",
    tree.complexity = 3,
    learning.rate = 0.001,
    max.trees = 10000,
    family = "gaussian",
    plot.main = TRUE
  )
  model_succeeded <- !is.null(GCFR_model)
  if (model_succeeded) {
    write_rds(
      GCFR_model,
      glue("{output_path}/GCFR_HDS-richness-gbm-step_BRT_{Sys.Date()}_permutation-{i}.RDS")
    )
    write_csv(
      select(my_BRT_summary(GCFR_model), -contribs),
      glue("{output_path}/GCFR_HDS-richness-gbm-step_summary_{Sys.Date()}_permutation-{i}.csv")
    )
    write_csv(
      as.data.frame(my_BRT_summary(GCFR_model)$contribs),
      glue("{output_path}/GCFR_HDS-richness-gbm-step_contribs_{Sys.Date()}_permutation-{i}.csv")
    )
  }
  rm(GCFR_model)

  SWAFR_data_HDS_permuted <- SWAFR_data_HDS
  SWAFR_data_HDS_permuted$log_HDS_richness <- permute_wo_nas(
    SWAFR_data_HDS_permuted$log_HDS_richness
  )
  SWAFR_model <- gbm.step(
    data = SWAFR_data_HDS_permuted,
    gbm.x = SWAFR_predictor_names_HDS,
    gbm.y = "log_HDS_richness",
    tree.complexity = 3,
    learning.rate = 0.001,
    max.trees = 10000,
    family = "gaussian",
    plot.main = TRUE
  )
  model_succeeded <- !is.null(SWAFR_model)
  if (model_succeeded) {
    write_rds(
      SWAFR_model,
      glue("{output_path}/SWAFR_HDS-richness-gbm-step_BRT_{Sys.Date()}_permutation-{i}.RDS")
    )
    write_csv(
      select(my_BRT_summary(SWAFR_model), -contribs),
      glue("{output_path}/SWAFR_HDS-richness-gbm-step_summary_{Sys.Date()}_permutation-{i}.csv")
    )
    write_csv(
      as.data.frame(my_BRT_summary(SWAFR_model)$contribs),
      glue("{output_path}/SWAFR_HDS-richness-gbm-step_contribs_{Sys.Date()}_permutation-{i}.csv")
    )
  }
  rm(SWAFR_model)

}
