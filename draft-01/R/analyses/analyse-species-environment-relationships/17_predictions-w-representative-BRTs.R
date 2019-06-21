# Predictions
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))
source(here(
  "R/analyses/",
  "analyse-species-environment-relationships/01_collate-data.R"
))

output_path <- here(
  "outputs/species-environment-relationships/from-local-machines/"
)

# Inspect collated data --------------------------------------------------------

GCFR_data_QDS_stack
SWAFR_data_QDS_stack
GCFR_data_HDS_stack
SWAFR_data_HDS_stack

max_richness <- max(na.rm = TRUE,
  getValues(GCFR_data_QDS_stack$QDS_richness),
  getValues(SWAFR_data_QDS_stack$QDS_richness)
)

# Import representative BRT-models ---------------------------------------------

model_paths <- c(
  "GCFR_HDS-richness-gbm-step_BRT_2018-12-06_312.RDS",
  "GCFR_HDS-turnover-gbm-step_BRT_2018-12-11_746.RDS",
  "GCFR_QDS-richness-gbm-step_BRT_2018-12-04_305.RDS",
  "SWAFR_HDS-richness-gbm-step_BRT_2018-12-06_325.RDS",
  "SWAFR_HDS-turnover-gbm-step_BRT_2018-12-10_406.RDS",
  "SWAFR_QDS-richness-gbm-step_BRT_2018-12-03_59.RDS"
)
models <- model_paths %>%
  paste0(output_path, .) %>%
  map(read_rds)
names(models) <- c(
  "GCFR_HDS_richness",
  "GCFR_HDS_turnover",
  "GCFR_QDS_richness",
  "SWAFR_HDS_richness",
  "SWAFR_HDS_turnover",
  "SWAFR_QDS_richness"
)

# Generate predicted richness/turnover -----------------------------------------

replace_obs_w_fitted <- function(enviro_stack, response_layer_name, fit) {
  response_layer_index <- which(names(enviro_stack) == response_layer_name)
  max_response_value <- max(
    c(getValues(enviro_stack[[response_layer_index]]), fit),
    na.rm = TRUE
  )
  plot(
    enviro_stack[[response_layer_index]],
    zlim = c(0, max_response_value),
    main = "Obs"
  )
  # Compile rows numbers where any var is NA
  non_na_indices <- TRUE  # to start
  for (i in 1:nlayers(enviro_stack)) {
    non_na_indices <- !is.na(enviro_stack[[i]])[] & non_na_indices
  }
  enviro_stack[[response_layer_index]][non_na_indices] <- fit
  enviro_stack[[response_layer_index]][
    enviro_stack[[response_layer_index]] > max(fit)
  ] <- NA
  plot(
    exp(enviro_stack[[response_layer_index]]),
    zlim = c(0, max_response_value),
    main = "Pred"
  )
  enviro_stack[[response_layer_index]]
}

GCFR_pred_QDS_richness <- replace_obs_w_fitted(
  GCFR_data_QDS_stack, "QDS_richness",
  models$GCFR_QDS_richness$fitted
)
writeRaster(
  GCFR_pred_QDS_richness,
  here(
    "outputs/species-environment-relationships/from-local-machines",
    "GCFR_pred_QDS_richness.tif"
  )
)
GCFR_pred_HDS_richness <- replace_obs_w_fitted(
  GCFR_data_HDS_stack, "HDS_richness",
  models$GCFR_HDS_richness$fitted
)
writeRaster(
  GCFR_pred_HDS_richness,
  here(
    "outputs/species-environment-relationships/from-local-machines",
    "GCFR_pred_HDS_richness.tif"
  )
)
SWAFR_pred_QDS_richness <- replace_obs_w_fitted(
  SWAFR_data_QDS_stack, "QDS_richness",
  models$SWAFR_QDS_richness$fitted
)
writeRaster(
  SWAFR_pred_QDS_richness,
  here(
    "outputs/species-environment-relationships/from-local-machines",
    "SWAFR_pred_QDS_richness.tif"
  )
)
SWAFR_pred_HDS_richness <- replace_obs_w_fitted(
  SWAFR_data_HDS_stack, "HDS_richness",
  models$SWAFR_HDS_richness$fitted
)
writeRaster(
  SWAFR_pred_HDS_richness,
  here(
    "outputs/species-environment-relationships/from-local-machines",
    "SWAFR_pred_HDS_richness.tif"
  )
)

# Cross predictions between scales ---------------------------------------------

cross_predict_gbm <- function(predictor_model, focal_region) {
  stopifnot({
    class(predictor_model) == "gbm"
    class(focal_region) == "gbm"
  })
  olddata <- as.data.frame(predictor_model$data$x.order)
  newdata <- as.data.frame(focal_region$data$x.order)
  for (oldvar in colnames(olddata)) {
    if (!(oldvar %in% colnames(newdata))) {
      #newdata[[oldvar]] <-
    }
  }
  pred_model1 <- predict.gbm(
    predictor_model,
    newdata = as.data.frame(focal_region$data$x.order),
    n.trees = predictor_model$n.trees
  )
  model1_name <- deparse(substitute(predictor_model))
  model2_name <- deparse(substitute(focal_region))
  tibble(pred_model1, obs_model1 = focal_region$data$y) %>%
    set_colnames(c(
      glue("pred_{model1_name}"),
      glue("obs_{model2_name}")
    ))
}

models %$%
  cross_predict_gbm(SWAFR_QDS_richness, SWAFR_HDS_richness) %$%
  lm(pred_SWAFR_QDS_richness ~ obs_SWAFR_HDS_richness) %>%
  visreg::visreg()

models %$%
  cross_predict_gbm(GCFR_HDS_turnover, SWAFR_HDS_turnover) %$%
  lm(pred_GCFR_HDS_turnover ~ obs_SWAFR_HDS_turnover) %>%
  visreg::visreg()
