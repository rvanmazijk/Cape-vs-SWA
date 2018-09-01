# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 5: Fitting BRT models to randomly permuted datasets
# Cape vs SWA publication
# Ruan van Mazijk

set.seed(1234)

library(doParallel)

# Choose nt, tc, lr ------------------------------------------------------------

# Thumbsucks for now:
tc <- 1:5  # No more than 5-way interactions
lr <- c(  # A range of learning rates
  0.01,
  0.005,
  0.001,
  0.0005,
  0.0001
)
nt <- 10000  # Maximum no. trees allowed in a BRT-model

presets <-
  expand.grid(tc = tc, lr = lr, KEEP.OUT.ATTRS = FALSE) %>%
  t() %>%
  as.data.frame() %>%
  as.list()
names(presets) <- NULL
for (i in seq_along(presets)) {
  names(presets[[i]]) <- c("tc", "lr")
}

cluster <- makeCluster(detectCores() - 1, outfile = "")
registerDoParallel(cluster)
foreach(preset = presets) %dopar% {
  print(preset)
}
stopCluster(cluster)

# Permute response columns -----------------------------------------------------

GCFR_variables_HDS_stack$HDS_richness %<>% permute_wo_nas()
GCFR_variables_HDS_stack$mean_QDS_turnover %<>% permute_wo_nas()
SWAFR_variables_HDS_stack$HDS_richness %<>% permute_wo_nas()
SWAFR_variables_HDS_stack$mean_QDS_turnover %<>% permute_wo_nas()

# Run BRT fit-simplify-refit ---------------------------------------------------

gbm_steps_simp <- pmap(
  # For both richness and turnover as reponses:
  .l = list(
    .response_name = list("HDS_richness", "mean_QDS_turnover"),
    .log_response = list(TRUE, FALSE)
  ),
  .f = function(.response_name, .log_response) {
    pmap(
      # For each region:
      .l = list(
        .variables = list(GCFR_variables_HDS_stack, SWAFR_variables_HDS_stack),
        # Use initial predictor sets from after checking for collinearity
        .predictor_names = list(GCFR_predictor_names, SWAFR_predictor_names)
      ),
      .f = function(.variables, .predictor_names) {

        # Initial model fitting: gbm.step(richness ~ ...) ----------------------

        gbm_step <- fit_gbm_step(
          variables = .variables, predictor_names = .predictor_names,
          response_name = .response_name, log_response = .log_response,
          permuted_BRT = TRUE,
          tc = tc, lr = lr, nt = nt
        )

        # Model simplification: gbm.simplify(...) ------------------------------

        predictor_names_simp <- simplify_predictors(gbm_step)

        # Refitting models with simplified predictor sets ----------------------

        gbm_step_simp <- fit_gbm_step(
          variables = .variables, predictor_names = predictor_names_simp,
          response_name = .response_name, log_response = .log_response,
          tc = tc, lr = lr, nt = nt
        )

      }
    )
  }
)
names(gbm_steps_simp) <- c("HDS_richness_BRT", "mean_QDS_turnover_BRT")
for (i in seq_along(gbm_steps)) {
  names(gbm_steps_simp[[i]]) <- c("Cape", "SWA")
}

# Explore results
map(gbm_steps_simp, map, my_BRT_summary)
