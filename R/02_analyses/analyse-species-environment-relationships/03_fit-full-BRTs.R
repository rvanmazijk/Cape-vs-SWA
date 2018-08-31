# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 3: Fitting BRT models to full datasets
# Cape vs SWA publication
# Ruan van Mazijk

# Choose nt, tc, lr ------------------------------------------------------------

# TODO properly

# Thumbsucks for now:
nt <- 10000
tc <- 5  # No more than 5-way interactions
lr <- 0.002

# Initial model fitting: gbm.step(richness ~ ...) ------------------------------

gbm_steps <- pmap(
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
        .predictor_names = list(GCFR_predictor_names, SWAFR_predictor_names)
      ),
      .f = function(.variables, .predictor_names) {
        fit_gbm_step(
          variables = .variables, predictor_names = .predictor_names,
          response_name = .response_name, log_response = .log_response,
          tc = tc, lr = lr, nt = nt
        )
      }
    )
  }
)
names(gbm_steps) <- c("HDS_richness_BRT", "mean_QDS_turnover_BRT")
for (i in seq_along(gbm_steps)) {
  names(gbm_steps[[i]]) <- c("Cape", "SWA")
}

# Explore results
map(gbm_steps, map, my_BRT_summary)

# Model simplification: gbm.simplify(...) --------------------------------------

GCFR_gbm_simp_richness <- gbm.simplify(GCFR_gbm_step_richness)
optimal_no_drops <- GCFR_gbm_simp_richness$deviance.summary %$%
  which(mean == min(mean))
GCFR_richness_predictor_names_simp <-
  GCFR_gbm_simp_richness$pred.list[[optimal_no_drops]]

SWAFR_gbm_simp_richness <- gbm.simplify(SWAFR_gbm_step_richness)
optimal_no_drops <- SWAFR_gbm_simp_richness$deviance.summary %$%
  which(mean == min(mean))
SWAFR_richness_predictor_names_simp <-
  SWAFR_gbm_simp_richness$pred.list[[optimal_no_drops]]

GCFR_gbm_simp_turnover <- gbm.simplify(GCFR_gbm_step_turnover)
optimal_no_drops <- GCFR_gbm_simp_turnover$deviance.summary %$%
  which(mean == min(mean))
GCFR_turnover_predictor_names_simp <-
  GCFR_gbm_simp_turnover$pred.list[[optimal_no_drops]]
# FIXME: need to manually choose a predictor set,
#   as multiple sets of predictors with same number of drops

SWAFR_gbm_simp_turnover <- gbm.simplify(SWAFR_gbm_step_turnover)
optimal_no_drops <- SWAFR_gbm_simp_turnover$deviance.summary %$%
  which(mean == min(mean))
SWAFR_turnover_predictor_names_simp <-
  SWAFR_gbm_simp_turnover$pred.list[[optimal_no_drops]]
# FIXME: need to manually choose a predictor set,
#   as multiple sets of predictors with same number of drops

# Refitting models with simplified predictor sets ------------------------------

gbm_steps_simp_richness <- pmap(
  # For each region:
  .l = list(
    variables = list(GCFR_variables_HDS_stack, SWAFR_variables_HDS_stack),
    predictor_names = list(GCFR_predictor_names_simp, SWAFR_predictor_names_simp)
  ),
  .f = function(variables, predictor_names) {
    fit_gbm_step(
      variables, predictor_names,
      response_name = "HDS_richness", log_response = TRUE,
      tc = tc, lr = lr, nt = nt
    )
  }
)
names(gbm_steps_simp_richness) <- c("Cape", "SWA")

gbm_steps_simp_turnover <- pmap(
  # For each region:
  .l = list(
    variables = list(GCFR_variables_HDS_stack, SWAFR_variables_HDS_stack),
    predictor_names = list(GCFR_predictor_names_simp, SWAFR_predictor_names_simp)
  ),
  .f = function(variables, predictor_names) {
    fit_gbm_step(
      variables, predictor_names,
      response_name = "mean_QDS_turnover", log_response = FALSE,
      tc = tc, lr = lr, nt = nt
    )
  }
)
names(gbm_steps_simp_turnover) <- c("Cape", "SWA")

# Explore results
map(gbm_steps_simp_richness, my_BRT_summary)
map(gbm_steps_simp_turnover, my_BRT_summary)
