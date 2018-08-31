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
  # For each region:
  .l = list(
    variables = list(GCFR_variables_HDS_stack, SWAFR_variables_HDS_stack),
    predictor_names = list(GCFR_predictor_names, SWAFR_predictor_names)
  ),
  .f = function(variables, predictor_names) {
    fit_gbm_step(
      variables, predictor_names,
      response_name = "HDS_richness", log_response = TRUE,
      tc = tc, lr = lr, nt = nt
    )
  }
)

# Explore results

GCFR_gbm_step <- gbm_steps[[1]]
SWAFR_gbm_step <- gbm_steps[[2]]

pseudo_r2(GCFR_gbm_step)
GCFR_gbm_step$n.trees
GCFR_gbm_step$contributions
summary(GCFR_gbm_step)

pseudo_r2(SWAFR_gbm_step)
SWAFR_gbm_step$n.trees
SWAFR_gbm_step$contributions
summary(SWAFR_gbm_step)

# Model simplification: gbm.simplify(...) --------------------------------------

GCFR_gbm_simp <- gbm.simplify(GCFR_gbm_step)
optimal_no_drops <- GCFR_gbm_simp$deviance.summary %$%
  which(mean == min(mean))
GCFR_predictor_names_simp <- GCFR_gbm_simp$pred.list[[optimal_no_drops]]

SWAFR_gbm_simp <- gbm.simplify(SWAFR_gbm_step)
optimal_no_drops <- SWAFR_gbm_simp$deviance.summary %$%
  which(mean == min(mean))
SWAFR_predictor_names_simp <- SWAFR_gbm_simp$pred.list[[optimal_no_drops]]

# Refitting models with simplified predictor sets ------------------------------

gbm_steps_simp <- pmap(
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

# Explore results

GCFR_gbm_step_simp <- gbm_steps_simp[[1]]
SWAFR_gbm_step_simp <- gbm_steps_simp[[2]]

pseudo_r2(GCFR_gbm_step_simp)
GCFR_gbm_step_simp$n.trees
GCFR_gbm_step_simp$contributions
summary(GCFR_gbm_step_simp)

pseudo_r2(SWAFR_gbm_step_simp)
SWAFR_gbm_step_simp$n.trees
SWAFR_gbm_step_simp$contributions
summary(SWAFR_gbm_step_simp)
