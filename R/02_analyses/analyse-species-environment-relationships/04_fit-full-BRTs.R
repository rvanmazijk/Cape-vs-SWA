# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 3: Fitting BRT models to full datasets
# Cape vs SWA publication
# Ruan van Mazijk

set.seed(1234)

# Choose nt, tc, lr ------------------------------------------------------------

# Thumbsucks for now:
tc <- 5  # No more than 5-way interactions
lr <- 0.002
nt <- 10000  # Maximum no. trees allowed in a BRT-model

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
        # Use initial predictor sets from after checking for collinearity
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

# For both richness and turnover as reponses, foor each region:
predictor_names_simp <- map(gbm_steps, map, simplify_predictors)

# Refitting models with simplified predictor sets ------------------------------

gbm_steps_simp <- pmap(
  # For both richness and turnover as reponses:
  .l = list(
    .response_name = list("HDS_richness", "mean_QDS_turnover"),
    .log_response = list(TRUE, FALSE),
    .predictor_names = predictor_names_simp
  ),
  .f = function(.response_name, .log_response, .predictor_names) {
    pmap(
      # For each region:
      .l = list(
        .variables = list(GCFR_variables_HDS_stack, SWAFR_variables_HDS_stack),
        .predictor_names = .predictor_names
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
names(gbm_steps_simp) <- c("HDS_richness_BRT", "mean_QDS_turnover_BRT")
for (i in seq_along(gbm_steps)) {
  names(gbm_steps_simp[[i]]) <- c("Cape", "SWA")
}

# Explore results
map(gbm_steps_simp, map, my_BRT_summary)
