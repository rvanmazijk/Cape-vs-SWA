# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---
#   using bare-minimum BRTs on the UCT HPC
# Import data and define functions
# Cape vs SWA publication
# Ruan van Mazijk

# Import data ------------------------------------------------------------------

# Import enviro + roughness + richness + turnover datasets
GCFR_variables_HDS <- read.csv("GCFR_variables_HDS.csv")[, -1]
SWAFR_variables_HDS <- read.csv("SWAFR_variables_HDS.csv")[, -1]

# Import non-collinear environmental variable names
GCFR_predictor_names <- read.csv("GCFR_predictor_names.csv", stringsAsFactors = FALSE)[[2]]
SWAFR_predictor_names <- read.csv("SWAFR_predictor_names.csv", stringsAsFactors = FALSE)[[2]]

# Define functions -------------------------------------------------------------

fit_gbm_step <- function(variables, predictor_names, response_name,
                         log_response = TRUE,
                         tc, lr, nt) {
  # Convenience function to fit and/or refit a BRT model
  stopifnot(exprs = {
    is.data.frame(variables)
    is.character(predictor_names)
  })
  if (log_response) {
    variables[[response_name]] <- log(variables[[response_name]])
  }
  print(paste(
    "Fitting", response_name,
    ifelse(log_response, "(logged)", "(unlogged)"),
    "with tc =", tc, ", lr = ", lr, ", max.trees = ", nt
  ))
  dismo::gbm.step(
    data = variables,
    gbm.x = predictor_names,
    gbm.y = response_name,
    tree.complexity = tc,
    learning.rate = lr,
    max.trees = nt,
    family = "gaussian"
  )
}
simplify_predictors <- function(x) {
  # Convenience function for gbm.simplify()
  stopifnot(class(x) == "gbm")
  gbm_simp <- dismo::gbm.simplify(x)
  # Drop as many variables as can if multiple nos. of drops are optimal
  # (hence (max(which(mean == min(mean)))))
  optimal_no_drops <- max(which(
    gbm_simp$deviance.summary$mean == min(gbm_simp$deviance.summary$mean)
  ))
  gbm_simp$pred.list[[optimal_no_drops]]
}

