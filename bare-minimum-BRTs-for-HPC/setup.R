# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---
#   using bare-minimum BRTs on the UCT HPC
# Import data and define functions
# Cape vs SWA publication
# Ruan van Mazijk

# Load packages into memory ----------------------------------------------------

library(dismo)
library(gbm)
library(foreach)

# Import data ------------------------------------------------------------------

# Import enviro + roughness + richness + turnover datasets
GCFR_variables_HDS <- read.csv("GCFR_variables_HDS.csv")[, -1]
SWAFR_variables_HDS <- read.csv("SWAFR_variables_HDS.csv")[, -1]

# Import non-collinear environmental variable names
GCFR_predictor_names <- read.csv("GCFR_predictor_names.csv", stringsAsFactors = FALSE)[[2]]
SWAFR_predictor_names <- read.csv("SWAFR_predictor_names.csv", stringsAsFactors = FALSE)[[2]]

# Maximum no. trees allowed = 10,000 (global variable) -------------------------

nt <- 10000

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
  message(paste(
    "Fitting", response_name,
    ifelse(log_response, "(logged)", "(unlogged)"),
    "with tc =", tc, ", lr = ", lr, ", max.trees = ", nt
  ))
  gbm_step <- dismo::gbm.step(
    data = variables,
    gbm.x = predictor_names,
    gbm.y = response_name,
    tree.complexity = tc,
    learning.rate = lr,
    max.trees = nt,
    family = "gaussian",
    silent = TRUE
  )
  message("Done (fit_gbm_step())")
  gbm_step
}
simplify_predictors <- function(x) {
  # Convenience function for gbm.simplify()
  gbm_simp <- dismo::gbm.simplify(x)
  # Drop as many variables as can if multiple nos. of drops are optimal
  # (hence (max(which(mean == min(mean)))))
  optimal_no_drops <- max(which(
    gbm_simp$deviance.summary$mean == min(gbm_simp$deviance.summary$mean)
  ))
  gbm_simp$pred.list[[optimal_no_drops]]
}
run_initial_BRTs <- function(preset,
                             response_names = list("HDS_richness",
                                                   "mean_QDS_turnover"),
                             log_response_options = list(TRUE, FALSE),
                             regions_variables = list(GCFR_variables_HDS,
                                                      SWAFR_variables_HDS),
                             regions_predictor_names = list(GCFR_predictor_names,
                                                            SWAFR_predictor_names)) {
  # Analyse value of environmental & heterogeneity variables for predicting
  #   vascular plant species richness and turnover---
  #   using bare-minimum BRTs on the UCT HPC
  # Part 1:
  #   Fitting BRT models with different tc and lr settings:
  #     Define what's going to be run in each parallel branch
  stopifnot(is.list(preset))
  gbm_steps_simp <- foreach(response_name = response_names,
                            log_response = log_response_options) %do% {
    foreach(variables = regions_variables,
            predictor_names = regions_predictor_names) %do% {
      # Initial model fitting: gbm.step(richness ~ ...)
      gbm_step <- fit_gbm_step(
        variables = variables, predictor_names = predictor_names,
        response_name = response_name, log_response = log_response,
        tc = preset$tc, lr = preset$lr, nt = nt
      )
      if (is.null(gbm_step)) {
        # Catch when BRT cannot fit because lr or step-size too fast
        # or inappropriate
        return("failed")
      } else {
        # Continue on to simplify and refit BRT-models
        message("Inital BRT-model fit")
        predictor_names_simp <- simplify_predictors(gbm_step)
        message("Simpler predictor set found")
        gbm_step_simp <- fit_gbm_step(
          variables = variables, predictor_names = predictor_names_simp,
          response_name = response_name, log_response = log_response,
          tc = preset$tc, lr = preset$lr, nt = nt
        )
        message("Inital BRT-model re-fit to simplified predictor set")
        return(gbm_step_simp)
      }
    }
  }
  names(gbm_steps_simp) <- c("HDS_richness_BRT", "mean_QDS_turnover_BRT")
  for (i in seq_along(gbm_steps_simp)) {
    names(gbm_steps_simp[[i]]) <- c("Cape", "SWA")
  }
  message("Done (run_initial_BRTs())")
  gbm_steps_simp
}
run_final_BRTs <- function(preset) {
  # Analyse value of environmental & heterogeneity variables for predicting
  #   vascular plant species richness and turnover---
  #   using bare-minimum BRTs on the UCT HPC
  # Part 2:
  #   Fitting final BRT models with ideal tc and lr settings
  stopifnot(is.list(preset))
  model_configs <- list(
    GCFR_richness = list(
      response_name = "HDS_richness",
      log_response = TRUE,
      variables = GCFR_variables_HDS,
      predictor_names = GCFR_predictor_names
    ),
    GCFR_turnover = list(
      response_name = "mean_QDS_turnover",
      log_response = FALSE,
     variables = GCFR_variables_HDS,
     predictor_names = GCFR_predictor_names
    ),
    SWAFR_richness = list(
      response_name = "HDS_richness",
      log_response = TRUE,
      variables = SWAFR_variables_HDS,
      predictor_names = SWAFR_predictor_names
    ),
    SWAFR_turnover = list(
      response_name = "mean_QDS_turnover",
      log_response = FALSE,
      variables = GCFR_variables_HDS,
      predictor_names = SWAFR_predictor_names
    )
  )
  registerDoParallel(detectCores())
  gbm_steps_simp <- foreach(model_config = model_configs) %dopar% {
    gbm_step <- fit_gbm_step(
      variables = model_config$variables,
      predictor_names = model_config$predictor_names,
      response_name = model_config$response_name,
      log_response = model_config$log_response,
      tc = preset$tc,
      lr = preset$lr,
      nt = nt
    )
    message("Inital BRT-model fit")
    predictor_names_simp <- simplify_predictors(gbm_step)
    message("Simpler predictor set found")
    gbm_step_simp <- fit_gbm_step(
      variables = model_config$variables,
      predictor_names = predictor_names_simp,
      response_name = model_config$response_name,
      log_response = model_config$log_response,
      tc = preset$tc,
      lr = preset$lr,
      nt = nt
    )
    message("Inital BRT-model re-fit to simplified predictor set")
    gbm_step_simp
  }
  names(gbm_steps_simp) <- c("HDS_richness_BRT", "mean_QDS_turnover_BRT")
  for (i in seq_along(gbm_steps_simp)) {
    names(gbm_steps_simp[[i]]) <- c("Cape", "SWA")
  }
  message("Done (run_initial_BRTs())")
  gbm_steps_simp
}
