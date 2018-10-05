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
library(doParallel)

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
  one_too_many_drops <- max(which(
    gbm_simp$deviance.summary$mean == min(gbm_simp$deviance.summary$mean)
  ))
  optimal_no_drops <- one_too_many_drops - 1
  message(paste("Optimal no. drops =", optimal_no_drops))
  message(paste("Final predictor list =", gbm_simp$pred.list))
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

run_final_BRTs <- function(preset, model) {
  # Analyse value of environmental & heterogeneity variables for predicting
  #   vascular plant species richness and turnover---
  #   using bare-minimum BRTs on the UCT HPC
  # Part 2:
  #   Fitting final BRT models with ideal tc and lr settings
  stopifnot(exprs = {
    is.list(preset)
    is.character(model)
  })

  # Describe all 4 models ------------------------------------------------------

  models <- list(
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

  model_code <- paste0(
    "final_", model,
    "_worker-", Sys.getpid(),
    "_tc", preset$tc,
    "_lr-", preset$lr,
    "_", Sys.Date()
  )

  # Run the specified model ----------------------------------------------------

  message(paste("Fitting inital BRT-model for", model))
  gbm_step <- fit_gbm_step(
    variables = models[[model]]$variables,
    predictor_names = models[[model]]$predictor_names,
    response_name = models[[model]]$response_name,
    log_response = models[[model]]$log_response,
    tc = preset$tc,
    lr = preset$lr,
    nt = nt
  )
  message(paste("Inital BRT-model fit for", model))

  predictor_names_simp <- simplify_predictors(gbm_step)
  message(paste("Simpler predictor set found for", model))

  message(paste("Re-fitting to simplified predictor set for", model))
  gbm_step_simp <- fit_gbm_step(
    variables = models[[model]]$variables,
    predictor_names = predictor_names_simp,
    response_name = models[[model]]$response_name,
    log_response = models[[model]]$log_response,
    tc = preset$tc,
    lr = preset$lr,
    nt = nt
  )
  message(paste("Re-fit to simplified predictor set for", model))

  message(paste("Done (run_final_BRTs()) for", model))
  gbm_step_simp
}

# Describe all 4 models outside of function body for use in run_permuted_BRTs()
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
permute_vector <- function(x) {
  # Shuffles positions of values in vector
  x[sample(length(x))]
}
permute_wo_nas <- function(x) {
  # Shuffles positions of values in vector,
  # but keeps NAs in their starting positions
  x[!is.na(x)] <- permute_vector(x[!is.na(x)])
  x
}
run_permuted_BRTs <- function(preset, model_config, model_config_name) {
  # Analyse value of environmental & heterogeneity variables for predicting
  #   vascular plant species richness and turnover---
  #   using bare-minimum BRTs on the UCT HPC
  # Part 3:
  #   Fitting BRT models to randomly permuted data,
  #   with ideal tc and lr settings
  stopifnot(exprs = {
    is.list(preset)
    is.list(model_config)
    is.character(model_config_name)
  })
  registerDoParallel(detectCores())
  foreach(i = 1:1000) %dopar% {
    model_code <- paste0(
      "permutation-", i,
      "_", model_config_name,
      "_worker-", Sys.getpid(),
      "_tc", preset$tc,
      "_lr-", preset$lr,
      "_", Sys.Date()
    )
    capture.output(file = paste0(model_code, "_log.txt"), append = FALSE, {
      message(paste(
        i, "- Permuting", model_config$response_name, "column (without NAs)"
      ))
      set.seed(i)
      model_config$variables[[model_config$response_name]] <- permute_wo_nas(
        model_config$variables[[model_config$response_name]]
      )
      message(paste(
        i, "-", model_config$response_name, "column permuted (without NAs)"
      ))
      message(paste(
        i, "- Writing permuted data CSV to disc"
      ))
      write.csv(
        model_config$variables,
        paste0("data_", model_code, ".csv")
      )
      message(paste(
        i, "- Permuted data CSV written to disc"
      ))
      message(paste(
        i, "- Fitting inital BRT-model for", model_config_name
      ))
      gbm_step <- fit_gbm_step(
        variables = model_config$variables,
        predictor_names = model_config$predictor_names,
        response_name = model_config$response_name,
        log_response = model_config$log_response,
        tc = preset$tc,
        lr = preset$lr,
        nt = nt
      )
      message(paste(
        i, "- Inital BRT-model fit for", model_config_name
      ))
      predictor_names_simp <- simplify_predictors(gbm_step)
      message(paste(
        i, "- Simpler predictor set found"
      ))
      message(paste(
        i, "- Re-fitting to simplified predictor set for", model_config_name
      ))
      gbm_step_simp <- fit_gbm_step(
        variables = model_config$variables,
        predictor_names = predictor_names_simp,
        response_name = model_config$response_name,
        log_response = model_config$log_response,
        tc = preset$tc,
        lr = preset$lr,
        nt = nt
      )
      message(paste(
        i, "- Re-fit to simplified predictor set for", model_config_name
      ))
      message(paste(
        i, "- Writing", model_config_name, "BRT RDS to disc"
      ))
      saveRDS(
        gbm_step_simp,
        paste0("BRT_", model_code, ".RDS")
      )
      message(paste(
        i, "-", model_config_name, "BRT RDS written to disc"
      ))
    })
  }
}
