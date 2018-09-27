# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---
#   using bare-minimum BRTs on the UCT HPC
# Part 1: Fitting BRT models with different tc and lr settings
# Cape vs SWA publication
# Ruan van Mazijk

source("setup.R")

# Define what's going to be run in each parallel branch ------------------------

run_initial_BRTs <- function(preset,
                             response_names = list("HDS_richness",
                                                   "mean_QDS_turnover"),
                             log_response_options = list(TRUE, FALSE),
                             regions_variables = list(GCFR_variables_HDS,
                                                      SWAFR_variables_HDS),
                             regions_predictor_names = list(GCFR_predictor_names,
                                                            SWAFR_predictor_names)) {
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
  gbm_steps_simp
}

# Choose nt, tc, lr ------------------------------------------------------------

# Values of tc and lr to loop through:
tc <- 1:5  # No more than 5-way interactions
lr <- c(  # A range of learning rates
  0.01,
  0.005,
  0.001,
  0.0005,
  0.0001
)

nt <- 10000  # Maximum no. trees allowed in a BRT-model

# Make tc and lr values in little preset-lists
presets <- as.list(as.data.frame(t(expand.grid(
  tc = tc,
  lr = lr,
  KEEP.OUT.ATTRS = FALSE
))))
names(presets) <- NULL
for (i in seq_along(presets)) {
  presets[[i]] <- as.list(presets[[i]])
  names(presets[[i]]) <- c("tc", "lr")
}

rm(tc, lr)  # Avoids clashes when running run_BRTs()

# Run run_BRTs() in parallel for all presets of tc and lr ----------------------

library(doParallel)
registerDoParallel(cores = detectCores())
foreach(preset = presets) %dopar% {
  set.seed(1234)
  model_code <- paste0(
    "worker-", Sys.getpid(),
    "_tc-", preset$tc,
    "_lr-", preset$lr,
    Sys.Date()
  )
  capture.output(
    gbm_steps_simp <- run_initial_BRTs(preset),  # To allow BRT outputs to return
    file = paste0("all-tc-lr-BRTs", model_code, "_log.txt"),
    append = FALSE
  )
  saveRDS(
    gbm_steps_simp,
    paste0("all-tc-lr-BRTs", model_code, "_BRTs.RDS")
  )
}

