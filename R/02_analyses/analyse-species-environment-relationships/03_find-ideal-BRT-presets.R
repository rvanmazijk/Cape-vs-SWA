# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 5: Fitting BRT models to randomly permuted datasets
# Cape vs SWA publication
# Ruan van Mazijk

library(doParallel)

set.seed(1234)

# Define what's going to be run in each parallel branch ------------------------

run_initial_BRTs <- function(preset) {
  stopifnot(is.list(preset))

  # Run BRT fit-simplify-refit -------------------------------------------------

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

          # Initial model fitting: gbm.step(richness ~ ...) --------------------

          gbm_step <- fit_gbm_step(
            variables = .variables, predictor_names = .predictor_names,
            response_name = .response_name, log_response = .log_response,
            tc = preset$tc, lr = preset$lr, nt = nt
          )

          if (is.null(gbm_step)) {
            # Catch when BRT cannot fit because lr or step-size too fast
            # or inappropriate
            return("failed")
          } else {
            message("Inital BRT-model fit")

            # Model simplification: gbm.simplify(...) --------------------------

            predictor_names_simp <- simplify_predictors(gbm_step)

            message("Simpler predictor set found")

            # Refitting models with simplified predictor sets ------------------

            gbm_step_simp <- fit_gbm_step(
              variables = .variables, predictor_names = predictor_names_simp,
              response_name = .response_name, log_response = .log_response,
              tc = preset$tc, lr = preset$lr, nt = nt
            )

            message("Inital BRT-model re-fit to simplified predictor set")

            return(gbm_step_simp)
          }
        }
      )
    }
  )

  names(gbm_steps_simp) <- c("HDS_richness_BRT", "mean_QDS_turnover_BRT")
  for (i in seq_along(gbm_steps_simp)) {
    names(gbm_steps_simp[[i]]) <- c("Cape", "SWA")
  }

  gbm_steps_simp
}

# Test
if (FALSE) {
  nt <- 10000
  foo <- run_initial_BRTs(preset = list(tc = 5, lr = 0.001))
}
# Works

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
presets <-
  expand.grid(tc = tc, lr = lr, KEEP.OUT.ATTRS = FALSE) %>%
  t() %>%
  as.data.frame() %>%
  as.list()
names(presets) <- NULL
for (i in seq_along(presets)) {
  presets[[i]] %<>% as.list()
  names(presets[[i]]) <- c("tc", "lr")
}

rm(tc, lr)  # Avoids clashes when running run_BRTs()

# Run run_BRTs() in parallel for all presets of tc and lr ----------------------

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
all_tc_lr_gbm_steps_simp <- foreach(preset = presets) %dopar% {

  # Load packages --------------------------------------------------------------
  # doParallel:: and parallel:: don't have access to the global environment's
  # loaded packages, so reload packages as needed here.

  library(here)
  library(magrittr)
  library(purrr)
  library(glue)
  library(dismo)
  library(gbm)

  # Run run_BRTs() -------------------------------------------------------------
  # Capturing each worker-core's outputs in separate text files

  capture.output(
    gbm_steps_simp <- run_initial_BRTs(preset),  # To allow BRT outputs to return
    file = glue(
      "{here('outputs/species-environment-relationships/all-tc-lr-BRTs_')}\\
      worker-{Sys.getpid()}-tc-{preset$tc}-lr-{preset$lr}-log_{Sys.Date()}.txt"
    ),
    append = FALSE
  )
  gbm_steps_simp
}
stopCluster(cluster)
