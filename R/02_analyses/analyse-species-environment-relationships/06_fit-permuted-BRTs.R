# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 5: Fitting BRT models to randomly permuted datasets
# Cape vs SWA publication
# Ruan van Mazijk

library(doParallel)

set.seed(1234)

# Choose nt, tc, lr ------------------------------------------------------------

# Thumbsucks for now:
tc <- 5  # No more than 5-way interactions
lr <- 0.002
nt <- 10000  # Maximum no. trees allowed in a BRT-model

# Define BRT fit-simplify-refit procedure --------------------------------------
# To be run on each permuted dataset

run_permuted_BRTs <- function() {
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
          .variables = list(GCFR_variables_HDS_stack2, SWAFR_variables_HDS_stack2),
          # Use initial predictor sets from after checking for collinearity
          .predictor_names = list(GCFR_predictor_names, SWAFR_predictor_names)
        ),
        .f = function(.variables, .predictor_names) {

          # Initial model fitting: gbm.step(richness ~ ...) --------------------

          gbm_step <- fit_gbm_step(
            variables = .variables, predictor_names = .predictor_names,
            response_name = .response_name, log_response = .log_response,
            tc = tc, lr = lr, nt = nt
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
              tc = tc, lr = lr, nt = nt
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
  foo <- run_permuted_BRTs()
}
# Works

# Run run_BRTs() in parallel for different permuted datasets -------------------

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
permuted_gbm_steps_simp <- foreach(i = as.list(1:100)) %dopar% {

  # Load packages --------------------------------------------------------------
  # doParallel:: and parallel:: don't have access to the global environment's
  # loaded packages, so reload packages as needed here.

  library(here)
  library(magrittr)
  library(purrr)
  library(glue)
  library(dismo)
  library(gbm)

  # Permute response columns ---------------------------------------------------

  GCFR_variables_HDS_stack2 <- GCFR_variables_HDS_stack
  GCFR_variables_HDS_stack2$HDS_richness %<>% permute_wo_nas()
  GCFR_variables_HDS_stack2$mean_QDS_turnover %<>% permute_wo_nas()

  SWAFR_variables_HDS_stack2 <- SWAFR_variables_HDS_stack
  SWAFR_variables_HDS_stack2$HDS_richness %<>% permute_wo_nas()
  SWAFR_variables_HDS_stack2$mean_QDS_turnover %<>% permute_wo_nas()

  message("Permuted response columns")

  # Run run_BRTs() -------------------------------------------------------------
  # Capturing each worker-core's outputs in separate text files

  capture.output(
    gbm_steps_simp <- run_permuted_BRTs(), # To allow BRT outputs to return
    file = glue(
      "{here('outputs/species-environment-relationships/permuted-BRTs_')}\\
      worker-{Sys.getpid()}-{i}-log_{Sys.Date()}.txt"
    ),
    append = TRUE
  )
  gbm_steps_simp
}
stopCluster(cluster)
