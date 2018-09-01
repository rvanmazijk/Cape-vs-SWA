fit_gbm_step <- function(variables, predictor_names,
                         response_name,
                         log_response = TRUE, permuted_BRT = FALSE,
                         tc, lr, nt) {
  # Convenience function to fit and/or refit a BRT model
  stopifnot(exprs = {
    class(variables) == "RasterStack"
    is.character(predictor_names)
  })
  variables %<>%
    as.data.frame() %>%
    na.exclude()
  if (permuted_BRT) {
    variables %<>% permute_response(response_name)
  }
  if (log_response) {
    variables[[response_name]] %<>% log()
  }
  gbm.step(
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
  gbm_simp <- gbm.simplify(x)
  # Drop as many variables as can if multiple nos. of drops are optimal
  # (hence (max(which(mean == min(mean)))))
  optimal_no_drops <- gbm_simp %$%
    deviance.summary %$%
    which(mean == min(mean)) %>%
    max()
  gbm_simp$pred.list[[optimal_no_drops]]
}

pseudo_r2 <- function(x) {
  # Get the pseudo-R^2 of a gbm model
  stopifnot(class(x) == "gbm")
  x %$% {
    1 - (cv.statistics$deviance.mean / self.statistics$mean.null)
  }
}

my_BRT_summary <- function(x) {
  # Gives the nt, pseudo-R^2 and variables' contributions for a BRT
  stopifnot(class(x) == "gbm")
  list(
    nt = x$n.trees,
    pseudo_r2 = pseudo_r2(x),
    contribs = summary(x)
  )
}

permute_vector <- function(x) {
  # Shuffles positions of values in vector
  x[sample(length(x))]
}

permute_wo_nas <- function(x) {
  # Shuffles positions of values in vector,
  # but keeps NAs in their starting positions
  x[!is.na(x)] %<>% permute_vector()
  x
}
