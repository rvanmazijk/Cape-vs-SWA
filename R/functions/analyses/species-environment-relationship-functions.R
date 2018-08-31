pseudo_r2 <- function(x) {
  # Get the pseudo-R^2 of a gbm model
  stopifnot(class(x) == "gbm")
  x %$% {
    1 - (cv.statistics$deviance.mean / self.statistics$mean.null)
  }
}

fit_gbm_step <- function(variables, predictor_names,
                         response_name, log_response = TRUE,
                         tc, lr, nt) {
  # Convenience function to fit and/or refit a BRT model
  stopifnot(exprs = {
    class(variables) == "RasterStack"
    is.character(predictor_names)
  })
  set.seed(1234)
  variables %<>%
    as.data.frame() %>%
    na.exclude()
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
