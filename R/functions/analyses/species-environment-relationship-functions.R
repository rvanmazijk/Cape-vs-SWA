pseudo_r2 <- function(x) {
  # Get the pseudo-R^2 of a gbm model
  stopifnot(class(x) == "gbm")
  x %$% {
    1 - (cv.statistics$deviance.mean / self.statistics$mean.null)
  }
}
