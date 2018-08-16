delta_AICc <- function(x, file = NULL) {
  stopifnot(exprs = {
    is.list(x)
    all(map_chr(x, class) == "gwr")
  })
  AICcs <- t(map_df(x, ~ .$results$AICc))
  delta_AICcs <- AICcs - min(AICcs)
  akaike_weights <- exp(-0.5 * delta_AICcs) / sum(exp(-0.5 * delta_AICcs))
  out <-
    data.frame(
      model = rownames(delta_AICcs),
      AICc = AICcs,
      delta_AICc = delta_AICcs[, 1],
      akaike_weights = akaike_weights
    ) %>%
    arrange(delta_AICc)
  if (!is.null(file)) {
    write_csv(out, file)
  }
  out
}
