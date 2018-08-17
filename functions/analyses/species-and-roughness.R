# GWR helper function
gwr_model <- function(data, columns = NULL, rasterize_with = NULL,
                      formula_override = NULL) {

  stopifnot(class(data) == "SpatialPointsDataFrame")

  # Set explanatory variables --------------------------------------------------

  if (!is.null(formula_override)) {
    message("Using manual formula")
    message(glue("Using columns {columns}"))
    formula <- formula_override
  } else {
    if (is.null(columns)) {
      message("Defaulting to null model")
      formula <- richness ~ 1
      columns <- c(1:nlayers(data))  # To prevent "unsupported index type NULL"
    } else if (columns == "all") {
      message("Defaulting to full model")
      formula <- richness ~ .
      columns <- c(1:nlayers(data))
    } else {
      message(glue("Using columns {columns}"))
      formula <- richness ~ .
    }
  }

  # Compute optimal bandwidth for kernels --------------------------------------

  auto_bw <- spgwr::gwr.sel(
    formula, data[, columns],
    gweight = gwr.Gauss, verbose = TRUE
  )

  # Fit model ------------------------------------------------------------------

  model_gwr <- spgwr::gwr(
    formula, data[, columns],
    gweight = gwr.Gauss, bandwidth = auto_bw, hatmatrix = TRUE
  )

  # Rasterise the SDF of coefficients, if asked for ----------------------------

  if (!is.null(rasterize_with)) {
    model_gwr$raster <- rasterize(model_gwr$SDF, rasterize_with)
    message("Rasterised results")
  }

  model_gwr

}

# Neatly get deltaAICc values and compare between GWR models
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
