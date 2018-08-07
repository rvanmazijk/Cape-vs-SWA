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
