# First attempt at geographically weighted regressions (GWRs)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
map(pre_analysis_import_paths, source)
pacman::p_load(spgwr, GWmodel)

# Collate data -----------------------------------------------------------------

# Prepare richness, environment roughness layers
# GCFR
for (i in seq_along(GCFR_variables_QDS)) {
  names(GCFR_variables_QDS[[i]]) <- var_names[[i]]
}
names(GCFR_richness_QDS) <- "richness"
GCFR_roughness_QDS <- map(GCFR_variables_QDS, focal_sd)
names(GCFR_roughness_QDS) %<>% paste0("rough_", .)
for (i in seq_along(GCFR_roughness_QDS)) {
  names(GCFR_roughness_QDS[[i]]) <- paste0("rough_", var_names[[i]])
}
# SWAFR
for (i in seq_along(SWAFR_variables_QDS)) {
  names(SWAFR_variables_QDS[[i]]) <- var_names[[i]]
}
names(SWAFR_richness_QDS) <- "richness"
SWAFR_roughness_QDS <- map(SWAFR_variables_QDS, focal_sd)
names(SWAFR_roughness_QDS) %<>% paste0("rough_", .)
for (i in seq_along(SWAFR_roughness_QDS)) {
  names(SWAFR_roughness_QDS[[i]]) <- paste0("rough_", var_names[[i]])
}

# Combine richness, environment and roughness into SpatialPointsDataFrames
# GCFR
GCFR_all_QDS <- c(
  richness = GCFR_richness_QDS,
  map(GCFR_variables_QDS, na.omit),
  GCFR_roughness_QDS
)
GCFR_all_QDS_df <- GCFR_all_QDS %>%
  map(rasterToPoints) %>%
  map(as_tibble) %>%
  reduce(right_join)
names(GCFR_all_QDS_df) <- c(
  "x", "y",
  "richness",
  names(GCFR_variables),
  names(GCFR_roughness_QDS)
)
GCFR_all_QDS_pts <- SpatialPointsDataFrame(
  data   = na.omit(GCFR_all_QDS_df)[, -c(1, 2)],
  coords = na.omit(GCFR_all_QDS_df)[,  c(1, 2)],
  proj4string = CRS(std_CRS)
)
# SWAFR
SWAFR_all_QDS <- c(
  richness = SWAFR_richness_QDS,
  map(SWAFR_variables_QDS, na.omit),
  SWAFR_roughness_QDS
)
SWAFR_all_QDS_df <- SWAFR_all_QDS %>%
  map(rasterToPoints) %>%
  map(as_tibble) %>%
  reduce(right_join)
names(SWAFR_all_QDS_df) <- c(
  "x", "y",
  "richness",
  names(SWAFR_variables),
  names(SWAFR_roughness_QDS)
)
SWAFR_all_QDS_pts <- SpatialPointsDataFrame(
  data   = na.omit(SWAFR_all_QDS_df)[, -c(1, 2)],
  coords = na.omit(SWAFR_all_QDS_df)[,  c(1, 2)],
  proj4string = CRS(std_CRS)
)
# Combined
BOTH_all_QDS_pts <- SpatialPointsDataFrame(
  data = rbind(
    cbind(region = "GCFR",  na.omit(GCFR_all_QDS_df)[, -c(1, 2)]),
    cbind(region = "SWAFR", na.omit(SWAFR_all_QDS_df)[, -c(1, 2)])
  ),
  coords = rbind(
    na.omit(GCFR_all_QDS_df)[,  c(1, 2)],
    na.omit(SWAFR_all_QDS_df)[,  c(1, 2)]
  ),
  proj4string = CRS(std_CRS)
)

# GWR helper function ----------------------------------------------------------

gwr_model <- function(pkg, null_model = TRUE, formula = richness ~ .,
                      data, columns = NULL,
                      rasterize_with = NULL) {
  stopifnot(exprs = {
    pkg %in% c("spgwr", "GWmodel")
    class(data) == "SpatialPointsDataFrame"
  })
  if (null_model) {
    formula <- richness ~ 1
  } else {
    if (is.null(columns)) {
      print(glue(
        "Defaulting to full model"
      ))
      columns <- c(1:nlayers(data)
    } else {
      print(glue(
        "Using columns {columns}"
      ))
    }
  }
  switch(pkg,
    "spgwr" = {
      auto_bw <- spgwr::gwr.sel(
        formula, data[columns],
        gweight = gwr.Gauss, verbose = TRUE
      )
      print(glue(
        "Bandwidth automatically chosen"
      ))
      model_gwr <- spgwr::gwr(
        formula, data[columns],
        gweight = gwr.Gauss, bandwidth = auto_bw, hatmatrix = TRUE
      )
      print(glue(
        "GWR model fit"
      ))
    },
    "GWmodel" = {
      # TODO
    }
  )
  if (!is.null(rasterize_with)) {
    model_gwr$raster <- rasterize(model_gwr$SDF, rasterize_with)
    print(glue(
      "Rasterised results"
    ))
  }
  model_gwr
}

# Fit models -------------------------------------------------------------------

# .... Separate regions' models ------------------------------------------------

d <- GCFR_all_QDS_pts
r <- GCFR_richness_QDS
GCFR_models <- list(
  null = gwr_model(
    pkg = "spgwr", data = d, rasterize_with = r,
    null_model = TRUE
  ),
  abs = gwr_model(
    pkg = "spgwr", data = d, rasterize_with = r,
    columns = c(1, 2:10)
  ),
  rough = gwr_model(
    pkg = "spgwr", data = d, rasterize_with = r,
    columns = c(1, 11:19)
  ),
  elev = gwr_model(
    pkg = "spgwr", data = d, rasterize_with = r,
    columns = c(1, 2, 11)
  ),
  non_elev = gwr_model(
    pkg = "spgwr", data = d, rasterize_with = r,
    columns = -c(2, 11)
  ),
  soil = gwr_model(
    pkg = "spgwr", data = d, rasterize_with = r,
    columns = c(1, 7:10, 16:19)
  ),
  non_soil = gwr_model(
    pkg = "spgwr", data = d, rasterize_with = r,
    columns = -c(7:10, 16:19)
  ),
  full = gwr_model(
    pkg = "spgwr", data = d, rasterize_with = r,
    null_model = FALSE
  )
)
data <- SWAFR_all_QDS_pts
r <- SWAFR_richness_QDS
SWAFR_models <- list(
  null     = gwr_model(data = data, r = r, null = TRUE),
  abs      = gwr_model(data = data, r = r, columns = c(1, 2:10)),
  rough    = gwr_model(data = data, r = r, columns = c(1, 11:19)),
  elev     = gwr_model(data = data, r = r, columns = c(1, 2, 11)),
  non_elev = gwr_model(data = data, r = r, columns = -c(2, 11)),
  soil     = gwr_model(data = data, r = r, columns = c(1, 7:10, 16:19)),
  non_soil = gwr_model(data = data, r = r, columns = -c(7:10, 16:19)),
  full     = gwr_model(data = data, r = r, null = FALSE)
)
# NOTE:
# - non_elev: richness ~ soil + climate + ndvi + roughnesses thereof
# - non_soil: richness ~ elev + climate + ndvi + roughnesses thereof
# - etc.

# Overall "ranking" of model fits
delta_AICc(GCFR_models)
delta_AICc(SWAFR_models)
# Specific model comparisons
delta_AICc(GCFR_models[c("null", "abs",  "rough",    "full")])
delta_AICc(SWAFR_models[c("null", "abs",  "rough",    "full")])
delta_AICc(GCFR_models[c("null", "elev", "non_elev", "full")])
delta_AICc(SWAFR_models[c("null", "elev", "non_elev", "full")])
delta_AICc(GCFR_models[c("null", "soil", "non_soil", "full")])
delta_AICc(SWAFR_models[c("null", "soil", "non_soil", "full")])
map(GCFR_models, anova)  # TODO: interpretation of this?
map(SWAFR_models, anova)

####
spplot(
  GCFR_models$elev$SDF["Elevation"], scales = list(draw = TRUE),
  sp.layout = list(GCFR_border_buffered)
)
spplot(
  SWAFR_models$elev$SDF["Elevation"], scales = list(draw = TRUE),
  sp.layout = list(SWAFR_border_buffered)
)
####

# .... Region-term models ------------------------------------------------------

data <- BOTH_all_QDS_pts

# Code two binary variables scoring region membership
data$isGCFR <- ifelse(data$region == "GCFR", 1, 0)
data$isSWAFR <- ifelse(data$region == "SWAFR", 1, 0)
data$region <- NULL

models2 <- list(
  both_null      = gwr_model(data = data, null = TRUE),
  both_region    = gwr_model(data = data, columns = c(1, 20, 21)),
  both_no_region = gwr_model(data = data, columns = -c(20, 21)),
  both_full      = gwr_model(data = data, null = FALSE)
)
delta_AICc(models2)

both_region$SDF$region <- ifelse(both_region$SDF@coords[, 1] < 60, "GCFR", "SWAFR")

str(GCFR_all_QDS_pts)
str(both_region$SDF[1])
spplot(both_region$SDF[both_region$SDF$region == "GCFR",  "Elevation"], scales = list(draw = TRUE))
spplot(both_region$SDF[both_region$SDF$region == "SWAFR", "Elevation"], scales = list(draw = TRUE))
spplot(both_region$SDF[both_region$SDF$region == "SWAFR",  "MAP"], scales = list(draw = TRUE))
spplot(both_region$SDF[both_region$SDF$region == "SWAFR", "Elevation"], scales = list(draw = TRUE))
spplot(both_region$SDF["MAP"], scales = list(draw = TRUE))

# Not working right...
# TODO: Try w/ GWModel instead?
auto_bw <- bw.gwr(
  richness ~ region + Elevation, data = data,
  kernel = "gaussian"
)
model <- gwr.mixed(
  richness ~ region + Elevation, data,
  fixed.vars = "Elevation", kernel = "gaussian", bw = auto_bw
)
