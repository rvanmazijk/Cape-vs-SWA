# First attempt at geographically weighted regressions (GWRs)
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
map(pre_analysis_import_paths, source)
pacman::p_load(spgwr)

for (i in seq_along(GCFR_variables_QDS)) {
  names(GCFR_variables_QDS[[i]]) <- var_names[[i]]
}
names(GCFR_richness_QDS) <- "richness"
GCFR_roughness_QDS <- map(GCFR_variables_QDS, focal_sd)
names(GCFR_roughness_QDS) %<>% paste0("rough_", .)
for (i in seq_along(GCFR_variables_QDS)) {
  names(GCFR_roughness_QDS[[i]]) <- paste0("rough_", var_names[[i]])
}
for (i in seq_along(SWAFR_variables_QDS)) {
  names(SWAFR_variables_QDS[[i]]) <- var_names[[i]]
}
names(SWAFR_richness_QDS) <- "richness"
SWAFR_roughness_QDS <- map(SWAFR_variables_QDS, focal_sd)
names(SWAFR_roughness_QDS) %<>% paste0("rough_", .)
for (i in seq_along(SWAFR_variables_QDS)) {
  names(SWAFR_roughness_QDS[[i]]) <- paste0("rough_", var_names[[i]])
}

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
  coords = na.omit(GCFR_all_QDS_df)[,  c(1, 2)]
)
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
  coords = na.omit(SWAFR_all_QDS_df)[,  c(1, 2)]
)

gwr_model <- function(formula = richness ~ ., data, columns = c(1:nlayers(data)),
                      r, null = FALSE) {
  if (null) {
    formula <- richness ~ 1
  }
  auto_bw <- gwr.sel(
    formula, data[columns],
    gweight = gwr.Gauss, verbose = FALSE
  )
  print(glue(
    "Bandwidth automatically chosen"
  ))
  model_gwr <- gwr(
    formula, data[columns],
    gweight = gwr.Gauss, bandwidth = auto_bw, hatmatrix = TRUE
  )
  print(glue(
    "GWR model fit"
  ))
  model_gwr$raster <- rasterize(model_gwr$SDF, r)
  print(glue(
    "Rasterised results"
  ))
  model_gwr
}
data <- GCFR_all_QDS_pts
r <- GCFR_richness_QDS
GCFR_models <- list(
  null     = gwr_model(data = data, r = r, null = TRUE),
  abs      = gwr_model(data = data, r = r, columns = c(1, 2:10)),
  rough    = gwr_model(data = data, r = r, columns = c(1, 11:19)),
  elev     = gwr_model(data = data, r = r, columns = c(1, 2, 11)),
  non_elev = gwr_model(data = data, r = r, columns = -c(2, 11)),
  soil     = gwr_model(data = data, r = r, columns = c(1, 7:10, 16:19)),
  non_soil = gwr_model(data = data, r = r, columns = -c(7:10, 16:19)),
  full     = gwr_model(data = data, r = r, null = FALSE)
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

plot(models$elev$raster$X.Intercept.)
plot(models$elev$raster$Elevation)
plot(models$elev$raster$rough_Elevation)
