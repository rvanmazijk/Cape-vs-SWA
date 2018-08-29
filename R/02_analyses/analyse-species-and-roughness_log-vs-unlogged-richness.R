# Geographically weighted regressions of *log* species richness
# as a function of environment and environmental heterogeneity
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
map(data_import_paths, source)

out_dir <- here::here("outputs/species-and-roughness")

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
  log_richness = log(GCFR_richness_QDS),
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
  "log_richness",
  names(GCFR_variables),
  names(GCFR_roughness_QDS)
)
GCFR_all_QDS_pts <- SpatialPointsDataFrame(
  data   = na.omit(GCFR_all_QDS_df)[, -c(1, 2)],
  coords = na.omit(GCFR_all_QDS_df)[,  c(1, 2)],
  proj4string = CRS(std_CRS)
)
# Adjust environmental values stored at x10 etc.
GCFR_all_QDS_pts@data %<>% mutate(
  `Surface T` = `Surface T` - 273.15, # no adj needed for rough, as additive
  NDVI        = NDVI / 1e+07,
  rough_NDVI  = rough_NDVI / 1e+07,
  pH          = pH / 10,
  rough_pH    = rough_pH / 10
)
# TODO: Check which other vars have been x10
# TODO: Check units for NDVI

# SWAFR
SWAFR_all_QDS <- c(
  richness = SWAFR_richness_QDS,
  log_richness = log(SWAFR_richness_QDS),
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
  "log_richness",
  names(SWAFR_variables),
  names(SWAFR_roughness_QDS)
)
SWAFR_all_QDS_pts <- SpatialPointsDataFrame(
  data   = na.omit(SWAFR_all_QDS_df)[, -c(1, 2)],
  coords = na.omit(SWAFR_all_QDS_df)[,  c(1, 2)],
  proj4string = CRS(std_CRS)
)
# Adjust environmental values stored at x10 etc.
SWAFR_all_QDS_pts@data %<>% mutate(
  `Surface T` = `Surface T` - 273.15, # no adj needed for rough, as additive
  NDVI        = NDVI / 1e+07,
  rough_NDVI  = rough_NDVI / 1e+07,
  pH          = pH / 10,
  rough_pH    = rough_pH / 10
)
# TODO: Check which other vars have been x10
# TODO: Check units for NDVI

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
# Adjust environmental values stored at x10 etc.
BOTH_all_QDS_pts@data %<>% mutate(
  `Surface T` = `Surface T` - 273.15, # no adj needed for rough, as additive
  NDVI        = NDVI / 1e+07,
  rough_NDVI  = rough_NDVI / 1e+07,
  pH          = pH / 10,
  rough_pH    = rough_pH / 10
)
# TODO: Check which other vars have been x10
# TODO: Check units for NDVI

# Fit models -------------------------------------------------------------------

# .... Separate regions' models ------------------------------------------------

set.seed(1234)
null_GCFR_model <- gwr_model(
  data = GCFR_all_QDS_pts,
  formula_override = "richness ~ .",
  columns = -2
)
log_null_GCFR_model <- gwr_model(
  data = GCFR_all_QDS_pts,
  formula_override = "log_richness ~ .",
  columns = -1
)
null_SWAFR_model <- gwr_model(
  data = SWAFR_all_QDS_pts,
  formula_override = "richness ~ .",
  columns = -2
)
log_null_SWAFR_model <- gwr_model(
  data = SWAFR_all_QDS_pts,
  formula_override = "log_richness ~ .",
  columns = -1
)
null_BOTH_model <- gwr_model(
  data = BOTH_all_QDS_pts,
  formula_override = "richness ~ .",
  columns = c(-1, -3)
)
log_null_BOTH_model <- gwr_model(
  data = BOTH_all_QDS_pts,
  formula_override = "log_richness ~ .",
  columns = c(-1, -2)
)
delta_AICc(list(
  null = null_GCFR_model,
  log_null = log_null_GCFR_model
))
delta_AICc(list(
  null = null_SWAFR_model,
  log_null = log_null_SWAFR_model
))
delta_AICc(list(
  null = null_BOTH_model,
  log_null = log_null_BOTH_model
))

map(
  .x = list(
    GCFR_all_QDS_pts,
    SWAFR_all_QDS_pts,
    BOTH_all_QDS_pts
  ),
  .f = function(.x) {
    hist(.x@data$richness)
    hist(log(.x@data$richness))
  }
)

# All AICc support log-richness... Histograms show its obvious skew
# Log-richness it is then!
