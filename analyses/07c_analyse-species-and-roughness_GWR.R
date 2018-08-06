# Geographically weighted regressions of *log* species richness
# as a function of environment and environmental heterogeneity
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
map(pre_analysis_import_paths, source)

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
  richness = log(GCFR_richness_QDS),
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
  richness = log(SWAFR_richness_QDS),
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

model_specs <- list(
  null     =  c(NULL),
  abs      =  c(1, 2:10),
  rough    =  c(1, 11:19),
  elev     =  c(1, 2, 11),
  non_elev = -c(2, 11),
  soil     =  c(1, 7:10, 16:19),
  non_soil = -c(7:10, 16:19),
  full     =  c("all")
)
GCFR_models <- map(.x = model_specs,
  .f = ~ gwr_model(
    data = GCFR_all_QDS_pts,
    columns = .x,
    rasterize_with = GCFR_richness_QDS
  )
)
SWAFR_models <- map(.x = model_specs,
  .f = ~ gwr_model(
    data = SWAFR_all_QDS_pts,
    columns = .x,
    rasterize_with = SWAFR_richness_QDS
  )
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
