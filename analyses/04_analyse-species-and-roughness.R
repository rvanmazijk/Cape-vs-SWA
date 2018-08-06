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
set.seed(1234)
GCFR_models <- map(.x = model_specs,
  .f = ~ gwr_model(
    data = GCFR_all_QDS_pts,
    columns = .x,
    rasterize_with = GCFR_richness_QDS
  )
)
set.seed(1234)
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
delta_AICc(GCFR_models[c( "null", "abs",  "rough",    "full")])
delta_AICc(SWAFR_models[c("null", "abs",  "rough",    "full")])
delta_AICc(GCFR_models[c( "null", "elev", "non_elev", "full")])
delta_AICc(SWAFR_models[c("null", "elev", "non_elev", "full")])
delta_AICc(GCFR_models[c( "null", "soil", "non_soil", "full")])
delta_AICc(SWAFR_models[c("null", "soil", "non_soil", "full")])

# TODO: interpretation of this?
map(GCFR_models, anova)
map(SWAFR_models, anova)

#### WIP
spplot(
  GCFR_models$elev$SDF["Elevation"], scales = list(draw = TRUE),
  sp.layout = list(GCFR_border_buffered)
)
spplot(
  SWAFR_models$elev$SDF["Elevation"], scales = list(draw = TRUE),
  sp.layout = list(SWAFR_border_buffered)
)
####

# .... Combined regions' models ------------------------------------------------

# Note, "region" column is simply to aid visualisation later.
# GWR cannot actually handle a discrete term directly (hence -1 for
# some model specs) but the spatial covariance matrix means that we can see
# how different the local coefficients turn out to be estimated w/i each region.

model_specs <- list(
  null     =  c(NULL),
  abs      =  c(2, 3:11),
  rough    =  c(2, 12:20),
  elev     =  c(2, 3, 12),
  non_elev = -c(1, 3, 12),
  soil     =  c(2, 8:11, 17:20),
  non_soil = -c(1, 8:11, 17:20),
  full     = -c(1)
)
set.seed(1234)
combined_models <- map(.x = model_specs,
  .f = ~ gwr_model(
    data = BOTH_all_QDS_pts,
    columns = .x
  )
)

delta_AICc(combined_models)

delta_AICc(combined_models[c("null", "abs",  "rough",    "full")])
delta_AICc(combined_models[c("null", "elev", "non_elev", "full")])
delta_AICc(combined_models[c("null", "soil", "non_soil", "full")])

full_coeff <- combined_models$full$SDF@data
full_coeff <- cbind(region = BOTH_all_QDS_pts@data$region, full_coeff)
names(full_coeff)[[70]] <- "pred.se2"
full_coeff %<>% as_tibble()

# Compare coefficients of absolute variables between regions
full_coeff %>%
  select(region, Elevation:pH) %>%
  gather(term, est, -region) %>%
  ggplot(aes(est, col = region, fill = region)) +
    geom_histogram(alpha = 0.5) +
    scale_colour_manual(values = my_palette) +
    scale_fill_manual(values = my_palette) +
    facet_wrap(~ term, scales = "free")

# Compare coefficients of roughness variables between regions
full_coeff %>%
  select(region, rough_Elevation:rough_pH) %>%
  gather(term, est, -region) %>%
  ggplot(aes(est, col = region, fill = region)) +
    geom_histogram(alpha = 0.5) +
    scale_colour_manual(values = my_palette) +
    scale_fill_manual(values = my_palette) +
    facet_wrap(~ term, scales = "free")

# TODO: what do all the *other* columns in the GWR output mean?
#   --> DO SOME READING
# TODO: make this sort of graphical comparison between
#   the separate models' estimates
