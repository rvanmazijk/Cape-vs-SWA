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

# Fit models if not already done

GCFR_models_path <- glue("{out_dir}/GCFR_models.RDS")
if (!file.exists(GCFR_models_path)) {
  set.seed(1234)
  GCFR_models <- map(.x = model_specs,
    .f = ~ gwr_model(
      data = GCFR_all_QDS_pts,
      columns = .x,
      rasterize_with = GCFR_richness_QDS
    )
  )
  write_rds(GCFR_models, GCFR_models_path)
} else {
  GCFR_models <- read_rds(GCFR_models_path)
}

SWAFR_models_path <- glue("{out_dir}/SWAFR_models.RDS")
if (!file.exists(SWAFR_models_path)) {
  set.seed(1234)
  SWAFR_models <- map(.x = model_specs,
    .f = ~ gwr_model(
      data = SWAFR_all_QDS_pts,
      columns = .x,
      rasterize_with = SWAFR_richness_QDS
    )
  )
  write_rds(SWAFR_models, SWAFR_models_path)
} else {
  SWAFR_models <- read_rds(SWAFR_models_path)
}
# NOTE:
# - non_elev: richness ~ soil + climate + ndvi + roughnesses thereof
# - non_soil: richness ~ elev + climate + ndvi + roughnesses thereof
# - etc.

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

# Fit models if not already done

combined_models_path <- glue("{out_dir}/combined_models.RDS")
if (!file.exists(combined_models_path)) {
  set.seed(1234)
  foo <- map(.x = model_specs,
    .f = ~ gwr_model(
      data = BOTH_all_QDS_pts,
      columns = .x
    )
  )
  write_rds(combined_models, combined_models_path)
} else {
  combined_models <- read_rds(combined_models_path)
}

# Interpret models -------------------------------------------------------------

# .... Separate regions' models ------------------------------------------------

# Overall "ranking" of model fits
delta_AICc(
  GCFR_models,
  glue("{out_dir}/GCFR_models_AICc_all.csv")
)
delta_AICc(
  SWAFR_models,
  glue("{out_dir}/SWAFR_models_AICc_all.csv")
)

# Specific model comparisons
abs_vs_rough     <- c("null", "abs",  "rough",    "full")
elev_vs_non_elev <- c("null", "elev", "non_elev", "full")
soil_vs_non_soil <- c("null", "soil", "non_soil", "full")
delta_AICc(
  GCFR_models[abs_vs_rough],
  glue("{out_dir}/GCFR_models_AICc_abs_vs_rough.csv")
)
delta_AICc(
  SWAFR_models[abs_vs_rough],
  glue("{out_dir}/SWAFR_models_AICc_abs_vs_rough.csv")
)
delta_AICc(
  GCFR_models[elev_vs_non_elev],
  glue("{out_dir}/GCFR_models_AICc_elev_vs_non_elev.csv")
)
delta_AICc(
  SWAFR_models[elev_vs_non_elev],
  glue("{out_dir}/SWAFR_models_AICc_elev_vs_non_elev.csv")
)
delta_AICc(
  GCFR_models[soil_vs_non_soil],
  glue("{out_dir}/GCFR_models_AICc_soil_vs_non_soil.csv")
)
delta_AICc(
  SWAFR_models[soil_vs_non_soil],
  glue("{out_dir}/SWAFR_models_AICc_soil_vs_non_soil.csv")
)

# .... Combined regions' models ------------------------------------------------

delta_AICc(
  combined_models,
  glue("{out_dir}/combined_models_AICc_all.csv")
)
delta_AICc(
  combined_models[abs_vs_rough],
  glue("{out_dir}/combined_models_AICc_abs_vs_rough.csv")
)
delta_AICc(
  combined_models[elev_vs_non_elev],
  glue("{out_dir}/combined_models_AICc_elev_vs_non_elev.csv")
)
delta_AICc(
  combined_models[soil_vs_non_soil],
  glue("{out_dir}/combined_models_AICc_soil_vs_non_soil.csv")
)

# Output all model outputs -----------------------------------------------------

GCFR_models_outputs <- map_df(
  .x = GCFR_models,
  .id = "model",
  .f = function(.x) {
    model_outputs <- .x$SDF@data
    model_outputs <- cbind(
      obs_log_richness = GCFR_all_QDS_pts@data$richness,
      model_outputs
    )
    names(model_outputs)[names(model_outputs) == "pred.se"][[2]] <- "pred.se2"
    model_outputs %<>%
      as_tibble() %>%
      mutate(
        obs_richness = exp(obs_log_richness),
        pred_richness = exp(pred)
      )
  }
)
GCFR_models_outputs$region <- "GCFR"

SWAFR_models_outputs <- map_df(
  .x = SWAFR_models,
  .id = "model",
  .f = function(.x) {
    model_outputs <- .x$SDF@data
    model_outputs <- cbind(
      obs_log_richness = SWAFR_all_QDS_pts@data$richness,
      model_outputs
    )
    names(model_outputs)[names(model_outputs) == "pred.se"][[2]] <- "pred.se2"
    model_outputs %<>%
      as_tibble() %>%
      mutate(
        obs_richness = exp(obs_log_richness),
        pred_richness = exp(pred)
      )
  }
)
SWAFR_models_outputs$region <- "SWAFR"

combined_models_outputs <- map_df(
  .x = combined_models,
  .id = "model",
  .f = function(.x) {
    model_outputs <- .x$SDF@data
    model_outputs <- cbind(
      obs_log_richness = BOTH_all_QDS_pts@data$richness,
      region = BOTH_all_QDS_pts@data$region,
      model_outputs
    )
    names(model_outputs)[names(model_outputs) == "pred.se"][[2]] <- "pred.se2"
    model_outputs %<>%
      as_tibble() %>%
      mutate(
        region = case_when(
          region == "GCFR" ~ "Cape",
          region == "SWAFR" ~ "SWA"
        ),
        obs_richness = exp(obs_log_richness),
        pred_richness = exp(pred)
      )
  }
)

all_GWR_models_outputs <- bind_rows(
  list(
    GCFR = GCFR_models_outputs,
    SWAFR = SWAFR_models_outputs,
    both = combined_models_outputs
  ),
  .id = "model_region"
)
write_csv(
  all_GWR_models_outputs,
  glue("{out_dir}/all_GWR_models_outputs.csv")
)
