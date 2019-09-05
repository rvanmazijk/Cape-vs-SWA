# Import environmental data ----------------------------------------------------

GCFR_file_names  <- glue("{data_dir}/GCFR_{var_names}_masked2.tif")
SWAFR_file_names <- glue("{data_dir}/SWAFR_{var_names}_masked2.tif")

GCFR_variables  <- stack(GCFR_file_names)
SWAFR_variables <- stack(SWAFR_file_names)

names(GCFR_variables)  <- str_replace_all(var_names, " ", "_")
names(SWAFR_variables) <- str_replace_all(var_names, " ", "_")

# Resample environmental data from 0.05 x 0.05 to EDS --------------------------

GCFR_EDS_template_raster <- GCFR_variables$Elevation %>%
  aggregate(fact = 5) %>%  # aggregate up to QDS
  disaggregate(fact = 2)   # disaggregate down to EDS
SWAFR_EDS_template_raster <- SWAFR_variables$Elevation %>%
  aggregate(fact = 5) %>%
  disaggregate(fact = 2)

GCFR_variables_EDS <- GCFR_variables %>%
  resample(GCFR_EDS_template_raster, method = "bilinear")
SWAFR_variables_EDS <- SWAFR_variables %>%
  resample(SWAFR_EDS_template_raster, method = "bilinear")

# Generate heterogeneity data --------------------------------------------------

scales <- list(QDS = 1, HDS = 2, DS = 4)

GCFR_heterogeneity <- map(scales,
  ~ GCFR_variables_EDS %>%
    aggregate(fact = .x) %>%
    aggregate(fun = var)
)
GCFR_heterogeneity <- c(
  point1 = aggregate(GCFR_variables, fun = var),
  GCFR_heterogeneity
)

SWAFR_heterogeneity <- map(scales,
  ~ SWAFR_variables_EDS %>%
    aggregate(fact = .x) %>%
    aggregate(fun = var)
)
SWAFR_heterogeneity <- c(
  point1 = aggregate(SWAFR_variables, fun = var),
  SWAFR_heterogeneity
)

# Save heterogeneity rasters to disc -------------------------------------------

map(GCFR_heterogeneity,
  write.raster, glue("{data_dir}/GCFR_{var_names}_masked2_heterogeneity.tif")
)

map(SWAFR_heterogeneity,
  write.raster, glue("{data_dir}/SWAFR_{var_names}_masked2_heterogeneity.tif")
)

# Tidy heterogeneity data ------------------------------------------------------

# Join regions' datasets
heterogeneity <- map2(GCFR_heterogeneity, SWAFR_heterogeneity,
  function(each_GCFR_layer, each_SWAFR_layer) {
    each_GCFR_df <- each_GCFR_layer %>%
      log10() %>%
      as.data.frame() %>%
      cbind(region = "GCFR")
    each_SWAFR_df <- each_SWAFR_layer %>%
      log10() %>%
      as.data.frame() %>%
      cbind(region = "SWAFR")
    each_heterogeneity_df <- na.exclude(rbind(
      each_GCFR_df,
      each_SWAFR_df
    ))
    each_heterogeneity_df
  }
)
# Scale and centre all heterogeneity values _across_ regions
heterogeneity %<>%
  map(mutate_if, is.numeric, scale) %>%
  map(as_tibble)

# Generate PC1 of heterogeneity ------------------------------------------------

heterogeneity_PCAs <- map(heterogeneity,
  ~ .x %>%
    dplyr::select(-region) %>%
    prcomp(center = TRUE, scale. = TRUE)
)

# Force PC1 scores to be positive if all vars rotations are negative
heterogeneity_PCAs %<>% map(function(PCA) {
  if (all(PCA$rotation[, 1] <= 0)) {
    message("Multiplying this one by -1")
    PCA$rotation[, 1] %<>% multiply_by(-1)
    PCA$x[, 1]        %<>% multiply_by(-1)
  }
  PCA
})

# Add PC1 to heterogeneity dataset
PC1s <- map(heterogeneity_PCAs,
  ~tibble(PC1 = .x$x[, 1])
)
heterogeneity %<>% map2(PC1s,
  ~as_tibble(cbind(.x, .y))
)
