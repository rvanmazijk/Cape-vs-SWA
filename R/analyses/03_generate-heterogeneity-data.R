# Resample environmental data to EDS -------------------------------------------

enviro_data_EDS <- resample(
  enviro_data, Larsen_grid_EDS_ras,
  method = "bilinear"
)
enviro_data_QDS <- resample(
  enviro_data, Larsen_grid_QDS_ras,
  method = "bilinear"
)
enviro_data_HDS <- resample(
  enviro_data, Larsen_grid_HDS_ras,
  method = "bilinear"
)

# Merge environmental data from rasters into dataframes ------------------------

enviro_data_EDS_df <- raster2df(enviro_data_EDS, Larsen_grid_EDS_data)
enviro_data_QDS_df <- raster2df(enviro_data_QDS, Larsen_grid_QDS_data)
enviro_data_HDS_df <- raster2df(enviro_data_HDS, Larsen_grid_HDS_data)

# Calculate heterogeneity within 0.10, QDS, HDS, DS ----------------------------

heterogeneity_0.10 <- enviro_data %>%
  aggregate(fun = var) %>%
  log() %>%
  scale()
heterogeneity_0.10_df <- heterogeneity_0.10 %>%
  raster2df() %>%
  na.exclude() %>%
  mutate(region = ifelse(lon > 60, "SWAFR", "GCFR")) %>%
  dplyr::select(region, lon, lat, Elevation:pH)

heterogeneity_QDS_df <- enviro_data_EDS_df %>%
  filter(qdgc %in% QDS_w_all_EDS) %>%
  filter(!is.na(region)) %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  group_by(region, qdgc) %>%
  summarise_at(
    vars(str_replace(var_names, " ", "_")),
    ~var(., na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_at(vars(var_names_tidy), log10) %>%
  mutate_at(vars(var_names_tidy), scale)

heterogeneity_HDS_df <- enviro_data_QDS_df %>%
  filter(hdgc %in% HDS_w_all_QDS) %>%
  filter(!is.na(region)) %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  group_by(region, hdgc) %>%
  summarise_at(
    vars(str_replace(var_names, " ", "_")),
    ~var(., na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_at(vars(var_names_tidy), log10) %>%
  mutate_at(vars(var_names_tidy), scale)

heterogeneity_DS_df <- enviro_data_HDS_df %>%
  filter(dgc %in% DS_w_all_HDS) %>%
  filter(!is.na(region)) %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  group_by(region, dgc) %>%
  summarise_at(
    vars(str_replace(var_names, " ", "_")),
    ~var(., na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_at(vars(var_names_tidy), log10) %>%
  mutate_at(vars(var_names_tidy), scale)

# Run PCA of heterogeneity -----------------------------------------------------

heterogeneity_0.10_PCA <- heterogeneity_0.10_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

heterogeneity_QDS_PCA <- heterogeneity_QDS_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

heterogeneity_HDS_PCA <- heterogeneity_HDS_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

heterogeneity_DS_PCA <- heterogeneity_DS_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

summary(heterogeneity_0.10_PCA)
summary(heterogeneity_QDS_PCA)
summary(heterogeneity_HDS_PCA)
summary(heterogeneity_DS_PCA)

# Save PCA-objects to disc for use in figure -----------------------------------

write_rds(heterogeneity_0.10_PCA, here("results/heterogeneity_0.10_PCA"))
write_rds(heterogeneity_QDS_PCA,  here("results/heterogeneity_QDS_PCA"))
write_rds(heterogeneity_HDS_PCA,  here("results/heterogeneity_HDS_PCA"))
write_rds(heterogeneity_DS_PCA,   here("results/heterogeneity_DS_PCA"))

# Save PC1 and PC2 into dataframes ---------------------------------------------

heterogeneity_0.10_df$PC1 <- heterogeneity_0.10_PCA$x[, 1]
heterogeneity_0.10_df$PC2 <- heterogeneity_0.10_PCA$x[, 2]

heterogeneity_QDS_df$PC1 <- heterogeneity_QDS_PCA$x[, 1]
heterogeneity_QDS_df$PC2 <- heterogeneity_QDS_PCA$x[, 2]

heterogeneity_HDS_df$PC1 <- heterogeneity_HDS_PCA$x[, 1]
heterogeneity_HDS_df$PC2 <- heterogeneity_HDS_PCA$x[, 2]

heterogeneity_DS_df$PC1 <- heterogeneity_DS_PCA$x[, 1]
heterogeneity_DS_df$PC2 <- heterogeneity_DS_PCA$x[, 2]

# Save heterogeneity dataframes to disc ----------------------------------------

write_csv(
  heterogeneity_0.10_df,
  glue("{data_dir}/heterogeneity-data-0.10.csv")
)

write_csv(
  heterogeneity_QDS_df,
  glue("{data_dir}/heterogeneity-data-QDS.csv")
)

write_csv(
  heterogeneity_HDS_df,
  glue("{data_dir}/heterogeneity-data-HDS.csv")
)

write_csv(
  heterogeneity_DS_df,
  glue("{data_dir}/heterogeneity-data-DS.csv")
)

# Rasterise heterogeneity dataframes -------------------------------------------
# (not needed for 0.10-scale)

heterogeneity_QDS_ras <- var_names_tidy %>%
  map(function(each_var) {
    heterogeneity_QDS_df %>%
      full_join(Larsen_grid_QDS_data) %>%
      dplyr::select_("region", "qdgc", "lon", "lat", each_var) %>%
      rasterise_data(each_var, Larsen_grid_QDS_ras)
  }) %>%
  stack() %>%
  set_names(str_replace_all(var_names, " ", "_"))

heterogeneity_HDS_ras <- var_names_tidy %>%
  map(function(each_var) {
    heterogeneity_HDS_df %>%
      full_join(Larsen_grid_HDS_data) %>%
      dplyr::select_("region", "hdgc", "lon", "lat", each_var) %>%
      rasterise_data(each_var, Larsen_grid_HDS_ras)
  }) %>%
  stack() %>%
  set_names(str_replace_all(var_names, " ", "_"))

heterogeneity_DS_ras <- var_names_tidy %>%
  map(function(each_var) {
    heterogeneity_DS_df %>%
      # Make DS-cell midpoints manually
      mutate(
        lon = dgc %>%
          str_extract("E[0-9]{3}") %>%
          str_remove("E") %>%
          as.numeric() %>%
          add(0.5),
        lat = dgc %>%
          str_extract("S[0-9]{2}") %>%
          str_remove("S") %>%
          as.numeric() %>%
          add(0.5) %>%
          multiply_by(-1)
      ) %>%
      dplyr::select_("region", "dgc", "lon", "lat", each_var) %>%
      rasterise_data(each_var, Larsen_grid_DS_ras)
  }) %>%
  stack() %>%
  set_names(var_names_tidy)

# Plot to check
if (FALSE) {
  plot(heterogeneity_QDS_ras)
  plot(heterogeneity_HDS_ras)
  plot(heterogeneity_DS_ras)
}

# Save heterogeneity rasters to disc -------------------------------------------

writeRaster(
  heterogeneity_0.10,
  glue("{data_dir}/raster-layers/heterogeneity-0.10"),
  bylayer = TRUE, suffix = "names", format = "GTiff",
  overwrite = TRUE
)

writeRaster(
  heterogeneity_QDS_ras,
  glue("{data_dir}/raster-layers/heterogeneity-QDS"),
  bylayer = TRUE, suffix = "names", format = "GTiff",
  overwrite = TRUE
)

writeRaster(
  heterogeneity_HDS_ras,
  glue("{data_dir}/raster-layers/heterogeneity-HDS"),
  bylayer = TRUE, suffix = "names", format = "GTiff",
  overwrite = TRUE
)

writeRaster(
  heterogeneity_DS_ras,
  glue("{data_dir}/raster-layers/heterogeneity-DS"),
  bylayer = TRUE, suffix = "names", format = "GTiff",
  overwrite = TRUE
)
