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

iwalk(GCFR_heterogeneity, function(each_scale, each_scales_name) {
  each_scale %<>%
    as.list() %>%
    set_names(str_replace_all(var_names, " ", "_"))
  iwalk(each_scale, function(each_layer, each_layers_name) {
    writeRaster(each_layer, overwrite = TRUE, filename = glue(
      "{data_dir}/",
      "GCFR_{each_layers_name}_masked2_{each_scales_name}_heterogeneity.tif"
    ))
  })
})

iwalk(SWAFR_heterogeneity, function(each_scale, each_scales_name) {
  each_scale %<>%
    as.list() %>%
    set_names(str_replace_all(var_names, " ", "_"))
  iwalk(each_scale, function(each_layer, each_layers_name) {
    writeRaster(each_layer, overwrite = TRUE, filename = glue(
      "{data_dir}/",
      "SWAFR_{each_layers_name}_masked2_{each_scales_name}_heterogeneity.tif"
    ))
  })
})

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
#heterogeneity_PCAs_clim <- map(heterogeneity,
#  ~ .x %>%
#    dplyr::select(Elevation, MAP, PDQ, Surface_T, NDVI) %>%
#    prcomp(center = TRUE, scale. = TRUE)
#)
#heterogeneity_PCAs_soil <- map(heterogeneity,
#  ~ .x %>%
#    dplyr::select(CEC, Clay, Soil_C, pH) %>%
#    prcomp(center = TRUE, scale. = TRUE)
#)

# Look at results
map(heterogeneity_PCAs, summary)
#>          Proportion of Variance (PC1)
#> $point1                        0.3819
#> $QDS                           0.4244
#> $HDS                           0.3902
#> $DS                            0.4126
map(heterogeneity_PCAs_clim, summary)
map(heterogeneity_PCAs_soil, summary)

# Force PC1 scores to be positive if all vars' rotations are negative
force_positive_PC1 <- function(PCA) {
  if (all(PCA$rotation[, 1] <= 0)) {
    message("Multiplying this one by -1")
    PCA$rotation[, 1] %<>% multiply_by(-1)
    PCA$x[, 1]        %<>% multiply_by(-1)
  }
  PCA
}
heterogeneity_PCAs      %<>% map(force_positive_PC1)
#heterogeneity_PCAs_clim %<>% map(force_positive_PC1)
#heterogeneity_PCAs_soil %<>% map(force_positive_PC1)

# Plot PC-biplots
plot_grid(plotlist = map2(heterogeneity_PCAs, heterogeneity,
  ~ autoplot(.x, data = .y, colour = "region",
    alpha = 0.25,
    loadings       = TRUE, loadings.colour       = "black",
    loadings.label = TRUE, loadings.label.colour = "black",
    loadings.label.hjust = -0.25
  ) +
  ggtitle(unique(.y$scale)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)
))
#plot_grid(plotlist = map2(heterogeneity_PCAs_clim, heterogeneity,
#  ~ autoplot(.x, data = .y, colour = "region",
#    alpha = 0.25,
#    loadings       = TRUE, loadings.colour       = "black",
#    loadings.label = TRUE, loadings.label.colour = "black",
#    loadings.label.hjust = -0.25
#  ) +
#  ggtitle(unique(.y$scale)) +
#  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
#  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)
#))
#plot_grid(plotlist = map2(heterogeneity_PCAs_soil, heterogeneity,
#  ~ autoplot(.x, data = .y, colour = "region",
#    alpha = 0.25,
#    loadings       = TRUE, loadings.colour       = "black",
#    loadings.label = TRUE, loadings.label.colour = "black",
#    loadings.label.hjust = -0.25
#  ) +
#  ggtitle(unique(.y$scale)) +
#  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
#  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)
#))

# Inspect other rotations/loadings? (PC2 et al.)
#PC2s <- heterogeneity_PCAs %$% as.data.frame(rbind(
#  point1 %$% cbind(
#    variable = rownames(rotation),
#    scale    = 0.10,
#    rotation = rotation[, 2]
#  ),
#  QDS %$% cbind(
#    variable = rownames(rotation),
#    scale    = 0.25,
#    rotation = rotation[, 2]
#  ),
#  HDS %$% cbind(
#    variable = rownames(rotation),
#    scale    = 0.50,
#    rotation = rotation[, 2]
#  ),
#  DS %$% cbind(
#    variable = rownames(rotation),
#    scale    = 1.00,
#    rotation = -rotation[, 2]  # !!!
#  )
#))
#PC2s$variable %<>% factor(levels = str_replace_all(var_names, " ", "_"))
#PC2s$scale    %<>% as.character() %>% as.numeric()
#PC2s$rotation %<>% as.character() %>% as.numeric()
#ggplot(PC2s, aes(scale, rotation, group = variable, colour = variable)) +
#  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
#  geom_point() +
#  geom_line()
#ggplot(PC2s, aes(scale, rotation)) +
#  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
#  geom_point() +
#  facet_wrap(~variable, nrow = 2)

# Add PC1 to heterogeneity dataset
PC1s <- map(heterogeneity_PCAs,
  ~tibble(PC1 = .x$x[, 1])
)
#PC1s_clim <- map(heterogeneity_PCAs_clim,
#  ~tibble(PC1_clim = .x$x[, 1])
#)
#PC1s_soil <- map(heterogeneity_PCAs_soil,
#  ~tibble(PC1_soil = .x$x[, 1])
#)
heterogeneity %<>%
  map2(PC1s, ~as_tibble(cbind(.x, .y))) #%>%
  #map2(PC1s_clim, ~as_tibble(cbind(.x, .y))) %>%
  #map2(PC1s_soil, ~as_tibble(cbind(.x, .y)))

# Fiddling
#
#heterogeneity %>%
#  bind_rows(.id = "scale") %>%
#  ggplot(aes(PC1_soil, PC1_clim, colour = region)) +
#    geom_point() +
#    facet_wrap(~scale, scales = "free")
#heterogeneity %>%
#  bind_rows(.id = "scale") %>%
#  gather(PC1_type, PC1_value, PC1, PC1_clim, PC1_soil) %>%
#  ggplot(aes(PC1_type, PC1_value, colour = region)) +
#    geom_boxplot() +
#    facet_wrap(~scale, scales = "free")
#heterogeneity %>%
#  map_dfr(.id = "scale",
#    ~ .x %$% tibble(
#      CLES_clim = CLES(
#        PC1_clim[region == "SWAFR"],
#        PC1_clim[region == "GCFR"]
#      ),
#      PU_clim = tidy(wilcox.test(PC1_clim ~ region))$p.value,
#      CLES_soil = CLES(
#        PC1_soil[region == "SWAFR"],
#        PC1_soil[region == "GCFR"]
#      ),
#      PU_soil = tidy(wilcox.test(PC1_soil ~ region))$p.value
#    )
#  )
#
# /Fiddling

# Save to disc
heterogeneity %>%
  bind_rows(.id = "scale") %>%
  write_csv(glue("{data_dir}/heterogeneity.csv"))
