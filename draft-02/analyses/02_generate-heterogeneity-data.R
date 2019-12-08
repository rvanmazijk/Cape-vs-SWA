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
heterogeneity_PCAs %<>% map(force_positive_PC1)

# Plot PC-biplots
PC_biplots <- pmap(list(heterogeneity_PCAs, heterogeneity, names(heterogeneity)),
  ~ autoplot(..1, data = ..2, colour = "region",
    loadings       = TRUE, loadings.colour       = "black",
    loadings.label = TRUE, loadings.label.colour = "black",
    loadings.label.hjust = -0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  lims(
    x = case_when(
      ..3 == "point1" ~ c(-0.10, 0.10),
      ..3 == "QDS"    ~ c(-0.20, 0.20),
      ..3 == "HDS"    ~ c(-0.25, 0.25),
      ..3 == "DS"     ~ c(-0.25, 0.25)
    ),
    y = case_when(
      ..3 == "point1" ~ c(-0.10, 0.10),
      ..3 == "QDS"    ~ c(-0.20, 0.20),
      ..3 == "HDS"    ~ c(-0.25, 0.25),
      ..3 == "DS"     ~ c(-0.25, 0.25)
    )
  ) +
  scale_colour_manual(name = "Region", values = c("grey25", "grey75")) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
)
my_legend <- get_legend(PC_biplots$point1)
PC_biplots %<>% map(~ . + theme(legend.position = "none"))
PC_biplots <- plot_grid(
  plotlist = PC_biplots,
  nrow = 2,
  labels         = c("(a) 0.10°×0.10°", "(b) QDS", "(c) HDS", "(d) DS"),
  label_fontface = "plain",
  label_x        = 0.150,
  label_y        = 0.975,
  hjust          = 0
)
PC_biplots <- plot_grid(
  PC_biplots, my_legend,
  nrow = 1, rel_widths = c(1, 0.2)
)
PC_biplots

# Save for SI
ggsave(
  here("draft-02/figures/plot-PC-biplots.pdf"),
  PC_biplots,
  width = 8, height = 6
)
ggsave(
  here("draft-02/figures/plot-PC-biplots.png"),
  PC_biplots,
  width = 8, height = 6
)

# Add PC1 to heterogeneity dataset
PC1s <- map(heterogeneity_PCAs,
  ~tibble(PC1 = .x$x[, 1])
)
heterogeneity %<>%
  map2(PC1s, ~as_tibble(cbind(.x, .y)))

# Save to disc
heterogeneity %>%
  bind_rows(.id = "scale") %>%
  write_csv(glue("{data_dir}/heterogeneity.csv"))
