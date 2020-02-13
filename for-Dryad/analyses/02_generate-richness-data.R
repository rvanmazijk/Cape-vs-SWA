# Detemine which DS, HDS & QDS have all 4 of their HDS, QDS & EDS --------------
# (within the regions' borders)

# Query the border polygon and store results in Larsen grid
Larsen_grid_EDS@data <- cbind(
  Larsen_grid_EDS@data,
  Larsen_grid_EDS %over% borders_buffered
)
# For larger cells, just use longitude for region classification,
# because later I filter the cells based on whether their constituent cells
# are in the regions (from EDS above)
Larsen_grid_QDS@data$region <-
  ifelse(Larsen_grid_QDS@data$lon > 90,
    "SWAFR", "GCFR"
  )
Larsen_grid_HDS@data$region <-
  ifelse(Larsen_grid_HDS@data$lon > 90,
    "SWAFR", "GCFR"
  )

# Filter to EDS that are within the regions' borders
Larsen_grid_EDS <- Larsen_grid_EDS[!is.na(Larsen_grid_EDS$region), ]

# Make grids tibbles for easier wrangling
Larsen_grid_EDS_data <- as_tibble(Larsen_grid_EDS@data)
Larsen_grid_QDS_data <- as_tibble(Larsen_grid_QDS@data)
Larsen_grid_HDS_data <- as_tibble(Larsen_grid_HDS@data)

# Pull out QDS-codes of QDS with all 4 EDS (within borders)
QDS_w_all_EDS <- Larsen_grid_EDS_data %>%
  group_by(qdgc, region) %>%
  dplyr::select(edgc) %>%
  distinct() %>%  # just in case
  summarise(n_EDS = n()) %>%
  filter(n_EDS == 4) %>%
  pull(qdgc)

# Pull out HDS-codes of HDS with all 4 HDS (within borders)
HDS_w_all_QDS <- Larsen_grid_QDS_data %>%
  group_by(hdgc, region) %>%
  dplyr::select(qdgc) %>%
  distinct() %>%
  filter(qdgc %in% QDS_w_all_EDS) %>%
  summarise(n_QDS = n()) %>%
  filter(n_QDS == 4) %>%
  pull(hdgc)

# Pull out DS-codes of DS with all 4 DS (within borders)
DS_w_all_HDS <- Larsen_grid_HDS_data %>%
  group_by(dgc, region) %>%
  dplyr::select(hdgc) %>%
  distinct() %>%
  filter(hdgc %in% HDS_w_all_QDS) %>%
  summarise(n_HDS = n()) %>%
  filter(n_HDS == 4) %>%
  pull(dgc)

# Plot grids and cell midpoints to check
# NOTE:
#   Plots EDS that belong to a QDS with all 4 EDS in region,
#   **not** the QDS themselves,
#   etc. for other scales
if (FALSE) {
  # GCFR:
  plot(border = "green", Larsen_grid_EDS[
    Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS &
    Larsen_grid_EDS$region == "GCFR",
  ])
  points(col = "green",
    Larsen_grid_EDS$lon[Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS],
    Larsen_grid_EDS$lat[Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS]
  )
  plot(border = "red", add = TRUE, Larsen_grid_QDS[
    Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS &
    Larsen_grid_QDS$region == "GCFR",
  ])
  points(col = "red",
    Larsen_grid_QDS$lon[Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS],
    Larsen_grid_QDS$lat[Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS]
  )
  plot(border = "blue", add = TRUE, Larsen_grid_HDS[
    Larsen_grid_HDS$dgc %in% DS_w_all_HDS &
    Larsen_grid_HDS$region == "GCFR",
  ])
  points(col = "blue",
    Larsen_grid_HDS$lon[Larsen_grid_HDS$dgc %in% DS_w_all_HDS],
    Larsen_grid_HDS$lat[Larsen_grid_HDS$dgc %in% DS_w_all_HDS]
  )

  # SWAFR:
  plot(border = "green", Larsen_grid_EDS[
    Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS &
    Larsen_grid_EDS$region == "SWAFR",
  ])
  plot(border = "red", add = TRUE, Larsen_grid_QDS[
    Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS &
    Larsen_grid_QDS$region == "SWAFR",
  ])
  plot(border = "blue", add = TRUE, Larsen_grid_HDS[
    Larsen_grid_HDS$dgc %in% DS_w_all_HDS &
    Larsen_grid_HDS$region == "SWAFR",
  ])
}

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
    loadings = TRUE,
      loadings.colour = "black",
      loadings.label = TRUE,
        loadings.label.colour = "black",
        loadings.label.hjust = -0.25,
        loadings.label.size = 3
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
  here("figures/plot-PC-biplots.pdf"),
  PC_biplots,
  width = 8, height = 6
)
ggsave(
  here("figures/plot-PC-biplots.png"),
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
