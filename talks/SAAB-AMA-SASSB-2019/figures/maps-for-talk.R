# Make Fig. 5.1 & 5.2 (Predicted and observed richness maps)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here(
  "R/analyses/analyse-species-environment-relationships/01_collate-data.R"
))

# Import predicted richness rasters --------------------------------------------

GCFR_pred_QDS_richness <- raster(here(
  "outputs/species-environment-relationships/from-local-machines",
  "GCFR_pred_QDS_richness.tif"
))
GCFR_pred_HDS_richness <- raster(here(
  "outputs/species-environment-relationships/from-local-machines",
  "GCFR_pred_HDS_richness.tif"
))
SWAFR_pred_QDS_richness <- raster(here(
  "outputs/species-environment-relationships/from-local-machines",
  "SWAFR_pred_QDS_richness.tif"
))
SWAFR_pred_HDS_richness <- raster(here(
  "outputs/species-environment-relationships/from-local-machines",
  "SWAFR_pred_HDS_richness.tif"
))

# Collate predicted and observed richness rasters ------------------------------

richness_rasters <- list(
  Cape_QDS_obs = GCFR_data_QDS_stack$QDS_richness,
  SWA_QDS_obs = SWAFR_data_QDS_stack$QDS_richness,
  Cape_QDS_pred = exp(GCFR_pred_QDS_richness),
  SWA_QDS_pred = exp(SWAFR_pred_QDS_richness),
  Cape_HDS_obs = GCFR_data_HDS_stack$HDS_richness,
  SWA_HDS_obs = SWAFR_data_HDS_stack$HDS_richness,
  Cape_HDS_pred = exp(GCFR_pred_HDS_richness),
  SWA_HDS_pred = exp(SWAFR_pred_HDS_richness)
)

# Collate border polygons, repeated for use in foreach()-loop below
region_borders <- list(
  GCFR_border,
  SWAFR_border,
  GCFR_border,
  SWAFR_border,
  GCFR_border,
  SWAFR_border,
  GCFR_border,
  SWAFR_border
)

# Plot these rasters -----------------------------------------------------------

max_richness_QDS <- max(
  as_vector(map(richness_rasters[1:4], getValues)),
  na.rm = TRUE
)
max_richness_HDS <- max(
  as_vector(map(richness_rasters[5:8], getValues)),
  na.rm = TRUE
)
i <- 1
richness_maps <- foreach(response = richness_rasters,
                         region = region_borders) %do% {
  region_name <- str_extract(names(richness_rasters)[[i]], "(Cape|SWA)")
  response_name <- str_remove(names(richness_rasters)[[i]], "(Cape|SWA)_")
  legend_name <- case_when(
    response_name == "QDS_obs"  ~ bquote(    italic("S") ["QDS"]),
    response_name == "QDS_pred" ~ bquote(hat(italic("S"))["QDS"]),
    response_name == "HDS_obs"  ~ bquote(    italic("S") ["HDS"]),
    response_name == "HDS_pred" ~ bquote(hat(italic("S"))["HDS"])
  )
  fill_lim <- c(0, case_when(
    str_detect(response_name, "QDS") ~ max_richness_QDS,
    str_detect(response_name, "HDS") ~ max_richness_HDS
  ))
  i <- i + 1
  gplot(response) +
    geom_tile(aes(fill = value), alpha = 0.95) +
    geom_path(
      data = region,
      aes(long, lat, group = group),
      fill = NA, color = "black", size = 0.5
    ) +
    scale_fill_viridis_c(name = legend_name, lim = fill_lim, na.value = NA) +
    labs(x = "Longitude (º)", y = "Latitude (º)", title = region_name) +
    ylim(-35.5, -25)
}
names(richness_maps) <- names(richness_rasters)
richness_maps[c(1, 3, 5, 7)] %<>%
  map(~ .x + theme(
    legend.position = "none"
  ))
richness_maps[c(2, 4, 6, 8)] %<>%
  map(~ .x + theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
  ))
richness_maps[c(1, 2, 5, 6)] %<>%
  map(~ .x + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  ))
richness_maps[c(3, 4, 7, 8)] %<>%
  map(~ .x + theme(
    plot.title = element_blank()
  ))

richness_maps_blank2 <- richness_maps[1:4]
richness_maps_blank2[3:4] %<>%
  map(~ .x + geom_tile(fill = "white"))
richness_maps_blank1 <- richness_maps[1:4]
richness_maps_blank1[1:4] %<>%
  map(~ .x + geom_tile(fill = "white"))

# Save to disc, panelled -------------------------------------------------------

ggsave(
  here("SAAB-AMA-SASSB-2019-talk/figures/fig-5.1-QDS-richness-maps_blank1.png"),
  plot_grid(
    plotlist = richness_maps_blank1,
    nrow = 2, ncol = 2,
    rel_widths = c(1, 1.25), rel_heights = c(1, 1.125)
  ),
  width = 5, height = 4,
  dpi = 300
)
ggsave(
  here("SAAB-AMA-SASSB-2019-talk/figures/fig-5.1-QDS-richness-maps_blank2.png"),
  plot_grid(
    plotlist = richness_maps_blank2,
    nrow = 2, ncol = 2,
    rel_widths = c(1, 1.25), rel_heights = c(1, 1.125)
  ),
  width = 5, height = 4,
  dpi = 300
)
ggsave(
  here("SAAB-AMA-SASSB-2019-talk/figures/fig-5.1-QDS-richness-maps.png"),
  plot_grid(
    plotlist = richness_maps[1:4],
    nrow = 2, ncol = 2,
    rel_widths = c(1, 1.25), rel_heights = c(1, 1.125)
  ),
  width = 5, height = 4,
  dpi = 300
)
ggsave(
  here("SAAB-AMA-SASSB-2019-talk/figures/fig-5.2-QDS-richness-maps.png"),
  plot_grid(
    plotlist = richness_maps[5:8],
    nrow = 2, ncol = 2,
    rel_widths = c(1, 1.25), rel_heights = c(1, 1.125)
  ),
  width = 5, height = 4,
  dpi = 300
)

# Roughness in elevation maps --------------------------------------------------

elevation_rasters <- list(
  Cape_elev       = GCFR_data_QDS_stack$Elevation,
  SWA_elev        = SWAFR_data_QDS_stack$Elevation,
  Cape_rough_elev = GCFR_data_QDS_stack$rough_Elevation,
  SWA_rough_elev  = SWAFR_data_QDS_stack$rough_Elevation
)
max_elev <- max(
  as_vector(map(elevation_rasters[1:2], getValues)),
  na.rm = TRUE
)
max_rough_elev <- max(
  as_vector(map(elevation_rasters[3:4], getValues)),
  na.rm = TRUE
)
i <- 1
elevation_maps <- foreach(response = elevation_rasters,
                          region = region_borders) %do% {
  region_name <- str_extract(names(elevation_rasters)[[i]], "(Cape|SWA)")
  response_name <- str_remove(names(elevation_rasters)[[i]], "(Cape|SWA)_")
  legend_name <- case_when(
    response_name == "elev"       ~ "Elev. (m)",
    response_name == "rough_elev" ~ "R Elev. (m)"
  )
  fill_lim <- c(0, case_when(
    response_name == "elev"       ~ max_elev,
    response_name == "rough_elev" ~ max_rough_elev
  ))
  print(fill_lim)
  i <- i + 1
  gplot(response) +
    geom_tile(aes(fill = value), alpha = 0.95) +
    geom_path(
      data = region,
      aes(long, lat, group = group),
      fill = NA, color = "black", size = 0.5
    ) +
    scale_fill_viridis_c(name = legend_name, lim = fill_lim, na.value = NA) +
    labs(x = "Longitude (º)", y = "Latitude (º)", title = region_name) +
    ylim(-35.5, -25) +
    theme(legend.title = element_text(size = 6))
}
names(elevation_maps) <- names(elevation_rasters)
elevation_maps[c(1, 3)] %<>%
  map(~ .x + theme(
    legend.position = "none"
  ))
elevation_maps[c(2, 4)] %<>%
  map(~ .x + theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
  ))
elevation_maps[c(1, 2)] %<>%
  map(~ .x + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  ))
elevation_maps[c(3, 4)] %<>%
  map(~ .x + theme(
    plot.title = element_blank()
  ))

elevation_maps_blank2 <- elevation_maps
elevation_maps_blank2[3:4] %<>%
  map(~ .x + geom_tile(fill = "white"))
elevation_maps_blank1 <- elevation_maps
elevation_maps_blank1 %<>%
  map(~ .x + geom_tile(fill = "white"))

# Save to disc, panelled -------------------------------------------------------

ggsave(
  here("SAAB-AMA-SASSB-2019-talk/figures/fig-1.3-elevation-maps_blank1.png"),
  plot_grid(
    plotlist = elevation_maps_blank1,
    nrow = 2, ncol = 2,
    rel_widths = c(1, 1.25), rel_heights = c(1, 1.125)
  ),
  width = 5, height = 4,
  dpi = 300
)
ggsave(
  here("SAAB-AMA-SASSB-2019-talk/figures/fig-1.3-elevation-maps_blank2.png"),
  plot_grid(
    plotlist = elevation_maps_blank2,
    nrow = 2, ncol = 2,
    rel_widths = c(1, 1.25), rel_heights = c(1, 1.125)
  ),
  width = 5, height = 4,
  dpi = 300
)
ggsave(
  here("SAAB-AMA-SASSB-2019-talk/figures/fig-1.3-elevation-maps.png"),
  plot_grid(
    plotlist = elevation_maps,
    nrow = 2, ncol = 2,
    rel_widths = c(1, 1.25), rel_heights = c(1, 1.125)
  ),
  width = 5, height = 4,
  dpi = 300
)

# Region maps ------------------------------------------------------------------

library(maps)
world <- ggplot() +
  borders("world", colour = "gray75", fill = "gray75", size = 0.1) +
  labs(x = "Longitude (º)", y = "Latitude (º)") +
  lims(x = c(0, 180), y = c(-70, 0))
cape <- geom_polygon(
  data = GCFR_border,
  aes(long, lat, group = group),
  fill = "orange", color = "orange", size = 0.5
)
swa <- geom_polygon(
  data = SWAFR_border,
  aes(long, lat, group = group),
  fill = "blue", color = "blue", size = 0.5
)
ggsave(
  here("SAAB-AMA-SASSB-2019-talk/figures/fig-6-region-map.png"),
  world + cape + swa,
  width = 5, height = 2.5,
  dpi = 300
)
