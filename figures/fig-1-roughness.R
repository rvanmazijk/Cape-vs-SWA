# Make Fig. 1 (Environmental heterogeneity and spatial scales)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))

output_path <- here::here("outputs/roughness")
import_objects(output_path)

var_shapes <- c(
  17,  # triangle      for elevation
  16,  # filled circle for MAP
  1,   # open circle   for PDQ
  15,  # square        for surfact T
  4,   # x             for NDVI,
  17,  # triangle      for CEC
  16,  # filled circle for clay
  1,   # open circle   for soil C
  15   # square        for pH
)
var_colours <- c(
  # <https://colourco.de/>
  "grey50",   # grey   for elevation
  "#507CC5",  # blue   for climate
  "#37A541",  # greeen for NDVI
  "#BA793E"   # brown  for soils
)

  gather(resolution, sig, -variable) %>%
  mutate(sig = ifelse(sig, "", "NS")) %>%
  left_join(jackknifed_CLES_summary)

data <- test_results_CLES_for_plot %>%
  mutate(
    variable_type = case_when(  # for colouring variables' lines etc.
      variable == "Elevation"      ~ "Elevation",
      variable == "NDVI"           ~ "NDVI",
      variable %in% var_names[2:4] ~ "Climate",
      variable %in% var_names[6:9] ~ "Soil"
      # (\n\n to "label" z_dbn_plot above CLES plot (cheat!))
    ),
    CLES = 1 - CLES  # Make CLES Cape - SWA, not SWA - Cape
    # (from obs CLES, not jackknife mean CLES)
  ) %>%
  full_join(test_results_summary) %>%
  mutate(
    variable = factor(variable, levels = var_names),
    variable_type = factor(variable_type, levels = c(
      "Elevation",
      "Climate",
      "NDVI",
      "Soil"
    )),
    resolution = factor(resolution, levels = c("0.05º", "QDS", "HDS", "3QDS"))
  )

# Make CLES ~ resolution panels ------------------------------------------------

pd <- position_dodge(0.4)

CLES_plot <- ggplot(data, aes(resolution, CLES, col = variable_type)) +
  geom_point(
    aes(shape = variable),
    size = 2#, position = pd
  ) +
  geom_line(
    aes(group = variable)#, position = pd
  ) +
  geom_text(aes(label = sig), size = 2, col = "black", nudge_x = 0.4) +
  scale_colour_manual(values = var_colours, guide = FALSE) +
  scale_shape_manual(values = var_shapes) +
  facet_wrap(~ variable_type, nrow = 1, dir = "h") +
  xlab("Spatial resolution") +
  ylab("CLES (Cape > SWA)") +
  ylim(0.4, 1) +
  guides(shape = guide_legend(
    title = "Environmental variables",
    nrow = 5, ncol = 2,
    direction = "vertical",
    override.aes = list(col = c(
      var_colours[1],
      rep(var_colours[2], 3),
      var_colours[3],
      rep(var_colours[4], 4)
    ))
  ))

# Roughness distribution plots -------------------------------------------------

data_for_violin_plot <- foreach(resolution = list(0.05, 0.25, 0.50, 0.75)) %do% {
  rbind(
    cbind(region = "GCFR", map2_df(GCFR_variables, resolution, prep_layer)),
    cbind(region = "SWAFR", map2_df(SWAFR_variables, resolution, prep_layer))
  )
}
data_for_violin_plot_tidy <- data_for_violin_plot %$%
  rbind(
    cbind(resolution = "0.05º", .[[1]]),
    cbind(resolution = "QDS",   .[[2]]),
    cbind(resolution = "HDS",   .[[3]]),
    cbind(resolution = "3QDS",  .[[4]])
  ) %>%
  as_tibble() %>%
  gather(variable, roughness, -resolution, -region) %>%
  na.omit() %>%
  group_by(resolution, variable) %>%
  mutate(z_roughness = scale(roughness)) %>%  # Z-scale!
  ungroup() %>%
  mutate(
    variable = factor(variable, levels = var_names),
    region = ifelse(region == "GCFR", "Cape", "SWA")
  )

z_dbn_plot <- data_for_violin_plot %>%
  filter(
    resolution %in% c("0.05º", "3QDS"),
    variable %in% c("Elevation", "MAP", "NDVI", "CEC")
  ) %>%
  mutate(variable = case_when(
    variable == "Elevation" ~ "Elevation",
    variable == "MAP"       ~ "Climate e.g. MAP",
    variable == "NDVI"      ~ "NDVI",
    variable == "CEC"       ~ "Soil e.g. CEC"
  )) %>%
  mutate(variable = factor(variable, levels = c(
    "Elevation",
    "Climate e.g. MAP",
    "NDVI",
    "Soil e.g. CEC"
  ))) %>%
  ggplot(aes(z_roughness, fill = region)) +
  geom_histogram(position = "dodge", bins = 20) +
  xlim(min(data_for_violin_plot$z_roughness), 5) +
  scale_fill_manual(name = "Region", values = my_palette) +
  facet_grid(resolution ~ variable, scales = "free_y") +
  labs(
    x = "Z(Roughness)",
    y = "No. cells"
  )

# Map panels of elevation ------------------------------------------------------

# TODO

#map(pre_analysis_import_paths, source)
#
#map_panel <- function(x, border, var = NULL) {
#  gplot(x) +
#    geom_tile(aes(fill = value)) +
#    geom_spatial(border, fill = NA, col = "black") +
#    scale_fill_viridis_c(na.value = NA, name = var) +
#    theme(
#      legend.position = c(0.75, 1),
#      legend.justification = c(1, 1),
#      legend.text = element_text(hjust = 1),
#      panel.border = element_blank(),
#      axis.title = element_blank(),
#      axis.ticks = element_blank(),
#      axis.text = element_blank()
#    )
#}
#map_panels <- function(x, border, var = NULL, var_title = NULL) {
#  stopifnot(is.list(x))
#  abs <- map(x, var)
#  rough <- map(abs, focal_sd)
#  GCFR_elev <-
#    c(abs, rough) %>%
#    map(
#      map_panel,
#      border = border,
#      var = var
#    )
#}
#
#GCFR_elev_panels <- map_panels(
#  list(
#    GCFR_variables,
#    GCFR_variables_3QDS
#  ),
#  border = GCFR_border_buffered,
#  var = "Elevation",
#  var_title = "Elevation (m)"
#)
#SWAFR_elev_panels <- map_panels(
#  list(
#    SWAFR_variables,
#    SWAFR_variables_3QDS
#  ),
#  border = SWAFR_border_buffered,
#  var = "Elevation",
#  var_title = "Elevation (m)"
#)
#
#plot_grid(plotlist = GCFR_elev_panels)
#plot_grid(plotlist = SWAFR_elev_panels)

#elev_panels_plot <-
#  plot_grid(ncol = 2, plotlist = list(
#    plot_grid(ncol = 1, plotlist =
#      GCFR_environment_plots[1:5]
#    ),
#    plot_grid(ncol = 1, rel_heights = c(4, 1), plotlist = list(
#      plot_grid(ncol = 1, plotlist =
#        GCFR_environment_plots[6:9]
#      ),
#      grid.rect(gp = gpar(col = "white"))
#    ))
#  ))

# Combine CLES and Z plots -----------------------------------------------------

legends <- plot_grid(
  get_legend(z_dbn_plot),
  get_legend(CLES_plot),
  nrow = 2
)
CLES_plot2 <- plot_grid(
  CLES_plot + theme(legend.position = "none"),
  grid.rect(gp = gpar(col = "white")),
  nrow = 1, rel_widths = c(4, 0.1)
)
final_plot <- plot_grid(
  z_dbn_plot + theme(legend.position = "none"),
  CLES_plot2,
  nrow = 2, rel_heights = c(1.5, 1),
  labels = c("(a)", "(b)")
)
final_plot <- plot_grid(
  final_plot,
  legends,
  nrow = 1, rel_widths = c(4, 1)
)

ggsave(
  here::here("figures/fig-1-roughness.png"),
  final_plot,
  width = 9, height = 6,
  dpi = 300
)

# Tidy-up workspace ------------------------------------------------------------

rm(
  CLES_plot,
  z_dbn_plot,
  legends,
  final_plot
)
