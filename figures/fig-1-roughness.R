# Make Fig. 1 (Environmental heterogeneity and spatial scales)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("figures/figure-setup.R"))

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

# Data-wrangling ---------------------------------------------------------------

jackknifed_CLES_summary_QDS %<>%
  gather(variable, CLES) %>%
  mutate(mean_or_sd =
    ifelse(str_detect(variable, "_mean"),
      "CLES_mean",
      "CLES_sd"
    )
  ) %>%
  mutate(variable = str_remove(variable, "_mean")) %>%
  mutate(variable = str_remove(variable, "_sd")) %>%
  spread(mean_or_sd, CLES) %>%
  mutate(resolution = "QDS")
jackknifed_CLES_summary_QDS$variable %<>% factor(levels = var_names)
jackknifed_CLES_summary_HDS %<>%
  gather(variable, CLES) %>%
  mutate(mean_or_sd =
    ifelse(str_detect(variable, "_mean"),
      "CLES_mean",
      "CLES_sd"
    )
  ) %>%
  mutate(variable = str_remove(variable, "_mean")) %>%
  mutate(variable = str_remove(variable, "_sd")) %>%
  spread(mean_or_sd, CLES) %>%
  mutate(resolution = "HDS")
jackknifed_CLES_summary_HDS$variable %<>% factor(levels = var_names)

jackknifed_CLES_summary <- full_join(
  jackknifed_CLES_summary_QDS,
  jackknifed_CLES_summary_HDS
)
jackknifed_CLES_summary %<>% mutate(
  CLES_upper = CLES_mean + CLES_sd,
  CLES_lower = CLES_mean - CLES_sd
)
test_results_summary %<>%
  gather(resolution, sig, -variable) %>%
  mutate(sig = ifelse(sig, "", "NS")) %>%
  left_join(jackknifed_CLES_summary)

data <- test_results_CLES_for_plot %>%
  mutate(
    variable_type = case_when(  # for colouring variables' lines etc.
      variable == "Elevation"      ~ "\n\nElevation",
      variable == "NDVI"           ~ "\n\nNDVI",
      variable %in% var_names[2:4] ~ "e.g. MAP\n\nClimate",
      variable %in% var_names[6:9] ~ "e.g. CEC\n\nSoil"
      # (\n\n to "label" z_dbn_plot above CLES plot (cheat!))
    ),
    CLES = 1 - CLES  # Make CLES Cape - SWA, not SWA - Cape
    # (from obs CLES, not jackknife mean CLES)
  ) %>%
  full_join(test_results_summary) %>%
  mutate(
    variable = factor(variable, levels = var_names),
    variable_type = factor(variable_type, levels = c(
      "\n\nElevation",
      "e.g. MAP\n\nClimate",
      "\n\nNDVI",
      "e.g. CEC\n\nSoil"
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
  #geom_errorbar(
  #  aes(
  #    ymin = CLES_lower,
  #    ymax = CLES_upper,
  #    group = paste(variable, resolution)
  #  ),
  #  width = 0, alpha = 0.5,
  #  position = pd
  #) +
  #geom_point(
  #  aes(y = CLES_mean, shape = variable),
  #  size = 2, alpha = 0.5,
  #  position = pd
  #) +
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

# Tidy-up raw distribution dataframe
data_for_violin_plot$variable %<>% factor(levels = var_names)

z_dbn_plot <- data_for_violin_plot %>%
  filter(
    resolution %in% c("0.05º", "3QDS"),
    variable %in% c("Elevation", "MAP", "NDVI", "CEC")
  ) %>%
  ggplot(aes(z_roughness, col = region, fill = region)) +
  geom_density(alpha = 0.5) +
  xlim(min(data_for_violin_plot$z_roughness), 5) +
  scale_colour_manual(name = "Region", values = my_palette) +
  scale_fill_manual(name = "Region", values = my_palette) +
  facet_wrap(
    ~ variable + resolution,
    nrow = 1,
    strip.position = "bottom",
    labeller = label_bquote(.(resolution))
  ) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank()
  )

#adj_z_roughness <- abs(min(data_for_violin_plot$z_roughness)) + 0.1
#
#z_dbn_plot_adj <- data_for_violin_plot %>%
#  filter(
#    resolution %in% c("0.05º", "3QDS"),
#    variable %in% c("Elevation", "MAP", "NDVI", "CEC")
#  ) %>%
#  ggplot(aes(log(z_roughness + adj_z_roughness), col = region, fill = region)) +
#  geom_density(alpha = 0.5) +
#  xlim(min(data_for_violin_plot$z_roughness), 5) +
#  scale_colour_manual(name = "Region", values = my_palette) +
#  scale_fill_manual(name = "Region", values = my_palette) +
#  facet_wrap(
#    ~ variable + resolution,
#    nrow = 1,
#    strip.position = "bottom",
#    labeller = label_bquote(.(resolution))
#  ) +
#  theme(
#    axis.title = element_blank(),
#    axis.ticks = element_blank(),
#    axis.text = element_blank(),
#    panel.border = element_blank()
#  )


# Map panels of elevation ------------------------------------------------------

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
  nrow = 2, rel_heights = c(1, 2)
)
z_dbn_plot <- plot_grid(
  grid.rect(gp = gpar(col = "white")),
  z_dbn_plot + theme(legend.position = "none"),
  nrow = 1, rel_widths = c(0.2, 4)
)
final_plot <- plot_grid(
  z_dbn_plot + theme(legend.position = "none"),
  CLES_plot + theme(legend.position = "none"),
  nrow = 2, rel_heights = c(1, 2),
  labels = c("(a)", "(b)"), vjust = c(1.5, 3)
)
final_plot <- plot_grid(
  final_plot,
  legends,
  nrow = 1, rel_widths = c(4, 1)
)

ggsave(
  here::here("figures/fig-1-roughness.png"),
  final_plot,
  width = 10, height = 4,
  dpi = 300
)

# Tidy-up workspace ------------------------------------------------------------

rm(
  CLES_plot,
  z_dbn_plot,
  legends,
  final_plot
)
