# Make Fig. 1 (Environmental heterogeneity and spatial scales)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

U_CLES_results <- read_csv(here("outputs/roughness/U_CLES_results.csv"))
roughness_data <- read_csv(here("outputs/roughness/roughness_data.csv"))

roughness_analysis_data <- U_CLES_results %>%
  mutate(
    U_sig = ifelse(U_p_value < 0.05, "", "NS"),
    variable_type = case_when(  # for colouring variables' lines etc.
      variable == "Elevation"      ~ "Elevation",
      variable == "NDVI"           ~ "NDVI",
      variable %in% var_names[2:4] ~ "Climate",
      variable %in% var_names[6:9] ~ "Soil"
    )
  ) %>%
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

# Basic plot
CLES_plot <- roughness_analysis_data %>%
  ggplot(aes(resolution, CLES_value, col = variable_type)) +
    geom_point(aes(shape = variable), size = 2) +
    geom_line(aes(group = variable)) +
    geom_text(aes(label = U_sig), size = 2, col = "black", nudge_x = 0.4)

# Theme and nicer labels
CLES_plot <- CLES_plot +
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

z_dbn_plot_data <- roughness_data %>%
  # Focal scales and variables for dbn panels
  filter(
    resolution %in% c("0.05º", "3QDS"),
    variable %in% c("Elevation", "MAP", "NDVI", "CEC")
  ) %>%
  group_by(resolution, variable) %>%
  mutate(z_roughness = scale(roughness)) %>%  # Z-scale
  ungroup() %>%
  mutate(variable =
    # Group variables for plot
    case_when(
      variable == "Elevation" ~ "Elevation",
      variable == "MAP"       ~ "Climate e.g. MAP",
      variable == "NDVI"      ~ "NDVI",
      variable == "CEC"       ~ "Soil e.g. CEC"
    ) %>%
    # Reorder new factors
    factor(levels = c(
      "Elevation",
      "Climate e.g. MAP",
      "NDVI",
      "Soil e.g. CEC"
    ))
  ) %>%
  na.exclude()

z_dbn_plot <- ggplot(z_dbn_plot_data, aes(z_roughness, fill = region)) +
  geom_histogram(position = "dodge", bins = 20) +
  xlim(min(z_dbn_plot_data$z_roughness), quantile(z_dbn_plot_data$z_roughness, 0.99)) +
  scale_fill_manual(name = "Region", values = my_palette) +
  facet_grid(resolution ~ variable, scales = "free_y") +
  labs(
    x = "Z(Roughness)",
    y = "No. cells"
  )

# Map panels of elevation ------------------------------------------------------

# TODO

# Combine CLES and Z plots -----------------------------------------------------

legends <- plot_grid(
  get_legend(z_dbn_plot),
  get_legend(CLES_plot),
  nrow = 2
)
CLES_plot <- plot_grid(
  CLES_plot + theme(legend.position = "none"),
  white_rect,
  nrow = 1, rel_widths = c(4, 0.1)
)
final_plot <- plot_grid(
  z_dbn_plot + theme(legend.position = "none"),
  CLES_plot,
  nrow = 2, rel_heights = c(1.5, 1),
  labels = c("(a)", "(b)")
)
final_plot <- plot_grid(
  final_plot,
  legends,
  nrow = 1, rel_widths = c(4, 1)
)

ggsave(
  here("figures/fig-1-roughness.png"),
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
