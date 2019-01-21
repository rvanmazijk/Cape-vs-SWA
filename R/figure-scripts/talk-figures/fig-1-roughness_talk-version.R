# Make Fig. 1.1 & 1.2 (Environmental heterogeneity and spatial scales)
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
    geom_hline(yintercept = 0.5, lty = "dashed", col = "grey25") +
    geom_point(aes(shape = variable), size = 2) +
    geom_line(aes(group = variable)) +
    geom_text(aes(label = U_sig), size = 2, col = "black", nudge_x = 0.4)

# Theme and nicer labels
CLES_plot <- CLES_plot +
  scale_colour_manual(values = var_colours, guide = FALSE) +
  scale_shape_manual(values = var_shapes) +
  facet_wrap(~ variable_type, nrow = 2, dir = "h") +
  xlab("Spatial scale") +
  ylab(expression(paste(italic(CLES)~ ~(Cape > SWA)))) +
  ylim(0.4, 1) +
  theme(legend.position = "none")

CLES_plot_blank <- CLES_plot +
  scale_colour_manual(values = c("white", "white", "white", "white"))
CLES_plot_blank$layers[4:5] <- NULL  # remove geom_text()s

# Roughness distribution plots -------------------------------------------------

elev_z_dbn_plot_data <- roughness_data %>%
  # Focal scales and variables for dbn panels
  filter(
    resolution %in% c("0.05º", "3QDS"),
    variable %in% "Elevation"
  ) %>%
  group_by(resolution, variable) %>%
  mutate(z_roughness = scale(roughness)) %>%  # Z-scale
  ungroup() %>%
  na.exclude()

elev_z_dbn_plot <- ggplot(elev_z_dbn_plot_data, aes(z_roughness, fill = region)) +
  geom_histogram(position = "dodge", bins = 20) +
  xlim(
    min(elev_z_dbn_plot_data$z_roughness),
    quantile(elev_z_dbn_plot_data$z_roughness, 0.99)
  ) +
  scale_fill_manual(name = "Region", values = my_palette) +
  facet_wrap(~ resolution, scales = "free_y") +
  labs(
    x = expression(paste(italic(Z)(Elevation~ ~roughness))),
    y = "No. cells"
  )

elev_z_dbn_plot_blank <- elev_z_dbn_plot +
  scale_fill_manual(values = c("white", "white")) +
  guides(fill = guide_legend(
    title = "Region",
    nrow = 2,
    override.aes = list(fill = my_palette)
  ))

# Save plots to disc -----------------------------------------------------------

ggsave(
  here("SAAB-AMA-SASSB-2019-talk/figures/fig-1.1-elev-roughness.png"),
  elev_z_dbn_plot,
  width = 5, height = 2.5,
  dpi = 300
)

ggsave(
  here("SAAB-AMA-SASSB-2019-talk/figures/fig-1.1-elev-roughness_blank.png"),
  elev_z_dbn_plot_blank,
  width = 5, height = 2.5,
  dpi = 300
)

ggsave(
  here("SAAB-AMA-SASSB-2019-talk/figures/fig-1.2-CLES-roughness.png"),
  CLES_plot,
  width = 4, height = 4,
  dpi = 300
)

ggsave(
  here("SAAB-AMA-SASSB-2019-talk/figures/fig-1.2-CLES-roughness_blank.png"),
  CLES_plot_blank,
  width = 4, height = 4,
  dpi = 300
)
