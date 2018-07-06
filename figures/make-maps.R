# Make maps of environmental and floral data
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("figures/figure-setup.R"))
map(pre_analysis_import_paths, source)

# Cape -------------------------------------------------------------------------

GCFR_environment_plots <- vector("list", length = length(var_names))
for (i in seq_along(var_names)) {
  absolute_panel <- gplot(GCFR_variables_QDS[[i]]) +
    geom_tile(aes(fill = value)) +
    geom_spatial(GCFR_border, fill = NA, col = "black") +
    scale_fill_viridis_c(na.value = NA) +
    labs(x = "Longitude (º)",
         y = "Latitude (º)",
         title = var_names[[i]]) +
    theme(legend.position = c(0.99, 0.99),
          legend.justification = c(1, 1),
          legend.title = element_blank())
  roughness_panel <- gplot(focal_sd(GCFR_variables_QDS[[i]])) +
    geom_tile(aes(fill = value)) +
    geom_spatial(GCFR_border, fill = NA, col = "black") +
    scale_fill_viridis_c(na.value = NA) +
    labs(x = "Longitude (º)",
         y = "Latitude (º)",
         title = glue("{var_names[[i]]} roughness")) +
    theme(legend.position = c(0.99, 0.99),
          legend.justification = c(1, 1),
          legend.title = element_blank())
  GCFR_environment_plots[[i]] <- plot_grid(absolute_panel, roughness_panel)
}
ggsave(here::here("figures/Cape-enviro-maps.png"),
  plot_grid(ncol = 2, plotlist = list(
    plot_grid(ncol = 1, plotlist =
      GCFR_environment_plots[1:5]
    ),
    plot_grid(ncol = 1, rel_heights = c(4, 1), plotlist = list(
      plot_grid(ncol = 1, plotlist =
        GCFR_environment_plots[6:9]
      ),
      grid.rect(gp = gpar(col = "white"))
    ))
  )),
  width = 10, height = 12,
  dpi = 300
)

# SWA --------------------------------------------------------------------------

SWAFR_environment_plots <- vector("list", length = length(var_names))
for (i in seq_along(var_names)) {
  absolute_panel <- gplot(SWAFR_variables_QDS[[i]]) +
    geom_tile(aes(fill = value)) +
    geom_spatial(SWAFR_border, fill = NA, col = "black") +
    scale_fill_viridis_c(na.value = NA) +
    labs(x = "Longitude (º)",
         y = "Latitude (º)",
         title = var_names[[i]]) +
    theme(legend.position = c(0.99, 0.99),
          legend.justification = c(1, 1),
          legend.title = element_blank())
  roughness_panel <- gplot(focal_sd(SWAFR_variables_QDS[[i]])) +
    geom_tile(aes(fill = value)) +
    geom_spatial(SWAFR_border, fill = NA, col = "black") +
    scale_fill_viridis_c(na.value = NA) +
    labs(x = "Longitude (º)",
         y = "Latitude (º)",
         title = glue("{var_names[[i]]} roughness")) +
    theme(legend.position = c(0.99, 0.99),
          legend.justification = c(1, 1),
          legend.title = element_blank())
  SWAFR_environment_plots[[i]] <- plot_grid(absolute_panel, roughness_panel)
}
ggsave(here::here("figures/SWA-enviro-maps.png"),
  plot_grid(ncol = 2, plotlist = list(
    plot_grid(ncol = 1, plotlist =
      SWAFR_environment_plots[1:5]
    ),
    plot_grid(ncol = 1, rel_heights = c(4, 1), plotlist = list(
      plot_grid(ncol = 1, plotlist =
        SWAFR_environment_plots[6:9]
      ),
      grid.rect(gp = gpar(col = "white"))
    ))
  )),
  width = 10, height = 12,
  dpi = 300
)

# Tidy-up workspace ------------------------------------------------------------

rm(GCFR_environment_plots, SWAFR_environment_plots)
