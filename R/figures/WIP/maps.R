# Make maps of environmental and floral data
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("figures/figure-setup.R"))
map(pre_analysis_import_paths, source)

# TODO:
# - Fix legends overlapping with regions
# - Make region borders thinner
# - Make neat region borders with buffer(..., width = 0)
# - Remove inter-panel redudundant axes

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

GCFR_richness_QDS_tidy <- GCFR_richness_QDS
GCFR_richness_QDS_tidy[GCFR_richness_QDS_tidy == 0] <- NA
GCFR_richness_QDS_tidy <- mask(GCFR_richness_QDS_tidy, GCFR_border_buffered)
GCFR_richness_plot <- gplot(GCFR_richness_QDS_tidy) +
  geom_tile(aes(fill = value)) +
  geom_spatial(GCFR_border, fill = NA, col = "black") +
  lims(x = c(16, 16 + 15),
       y = c(-36, -36 + 11)) +
  scale_fill_viridis_c(limits = c(0, cellStats(GCFR_richness_QDS_tidy, max)),
                       na.value = NA) +
  labs(x = "Longitude (º)",
       y = "Latitude (º)",
       title = "Species richness") +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1),
        legend.title = element_blank())
ggsave(
  here::here("figures/Cape-richness-map.png"),
  GCFR_richness_plot,
  width = 5, height = 5,
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

SWAFR_richness_QDS_tidy <- SWAFR_richness_QDS
SWAFR_richness_QDS_tidy[SWAFR_richness_QDS_tidy == 0] <- NA
SWAFR_richness_QDS_tidy <- mask(SWAFR_richness_QDS_tidy, SWAFR_border_buffered)
SWAFR_richness_plot <- gplot(SWAFR_richness_QDS_tidy) +
  geom_tile(aes(fill = value)) +
  geom_spatial(SWAFR_border, fill = NA, col = "black") +
  lims(x = c(112, 112 + 15),
       y = c(-36, -36 + 11)) +
  scale_fill_viridis_c(limits = c(0, cellStats(GCFR_richness_QDS_tidy, max)),
                       na.value = NA) +
  labs(x = "Longitude (º)",
       y = "Latitude (º)",
       title = "Species richness") +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1),
        legend.title = element_blank())
ggsave(
  here::here("figures/SWA-richness-map.png"),
  SWAFR_richness_plot,
  width = 5, height = 5,
  dpi = 300
)

# Tidy-up workspace ------------------------------------------------------------

rm(
  GCFR_environment_plots,
  GCFR_richness_plot,
  SWAFR_environment_plots,
  SWAFR_richness_plot
)
