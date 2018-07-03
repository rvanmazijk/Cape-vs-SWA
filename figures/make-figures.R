# Make figures
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))

pre_analysis_import_paths <- list.files(
  here::here("analyses"),
  pattern = "^\\d{2}_import-.*\\.R",
  full.names = TRUE
)
analysis_paths <- list.files(
  here::here("analyses"),
  pattern = "^\\d{2}_analyse-.*\\.R",
  full.names = TRUE
)

no_ext <- "^[^.]+$"
output_paths <- list.files(
  here::here("outputs"),
  pattern = no_ext,
  full.names = TRUE
)

if (all(!folder_is_empty(output_paths))) {
  map(output_paths, import_objects)
} else {
  map(pre_analysis_import_paths, source)
  map2(
    output_paths, analysis_paths,
    source_if_needed, import = TRUE
  )
}

var_names <- c(
  "Elevation",
  "MAP",
  "PDQ",
  "Surface T",
  "NDVI",
  "CEC",
  "Clay",
  "Soil C",
  "pH"
)

# Environment section ----------------------------------------------------------

# .... Roughness violins -------------------------------------------------------

# Tidy-up significance labels
test_results_summary %<>%
  gather(resolution, sig, -variable) %>%
  filter(resolution %in% c("0.05º", "3QDS")) %>%
  mutate(sig = ifelse(sig, "", "NS"),
         region = "GCFR") %>%
  mutate(region = ifelse(region == "GCFR", "Cape", "SWA"))
test_results_summary <- data_for_violin_plot %>%
  group_by(variable) %>%
  summarise(y = 0.7 * max(z_roughness)) %>%
  full_join(test_results_summary)
test_results_summary$variable %<>% factor(levels = var_names)

# Tidy-up CLES labels
test_results_CLES_for_plot <- data_for_violin_plot %>%
  group_by(variable) %>%
  summarise(y = 0.9 * max(z_roughness)) %>%
  full_join(test_results_CLES_for_plot)
test_results_CLES_for_plot %<>%
  # Convert from CLES of SWAFR - GCFR > 0 to GCFR - SWAFR > 0
  # (i.e. to now show what proprtion of pairs the Cape is greater for)
  mutate(CLES = 1 - CLES) %>%
  # Now round and format
  mutate(CLES = round(CLES, digits = 2))
test_results_CLES_for_plot$variable %<>% factor(levels = var_names)

# Tidy-up raw distribution dataframe
data_for_violin_plot$variable %<>% factor(levels = var_names)

# Make panel for non-soil variables
panel_a <- data_for_violin_plot %>%
  filter(resolution %in% c("0.05º", "3QDS"),
         variable %in% var_names[1:5]) %>%
  ggplot(aes(region, z_roughness, col = region)) +
  geom_violin() +
  scale_colour_manual(name = "Region", values = my_palette) +
  labs(y = "Z(roughness)") +
  stat_summary(geom = "point", fun.y = median) +
  facet_grid(variable ~ resolution,
             scales = "free_y",
             switch = "x") +
  # Label significances (Mann-Whitney U)
  geom_text(data = test_results_summary %>%
              filter(resolution %in% c("0.05º", "3QDS"),
                     variable %in% var_names[1:5]),
            aes(y = y, label = sig),
            x = 1.5,
            col = "grey25") +
  # Label CLES
  geom_text(data = test_results_CLES_for_plot %>%
              filter(resolution %in% c("0.05º", "3QDS"),
                     variable %in% var_names[1:5]),
            aes(y = y, label = CLES),
            x = 1.5,
            col = "grey25") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text.y = element_blank())

# Make panel for soil variables
panel_b <- data_for_violin_plot %>%
  filter(resolution %in% c("0.05º", "3QDS"),
         variable %in% var_names[6:9]) %>%
  ggplot(aes(region, z_roughness, col = region)) +
  geom_violin() +
  scale_colour_manual(name = "Region", values = my_palette) +
  labs(y = "Z(roughness)") +
  stat_summary(geom = "point", fun.y = median) +
  facet_grid(variable ~ resolution,
             scales = "free_y",
             switch = "x") +
  # Label significances (Mann-Whitney U)
  geom_text(data = test_results_summary %>%
              filter(resolution %in% c("0.05º", "3QDS"),
                     variable %in% var_names[6:9]),
            aes(y = y, label = sig),
            x = 1.5,
            col = "grey25") +
  # Label CLES
  geom_text(data = test_results_CLES_for_plot %>%
              filter(resolution %in% c("0.05º", "3QDS"),
                     variable %in% var_names[6:9]),
            aes(y = y, label = CLES),
            x = 1.5,
            col = "grey25") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text.y = element_blank())
# Add extra whitespace below panel b, as it has 1 less sub-panel than panel a
panel_b <- plot_grid(
  panel_b, grid.rect(gp = gpar(col = "white")),
  nrow = 2, rel_heights = c(4, 0.9)
)

# .... Roughness IQuR ----------------------------------------------------------
# Plot GCFR vs SWAFR 95%-interquantile ranges ~ scale

# Tidy-up data, just in-case
IQ95R_data$variable %<>% factor(levels = var_names)
IQ95R_data$resolution %<>% factor(levels = c("0.05º", "QDS", "HDS", "3QDS"))

# Make panel for non-soil variables
panel_c <- IQ95R_data %>%
  filter(variable %in% var_names[1:5]) %>%
  ggplot(aes(resolution, IXR,
             col = region,
             alpha = quantile,
             group = paste(variable, region, quantile))) +
  geom_point() +
  geom_path() +
  labs(y = "IQuR") +
  facet_grid(variable ~ .) +
  ylim(min(IQ95R_data$IXR), max(IQ95R_data$IXR)) +
  scale_color_manual(name = "Region", values = my_palette) +
  scale_alpha_continuous(name = "Quantile",
                         range = c(1.00, 0.40),
                         breaks = c(0.95, 0.99)) +
  theme(legend.position = "none",
        axis.title.x = element_blank())

# Make panel for soil variables
panel_d <- IQ95R_data %>%
  filter(variable %in% var_names[6:9]) %>%
  ggplot(aes(resolution, IXR,
             col = region,
             alpha = quantile,
             group = paste(variable, region, quantile))) +
  geom_point() +
  geom_path() +
  labs(y = "IQuR") +
  facet_grid(variable ~ .) +
  ylim(min(IQ95R_data$IXR), max(IQ95R_data$IXR)) +
  scale_color_manual(name = "Region", values = my_palette) +
  scale_alpha_continuous(name = "Quantile",
                         range = c(1.00, 0.40),
                         breaks = c(0.95, 0.99)) +
  theme(axis.title.x = element_blank())
# Add extra whitespace below panel d, as it has 1 less sub-panel than panel c
panel_d <- plot_grid(
  panel_d, grid.rect(gp = gpar(col = "white")),
  nrow = 2, rel_heights = c(4, 0.9)
)

# .... Combine those figures ---------------------------------------------------
# With a tiny bit more padding on panels c,d to compensate for panels a,b
# having thicker x-axis/x-facet-label spacing

# Also note the order a,c,b,d is intentional
# (thought of this layout during design process)

roughness_combined <- plot_grid(
  panel_a,
  panel_c %>% plot_grid(grid.rect(gp = gpar(col = "white")),
                        nrow = 2, rel_heights = c(5, 0.1)),
  panel_b,
  panel_d %>% plot_grid(grid.rect(gp = gpar(col = "white")),
                         nrow = 2, rel_heights = c(4, 0.1)),
  nrow = 1, rel_widths = c(3, 3, 3, 4.5),
  labels = LETTERS[1:4]
)

# Save
ggsave(
  here::here("figures/roughness.png"),
  roughness_combined,
  width = 10, height = 5,
  dpi = 500
)

# Richness & turnover section --------------------------------------------------

# .... Turnover vs geodist -----------------------------------------------------

taxa_turnover_geodist <- cbind(rank = "species", species_turnover_geodist)
# TODO: Genus and family level analyses in appendix?

taxa_turnover_geodist %<>% mutate(region = ifelse(region == "GCFR", "Cape", "SWA"))
taxa_turnover_geodist$region %<>% factor(levels = c("SWA", "Cape"))
taxa_turnover_geodist$geodist %<>% divide_by(1000)  # m -> km

scatter_plot <- taxa_turnover_geodist %>%
  group_by(region) %>%
  sample_n(5000) %>%
  ggplot() +
  geom_point(aes(geodist, turnover,
                 col = region),
             alpha = 0.25) +
  labs(x = "Distance between cells (km)",
       y = "Pairwise QDS turnover") +
  scale_colour_manual(name = "Region",
                      values = rev(my_palette)) +  # Because SWA is plotted first
  guides(col = guide_legend(override.aes = list(alpha = 1.00))) +
  theme(legend.position = c(0.75, 0.25))

rq_fits_added <- scatter_plot +
  geom_quantile(data = taxa_turnover_geodist %>%
                  filter(region == "Cape"),
                aes(geodist, turnover),
                col = rgb(t(col2rgb(my_palette[1]) * 0.75),  # Darker
                          maxColorValue = 255),
                size = 1,
                quantiles = 0.05,
                formula = y ~ log(x)) +
  geom_quantile(data = taxa_turnover_geodist %>%
                  filter(region == "SWA"),
                aes(geodist, turnover),
                col = rgb(t(col2rgb(my_palette[2]) * 0.75),  # Darker
                          maxColorValue = 255),
                size = 1,
                quantiles = 0.05,
                formula = y ~ log(x))
fig_turnover_vs_geodist <- rq_fits_added

# Save
ggsave(
  here::here("figures/turnover-vs-geodist.png"),
  fig_turnover_vs_geodist,
  width = 4, height = 3,
  dpi = 300
)

# .... Species richness vs richness vs turnover @HDS ---------------------------

richness_vs_richness_HDS_plot <-
  ggplot(gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "species", ],
         aes(avg_QDS_richness, richness,
             col = region)) +
  geom_point() +
  # Log t/form to normalise avg_QDS_richness scores
  geom_smooth(formula = y ~ log(x + 1), method = "lm") +
  ylim(0, 3000) +
  labs(x = "Mean QDS richness",
       y = "HDS richness") +
  scale_color_manual(name = "Region", values = my_palette) +
  theme(legend.position = "none")
richness_vs_turnover_HDS_plot <-
  ggplot(gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "species", ],
         aes(avg_QDS_turnover, richness,
             col = region)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  ylim(0, 3000) +
  labs(x = "Mean QDS turnover",
       y = "HDS richness") +
  scale_color_manual(name = "Region", values = my_palette) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
# TODO: Genus and family level analysis in appendix?

# .... Combine those figures ---------------------------------------------------

fig_combined <- cowplot::plot_grid(
  plotlist = list(
    fig_turnover_vs_geodist,
    cowplot::plot_grid(
      plotlist = list(
        richness_vs_richness_HDS_plot,
        richness_vs_turnover_HDS_plot
      ),
      rel_widths = c(1.0, 0.8)
    )
  ),
  rel_widths = c(1.0, 1.9),
  labels = c("A", "B")
)
ggsave(
  here::here("figures/richness-vs-turnover-vs-geodist.png"),
  fig_combined,
  width = 10, height = 3,
  dpi = 300
)

# .... Extra: Species richness vs richness vs turnover @3QDS -------------------

richness_vs_richness_3QDS_plot <-
  ggplot(gamma_beta_alpha_3QDS[gamma_beta_alpha_3QDS$rank == "species", ],
         aes(avg_QDS_richness, richness,
             col = region)) +
  geom_point() +
  # Log t/form to normalise avg_QDS_richness scores
  geom_smooth(formula = y ~ log(x + 1), method = "lm") +
  ylim(0, 3000) +
  labs(x = "Mean QDS richness",
       y = "3QDS richness") +
  scale_color_manual(name = "Region", values = my_palette) +
  theme(legend.position = c(0.75, 0.25))
richness_vs_turnover_3QDS_plot <-
  ggplot(gamma_beta_alpha_3QDS[gamma_beta_alpha_3QDS$rank == "species", ],
         aes(avg_QDS_turnover, richness,
             col = region)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  ylim(0, 3000) +
  labs(x = "Mean QDS turnover",
       y = "3QDS richness") +
  scale_color_manual(name = "Region", values = my_palette) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
# TODO: Genus and family level analysis in appendix?

# Plot 2 panels + legend
fig_richness_vs_turnover_3QDS <- cowplot::plot_grid(
  richness_vs_richness_3QDS_plot, richness_vs_turnover_3QDS_plot,
  nrow = 1, rel_widths = c(1, 0.9)
)

# Save
ggsave(
  here::here("figures/richness-vs-turnover-3QDS.png"),
  fig_richness_vs_turnover_3QDS,
  width = 8, height = 4,
  dpi = 300
)

if (FALSE) {
  # TODO: Taxa & environment section -----------------------------------------
  ggplot(taxa_enviro_roughness_HDS[, c("region",
                                       "richness",
                                       "rank",
                                       "roughness_Elevation")],
         aes(roughness_Elevation, richness,
         col = region)) +
  geom_point() +
  facet_wrap(~ rank)
}
