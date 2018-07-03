# Make Fig. 2 (Richness and turnover)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("figures/figure-setup.R"))

# Turnover vs geodist ----------------------------------------------------------

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

# Species richness vs richness vs turnover @HDS --------------------------------

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

# Appendix: Species richness vs richness vs turnover @3QDS ---------------------

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
