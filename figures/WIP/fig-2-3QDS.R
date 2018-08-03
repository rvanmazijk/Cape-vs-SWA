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
fig_richness_vs_turnover_3QDS <- plot_grid(
  richness_vs_richness_3QDS_plot, richness_vs_turnover_3QDS_plot,
  nrow = 1, rel_widths = c(1, 0.9)
)

# Save
ggsave(
  here::here("figures/fig-2-richness-vs-turnover-3QDS.png"),
  fig_richness_vs_turnover_3QDS,
  width = 8, height = 4,
  dpi = 300
)
