# Make Fig. 1 (Environmental heterogeneity and scales)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("figures/figure-setup.R"))

# Roughness violin plots -------------------------------------------------------

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

# Roughness IQuR ----------------------------------------------------------=====
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

# Combine those figures --------------------------------------------------------
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

# Tidy-up workspace ------------------------------------------------------------

rm(
  panel_a, panel_b, panel_c, panel_d,
  roughness_combined
)
