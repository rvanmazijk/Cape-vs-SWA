# Make figures and tables
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))

output_paths <- here::here("outputs", c(
  "04_roughness-across-scales/",
  "05_species-turnover-w-distance/",
  "06_species-turnover-and-richness/",
  "07_species-and-roughness/"
))
analysis_paths <- here::here("analyses", c(
  "04_analyse-roughness-across-scales.R",
  "05_analyse-species-turnover-w-distance.R",
  "06_analyse-species-turnover-and-richness.R",
  "07_analyse-species-and-roughness.R"
))
if (all(!folder_is_empty(output_paths))) {
  map(
    output_paths,
    import_all_objects_auto
  )
} else {
  source(here::here("analyses/01_import-region-polygons.R"))
  source(here::here("analyses/02_import-floral-data.R"))
  source(here::here("analyses/03_import-environmental-data.R"))
  map2(
    output_paths, analysis_paths,
    source_if_needed,
    import = TRUE
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

test_results_summary %<>%
  gather(resolution, sig, -variable) %>%
  filter(resolution %in% c("0.05 x 0.05", "0.75 x 0.75")) %>%
  mutate(sig = ifelse(sig, "", "NS"),
         region = "GCFR") %>%
  mutate(region = ifelse(region == "GCFR", "Cape", "SWA"))
test_results_summary <- data_for_violin_plot %>%
  group_by(variable) %>%
  summarise(y = 0.7 * max(z_roughness)) %>%
  full_join(test_results_summary)
test_results_summary$variable %<>% factor(levels = var_names)

test_results_CLES_for_plot %<>%
  mutate(resolution = ifelse(resolution == 0.05,
                             "0.05 x 0.05",
                             ifelse(resolution == 0.25,
                                    "0.25 x 0.25",
                                    ifelse(resolution == 0.50,
                                           "0.50 x 0.50",
                                           ifelse(resolution == 0.75,
                                                  "0.75 x 0.75",
                                                  NA)))))
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

data_for_violin_plot %<>% mutate(region = ifelse(region == "GCFR", "Cape", "SWA"))
data_for_violin_plot$variable %<>% factor(levels = var_names)

panel_a <- data_for_violin_plot %>%
  filter(resolution %in% c("0.05 x 0.05", "0.75 x 0.75"),
         variable %in% c("Elevation",
                         "MAP",
                         "PDQ",
                         "Surface T",
                         "NDVI")) %>%
  ggplot(aes(region, z_roughness, col = region)) +
  scale_color_discrete(name = "Region") +
  geom_violin() +
  ylab("Z(roughness)") +
  stat_summary(geom = "point", fun.y = median) +
  facet_grid(variable ~ resolution, scales = "free_y") +
  # Label significances (Mann-Whitney U)
  geom_text(data = test_results_summary %>%
              filter(resolution %in% c("0.05 x 0.05", "0.75 x 0.75"),
                     variable %in% c("Elevation",
                                     "MAP",
                                     "PDQ",
                                     "Surface T",
                                     "NDVI")),
            aes(y = y, label = sig),
            x = 1.5,
            col = "grey25") +
  # Label CLES
  geom_text(data = test_results_CLES_for_plot %>%
              filter(resolution %in% c("0.05 x 0.05", "0.75 x 0.75"),
                     variable %in% c("Elevation",
                                     "MAP",
                                     "PDQ",
                                     "Surface T",
                                     "NDVI")),
            aes(y = y, label = CLES),
            x = 1.5,
            col = "grey25") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

panel_b <- data_for_violin_plot %>%
  filter(resolution %in% c("0.05 x 0.05", "0.75 x 0.75"),
         variable %in% c("CEC",
                         "Clay",
                         "Soil C",
                         "pH")) %>%
  ggplot(aes(region, z_roughness, col = region)) +
  geom_violin() +
  stat_summary(geom = "point", fun.y = median) +
  facet_grid(variable ~ resolution, scales = "free_y") +
  # Label significances (Mann-Whitney U)
  geom_text(data = test_results_summary %>%
              filter(resolution %in% c("0.05 x 0.05", "0.75 x 0.75"),
                     variable %in% c("CEC",
                                     "Clay",
                                     "Soil C",
                                     "pH")),
            aes(y = y, label = sig),
            x = 1.5,
            col = "grey25") +
  # Label CLES
  geom_text(data = test_results_CLES_for_plot %>%
              filter(resolution %in% c("0.05 x 0.05", "0.75 x 0.75"),
                     variable %in% c("CEC",
                                     "Clay",
                                     "Soil C",
                                     "pH")),
            aes(y = y, label = CLES),
            x = 1.5,
            col = "grey25") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

region_legend <- gridExtra::arrangeGrob(panel_a)[[1]][[1]]$grobs[[37]]
panel_a <- panel_a + theme(legend.position = "none")

panel_b <- cowplot::plot_grid(
  panel_b, region_legend,
  nrow = 2, rel_heights = c(4, 0.9)
)
fig <- cowplot::plot_grid(
  panel_a, panel_b,
  nrow = 1, rel_widths = c(1, 0.9),
  labels = c("A", "B")
)

# Save
ggsave(
  here::here("figures/roughness-violins.png"),
  fig,
  width = 6, height = 6,
  dpi = 300
)

# .... Roughness IQuR ----------------------------------------------------------

# Plot GCFR vs SWAFR 95%-interquantile ranges ~ scale
IQ95R_data$variable %<>% factor(levels = var_names)
IQ95R_data %<>% mutate(region = ifelse(region == "GCFR", "Cape", "SWA"))
fig <- ggplot(IQ95R_data,
              aes(resolution, IXR,
                  col = region,
                  alpha = quantile,
                  group = paste(variable, region, quantile))) +
  geom_point() +
  geom_path() +
  labs(x = "Resolution",
       y = "IQuR") +
  facet_wrap(~ variable) +
  scale_color_discrete(name = "Region") +
  scale_alpha_continuous(name = "Quantile",
                         range = c(1.00, 0.40),
                         breaks = c(0.95, 0.99)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        strip.background = element_blank(),
        panel.grid = element_blank())

# Save
ggsave(
  here::here("figures/roughness-IQuR.png"),
  fig,
  dpi = 300
)

# Richness & turnover section --------------------------------------------------

# .... Turnover vs geodist -----------------------------------------------------

taxa_turnover_geodist <- cbind(rank = "species", species_turnover_geodist)
# TODO: Genus and family level analyses in appendix?

pink <- scales::hue_pal()(2)[[1]]
dark_pink <- "#D8564D"
bluu <- scales::hue_pal()(2)[[2]]
dark_bluu <- "#009FA4"

taxa_turnover_geodist %<>% mutate(region = ifelse(region == "GCFR", "Cape", "SWA"))
taxa_turnover_geodist$region %<>% factor(levels = c("SWA", "Cape"))

scatter_plot <- taxa_turnover_geodist %>%
  group_by(region) %>%
  sample_n(5000) %>%
  ggplot() +
  geom_point(aes(geodist, turnover, col = region), alpha = 0.25) +
  labs(x = "Distance between cells (m)",
       y = "Turnover (Jaccards distance)") +
  scale_colour_manual(name = "Region",
                      values = c(bluu, pink)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.75, 0.25))

rq_fits_added <- scatter_plot +
  geom_quantile(data = taxa_turnover_geodist %>%
                  filter(region == "Cape"),
                aes(geodist, turnover),
                col = dark_pink, size = 1,
                quantiles = 0.05,
                formula = y ~ log(x)) +
  geom_quantile(data = taxa_turnover_geodist %>%
                  filter(region == "SWA"),
                aes(geodist, turnover),
                col = dark_bluu, size = 1,
                quantiles = 0.05,
                formula = y ~ log(x))
fig <- rq_fits_added

# Save
ggsave(
  here::here("figures/turnover-vs-geodist.png"),
  fig,
  width = 4, height = 3,
  dpi = 300
)

# .... Gamma-beta-alpha @HDS ---------------------------------------------------

gamma_vs_alpha_HDS_plot <-
  ggplot(gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "species", ],
         aes(avg_QDS_richness, richness, col = region)) +
  geom_point() +
  # Log t/form to normalise avg_QDS_richness scores
  geom_smooth(formula = y ~ log(x + 1), method = "lm") +
  labs(x = "Mean QDS richness",
       y = "HDS richness") +
  scale_color_discrete(name = "Region") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.75, 0.25))
gamma_vs_beta_HDS_plot <-
  ggplot(gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "species", ],
         aes(avg_QDS_turnover, richness, col = region)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(x = "Mean QDS turnover",
       y = "HDS richness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
# TODO: Genus and family level analysis in appendix?

# Plot 2 panels + legend
fig <- cowplot::plot_grid(
  gamma_vs_alpha_HDS_plot, gamma_vs_beta_HDS_plot,
  nrow = 1, rel_widths = c(1, 0.9)
)

# Save
ggsave(
  here::here("figures/gamma-beta-alpha-HDS.png"),
  fig,
  width = 8, height = 4,
  dpi = 300
)


# .... Gamma-beta-alpha @3QDS --------------------------------------------------

gamma_vs_alpha_3QDS_plot <-
  ggplot(gamma_beta_alpha_3QDS[gamma_beta_alpha_3QDS$rank == "species", ],
         aes(avg_QDS_richness, richness, col = region)) +
  geom_point() +
  # Log t/form to normalise avg_QDS_richness scores
  geom_smooth(formula = y ~ log(x + 1), method = "lm") +
  labs(x = "Mean QDS richness",
       y = "3QDS richness") +
  scale_color_discrete(name = "Region") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.75, 0.25))
gamma_vs_beta_3QDS_plot <-
  ggplot(gamma_beta_alpha_3QDS[gamma_beta_alpha_3QDS$rank == "species", ],
         aes(avg_QDS_turnover, richness, col = region)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(x = "Mean QDS turnover",
       y = "3QDS richness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
# TODO: Genus and family level analysis in appendix?

# Plot 2 panels + legend
fig <- cowplot::plot_grid(
  gamma_vs_alpha_3QDS_plot, gamma_vs_beta_3QDS_plot,
  nrow = 1, rel_widths = c(1, 0.9)
)

# Save
ggsave(
  here::here("figures/gamma-beta-alpha-3QDS.png"),
  fig,
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
