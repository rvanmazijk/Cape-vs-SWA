# Make Fig. X (BRT model quality)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

output_path <- here("outputs/species-environment-relationships/from-local-machines")

# Import BRT quality results ---------------------------------------------------

summary_dataset_names <- c(
  "QDS-richness-models-summaries.csv",
  "HDS-richness-models-summaries.csv",
  "HDS-turnover-models-summaries.csv"
)
summary_data <- map_df(summary_dataset_names,
  ~ read_csv(glue("{output_path}/{.x}"))
)

n_succeeded_perms <- summary_data %>%
  filter(model_type == "permutations") %>%
  group_by(region, response, scale) %>%
  summarise(n = n())

nice_facet_labels <- list(
  qual = c(
    # "~ ~" = space
    "(a)~ ~italic(nt)",
    "(b)~ ~italic(R)[italic(pseudo)]^2",
    "(c)~ ~italic(R)[italic(E-O)]^2"
  ),
  scal = c(
    "QDS-richness",
    "HDS-richness",
    "HDS-turnover"
  )
)

# Make summary data long-form
summary_data %<>%
  select(scale, response, region, model_type, nt:pred_obs_r2) %>%
  gather(quality_metric, value, nt:pred_obs_r2) %>%
  # Make nice facet labels
  mutate(scale_response = glue("{scale}-{response}")) %>%
  mutate(scale_response = factor(scale_response, levels = nice_facet_labels$scal)) %>%
  mutate(quality_metric = case_when(
    quality_metric == "nt"              ~ nice_facet_labels$qual[[1]],
    quality_metric == "pseudo_r2"       ~ nice_facet_labels$qual[[2]],
    quality_metric == "pred_obs_r2"     ~ nice_facet_labels$qual[[3]]
  )) %>%
  # TODO?: swap exp(R2E-O) for eR2E-O?
  mutate(quality_metric = factor(quality_metric, levels = nice_facet_labels$qual))

# Quality-statistic distributions for the permuted and repeated BRTs -----------

quality_plot <- ggplot(summary_data, aes(value, fill = region, alpha = model_type)) +
  geom_histogram(
    position = position_dodge2(preserve = "single", padding = -1),
    bins = 25
  ) +
  labs(x = "Quality statistic value", y = "No. models") +
  facet_grid(
    scale_response ~ quality_metric,
    scales = "free",
    labeller = label_parsed
  ) +
  scale_fill_manual(name = "Region", values = my_palette) +
  scale_alpha_manual(name = "", values = c(0.5, 1), labels = c(
    "GCFR", "SWAFR"  # Cheat-labelling
  )) +
  guides(
    fill = guide_legend(
      title = "Replicates",
      nrow = 2, ncol = 1,
      direction = "vertical",
      override.aes = list(fill = my_palette, alpha = 1)
    ),
    alpha = guide_legend(
      title = "Permutations",
      nrow = 2, ncol = 2,
      direction = "vertical",
      override.aes = list(fill = my_palette, alpha = 0.5)
    )
  ) +
  theme(strip.text = element_text(hjust = 0))

# Save to disc -----------------------------------------------------------------

ggsave(
  here("figures/fig-3.X-BRT-quality-plot.png"),
  quality_plot,
  width = 8, height = 4,
  dpi = 300
)

