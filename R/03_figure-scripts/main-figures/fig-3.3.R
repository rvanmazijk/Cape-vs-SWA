# Make Fig. X (HDS-scale turnover model quality)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

output_path <- here("outputs/species-environment-relationships/from-local-machines")

# Import BRT quality and contribution results ----------------------------------

summary_data <- read_csv(glue("{output_path}/HDS-turnover-models-summaries.csv"))

# Prepare data for figure ------------------------------------------------------

# For use in figure caption:
n_succeeded_perms <- summary_data %>%
  filter(model_type == "permutations") %>%
  nrow()

nice_facet_labels <- c(
  # "~ ~" = space
  "(a)~ ~italic(nt)",
  "(b)~ ~italic(R)[italic(pseudo)]^2",
  "(c)~ ~italic(R)[italic(E-O)]^2",
  "(d)~ ~exp(italic(R)[italic(E-O)]^2)"
)

# Make summary data long-form
summary_data %<>%
  gather(quality_metric, value, nt:pred_obs_r2_exp) %>%
  mutate(quality_metric = case_when(
    quality_metric == "nt"              ~ nice_facet_labels[[1]],
    quality_metric == "pseudo_r2"       ~ nice_facet_labels[[2]],
    quality_metric == "pred_obs_r2"     ~ nice_facet_labels[[3]],
    quality_metric == "pred_obs_r2_exp" ~ nice_facet_labels[[4]]
  )) %>%
  # Make nice facet labels
  # TODO?: swap exp(R2E-O) for eR2E-O?
  mutate(quality_metric = factor(quality_metric, levels = nice_facet_labels))

# Quality-statistic distributions for the permuted and repeated BRTs -----------

# Make figure
# (Used this figure in my 2018 research highlights slides
#   for Muasya discussion group)
quality_plots <-
  ggplot(summary_data, aes(value, fill = region, alpha = model_type)) +
    geom_histogram(
      position = position_dodge2(preserve = "single", padding = -1),
      bins = 25
    ) +
    labs(x = "Quality statistic value", y = "No. models") +
    facet_wrap(~ quality_metric, scales = "free", labeller = label_parsed) +
    scale_fill_manual(name = "Region", values = my_palette) +
    scale_alpha_manual(name = "", values = c(1, 0.5), labels = c(
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
  here("figures/fig-3.3-HDS-turnover-quality-plots.png"),
  quality_plots,
  width = 6, height = 4,
  dpi = 300
)
