# Collating output files from 1000 replicate runs &
#   999 replicate permuted-response runs of BRTS of
#   plant species richness at HDS-scale
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

rep_output_path <- here(
  "outputs/species-environment-relationships",
  "from-local-machines/HDS-richness-models_1000-reps"
)

perm_output_path <- here(
  "outputs/species-environment-relationships",
  "from-local-machines/HDS-richness-models_999-permuted-reps"
)

# Helper function --------------------------------------------------------------

import_replicate_outputs <- function(output_path,
                                     output_pattern = c("summary", "contribs"),
                                     output_slug = "_") {
  output_paths <- list.files(
    output_path,
    pattern = output_pattern,
    full.names = TRUE
  )
  outputs <- map(output_paths, ~
    .x %>%
      read_csv() %>%
      mutate(path = .x) %>%
      mutate(
        region = str_extract(path, "(GCFR|SWAFR)"),
        response = "richness",  # TODO: generalise
        scale = "HDS",  # TODO: generalise
        rep = path %>%
          str_extract(glue("{output_slug}\\d+\\.csv$")) %>%
          str_remove(output_slug) %>%
          str_remove("\\.csv")
      )
  )
  bind_rows(outputs)
}

# Import BRT summary and variable-contribution tables --------------------------

rep_summaries <-
  import_replicate_outputs(rep_output_path, "summary")
rep_contribs <-
  import_replicate_outputs(rep_output_path, "contribs")

perm_summaries <-
  import_replicate_outputs(perm_output_path, "summary", "_permutation-")
perm_contribs <-
  import_replicate_outputs(perm_output_path, "contribs", "_permutation-")

# Explore ----------------------------------------------------------------------

summary_data <- rbind(
  cbind(model_type = "replicates", rep_summaries),
  cbind(model_type = "permutations", perm_summaries)
)
summary_data %>%
  gather(quality_metric, value, nt:pred_obs_r2_exp) %>%
  mutate(quality_metric = case_when(
    quality_metric == "nt"              ~ "italic(nt)",
    quality_metric == "pseudo_r2"       ~ "italic(R)[italic(pseudo)]^2",
    quality_metric == "pred_obs_r2"     ~ "italic(R)[italic(E-O)]^2",
    quality_metric == "pred_obs_r2_exp" ~ "exp(italic(R)[italic(E-O)]^2)"
  )) %>%
  mutate(quality_metric = factor(quality_metric, levels = c(
    "italic(nt)",
    "italic(R)[italic(pseudo)]^2",
    "italic(R)[italic(E-O)]^2",
    "exp(italic(R)[italic(E-O)]^2)"  # TODO?: swap exp(R2E-O) for eR2E-O?
  ))) %>%
  ggplot(aes(value, fill = region, alpha = model_type)) +
    geom_histogram(
      position = position_dodge2(preserve = "single", padding = -1),
      bins = 25
    ) +
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
    )

contribs_data <- rbind(
  cbind(model_type = "replicates", rep_contribs),
  cbind(model_type = "permutations", perm_contribs)
)
ggplot(contribs_data, aes(rel.inf, fill = region, alpha = model_type)) +
  geom_histogram(
    position = position_dodge2(preserve = "single", padding = -1),
    bins = 25
  ) +
  facet_wrap(~ var, scales = "free") +
  scale_fill_manual(values = my_palette) +
  scale_alpha_manual(values = c(1, 0.65))

contribs_data_summary <- contribs_data %>%
  group_by(model_type, region, var) %>%
  summarise(
    mean_rel.inf = mean(rel.inf, na.rm = TRUE),
    sd_rel.inf = sd(rel.inf, na.rm = TRUE)
  ) %>%
  mutate(
    upper_sd = mean_rel.inf + sd_rel.inf,
    lower_sd = mean_rel.inf - sd_rel.inf
  ) %>%
  split(.$region) %>%
  map(~ split(.x, .x$model_type))
foreach(region_ = c("GCFR", "SWAFR")) %do% {
  screeplot <-
    ggplot(
      contribs_data_summary[[region_]]$replicates,
      aes(reorder(var, desc(mean_rel.inf)), mean_rel.inf, fill = var)
    ) +
    geom_col()
  screeplot <- screeplot +
    geom_point(
      data = contribs_data_summary[[region_]]$permutations,
      aes(y = mean_rel.inf),
      col = "grey25"
    ) +
    geom_errorbar(
      data = contribs_data_summary[[region_]]$permutations,
      aes(ymin = lower_sd, ymax = upper_sd),
      col = "grey25",
      width = 0
    )
  # TODO: mention in text that don't need replicate-value error bars,
  #   because VERY small
  #geom_errorbar(
  #  data = contribs_data_summary[[region_]]$replicates,
  #  aes(ymin = lower_sd, ymax = upper_sd),
  #  position = position_nudge(x = 0.25),
  #  col = "black",
  #  width = 0
  #)
  screeplot
}
