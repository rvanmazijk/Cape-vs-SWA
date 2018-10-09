# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 4: Exploring the 4x final BRT-models
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

output_path <- here("outputs/species-environment-relationships/from-UCT-HPC/final-BRTs")

# Import final BRTs ------------------------------------------------------------

models <- list(
  cape_richness = read_rds(glue("{output_path}/final-BRT_GCFR_richness_BRTs.RDS")),
  swa_richness  = read_rds(glue("{output_path}/final-BRT_SWAFR_richness_BRTs.RDS")),
  cape_turnover = read_rds(glue("{output_path}/final-BRT_GCFR_turnover_BRTs.RDS")),
  swa_turnover  = read_rds(glue("{output_path}/final-BRT_SWAFR_turnover_BRTs.RDS"))
)
plot_grid(plotlist = imap(models, ~ .x$contributions %>%
  arrange(desc(rel.inf)) %>%
  mutate(var = factor(var, levels = var)) %>%
  mutate(
    var_type = ifelse(str_detect(var, "rough_"),
      "rough",
      "abs"
    ),
    var_class =
      case_when(
        str_detect(var, "Elevation")              ~ "Elevation",
        str_detect(var, "(MAP|PDQ|Surface\\.T)")  ~ "Climate",
        str_detect(var, "NDVI")                   ~ "NDVI",
        str_detect(var, "(CEC|Clay|Soil\\.C|pH)") ~ "Soil"
      ) %>%
      factor(levels = c("Elevation", "Climate", "NDVI", "Soil"))
  ) %>%
  ggplot(aes(var, rel.inf, fill = var_class)) +
    geom_col() +
    labs(
      x = "Environmental variable",
      y = "Relative influence",
      title = case_when(  # purrr::imap() sets the names of a list to .y
        .y == "cape_richness" ~ "Cape richness",
        .y == "swa_richness" ~ "SWA richness",
        .y == "cape_turnover" ~ "Cape turnover",
        .y == "swa_turnover" ~ "SWA turnover"
      ),
      subtitle = glue(
        "pseudo-R2 = {.x %>%
          pseudo_r2() %>%
          round(digits = 2)
        }, \\
        pred-obs-R2 = {.y %>%
          str_detect('richness') %>%
          ifelse(
            pred_obs_r2(.x)$pred_obs_m_exp,
            pred_obs_r2(.x)$pred_obs_m
          ) %>%
          round(digits = 2)
        }"
      )
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
))
models %>%
  map("contributions") %>%
  map_df(.id = "model", map_df, as.character) %>%
  mutate(
    rel.inf = as.numeric(rel.inf),
    region = str_extract(model, "(cape|swa)"),
    response = str_extract(model, "(richness|turnover)"),
    var_type = ifelse(str_detect(var, "rough_"),
      "rough",
      "abs"
    )
  ) %>%
  ggplot(aes(var, model, fill = rel.inf)) +
    geom_tile() +
    scale_fill_distiller(palette = "Spectral") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
models %>%
  map("contributions") %>%
  map_df(.id = "model", map_df, as.character) %>%
  mutate(
    rel.inf = as.numeric(rel.inf),
    region = str_extract(model, "(cape|swa)"),
    response = str_extract(model, "(richness|turnover)"),
    var_type = ifelse(str_detect(var, "rough_"),
      "rough",
      "abs"
    )
  ) %>%
  ggplot(aes("", rel.inf, fill = reorder(var_type, rel.inf))) +
    geom_col(col = "black") +
    coord_polar("y", start = 0) +
    facet_grid(response ~ region) +
    scale_fill_manual(values = c("white", "grey50")) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank()
    )
