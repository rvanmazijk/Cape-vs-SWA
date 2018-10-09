library(tidyverse)
library(magrittr)
library(cowplot)
library(dismo)
source("Cape-vs-SWA_hpc-bare-minimum/R/functions/analyses/species-environment-relationship-functions.R")
models <- list(
  cape_richness = read_rds("final-BRT_GCFR_richness_BRTs.RDS"),
  swa_richness  = read_rds("final-BRT_SWAFR_richness_BRTs.RDS"),
  cape_turnover = read_rds("final-BRT_GCFR_turnover_BRTs.RDS"),
  swa_turnover  = read_rds("final-BRT_SWAFR_turnover_BRTs.RDS")
)
plot_grid(plotlist = imap(models, ~ .x$contributions %>%
  arrange(desc(rel.inf)) %>%
  mutate(var = factor(var, levels = var)) %>%
  mutate(var_type = ifelse(str_detect(var, "rough_"),
    "rough",
    "abs"
  )) %>%
  ggplot(aes(var, rel.inf, fill = var_type)) +
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
      subtitle = paste("pseudo-R2 =", round(pseudo_r2(.x), digits = 2))
    ) +
    theme_bw() +
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
    theme_bw() +
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
  ggplot(aes("", rel.inf, fill = reorder(var, rel.inf))) +
    geom_col() +
    coord_polar("y", start = 0) +
    facet_grid(response ~ region) +
    scale_fill_manual(values = c(
      "orange",
      "orange",
      "orange",
      "blue",
      "green",
      "orange",
      "grey25",
      "blue",
      "orange",
      "orange",
      "grey25",
      "blue",
      "blue"
    )) +
    theme_bw() +
    theme(axis.text.x = element_blank())
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
    theme_bw() +
    theme(axis.text.x = element_blank())
