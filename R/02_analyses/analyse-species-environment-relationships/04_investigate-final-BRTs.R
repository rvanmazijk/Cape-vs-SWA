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

# Screeplots v1 ----------------------------------------------------------------

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

# Heatmap v1 -------------------------------------------------------------------

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

# Piecharts v1 -----------------------------------------------------------------

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

# Screeplot + piecharts --------------------------------------------------------

model_quality <- imap_dfr(models, ~ tibble(
  model_name = .y,
  pseudo_r2 = pseudo_r2(.x),
  pred_obs_r2 = ifelse(str_detect(.y, "richness"),
    pred_obs_r2(.x)$pred_obs_m_exp,
    pred_obs_r2(.x)$pred_obs_m
  )
))
model_contributions <-
  map_dfr(models, .id = "model_name", "contributions") %>%
  as_tibble() %>%
  mutate(
    region = model_name %>%
      str_split("_") %>%
      map(1) %$%
      case_when(
        . == "cape" ~ "Cape",
        . == "swa" ~ "SWA"
      ),
    response = model_name %>%
      str_split("_") %>%
      map(2) %>%
      as_vector()
  )
get_zero_contrib_vars <- function(region_, response_) {
  all_vars <- levels(model_contributions$var)
  some_vars <- model_contributions %>%
    filter(region == region_, response == response_) %>%
    select(var) %>%
    as_vector()
  all_vars[!(all_vars %in% some_vars)]
}
zero_contrib_vars <- map_dfr(
  .x = list(
    list(region = "Cape", response = "richness"),
    list(region = "Cape", response = "turnover"),
    list(region = "SWA", response = "richness"),
    list(region = "SWA", response = "turnover")
  ), ~ tibble(
    region = .x$region,
    response = .x$response,
    var = get_zero_contrib_vars(.x$region, .x$response)
  )
)




    var_type = ifelse(str_detect(var, "rough_"),
      "Rough",
      "Absolute"
    ),
    var_class =
      case_when(
        str_detect(var, "Elevation")              ~ "Elevation",
        str_detect(var, "(MAP|PDQ|Surface\\.T)")  ~ "Climate",
        str_detect(var, "NDVI")                   ~ "NDVI",
        str_detect(var, "(CEC|Clay|Soil\\.C|pH)") ~ "Soil"
      ) %>%
      factor(levels = c("Elevation", "Climate", "NDVI", "Soil")),
    var = var %>%
      str_replace_all("(_|\\.)", " ") %>%
      str_replace("rough", "Rough") %>%
      factor()
  )



ggplot(model_contributions, aes(var, rel.inf, fill = var_class)) +
  geom_col(aes(group = paste(region, response)), col = "black", position = "dodge", drop = FALSE) +
  labs(
    x = "Environmental variable",
    y = "Relative influence (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ... --------------------------------------------------------------------------

plot_grid(plotlist = foreach(model_name_ = names(models)) %do% {
  model_contributions %>%
    filter(model_name == model_name_) %>%
    mutate(var = reorder(var, desc(rel.inf))) %>%
    ggplot(aes(var, rel.inf, fill = var_class), drop = FALSE) +
      geom_col() +
      ylim(0, 60) +
      labs(
        x = "Environmental variable",
        y = "Relative influence (%)",
        title = case_when(  # purrr::imap() sets the names of a list to .y
          model_name_ == "cape_richness" ~ "Cape richness",
          model_name_ == "swa_richness" ~ "SWA richness",
          model_name_ == "cape_turnover" ~ "Cape turnover",
          model_name_ == "swa_turnover" ~ "SWA turnover"
        )
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
})
