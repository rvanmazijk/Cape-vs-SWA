# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 4: Exploring the 4x final BRT-models
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))

output_path <- here(
  "outputs/species-environment-relationships/",
  "from-local-machines"
)

# Import final BRTs ------------------------------------------------------------

brt_output_path <- here(
  "outputs/species-environment-relationships/",
  "from-UCT-HPC/final-BRTs"
)
models <-
  list(
    cape_richness = "{brt_output_path}/final-BRT_GCFR_richness_BRTs.RDS",
    swa_richness  = "{brt_output_path}/final-BRT_SWAFR_richness_BRTs.RDS",
    cape_turnover = "{brt_output_path}/final-BRT_GCFR_turnover_BRTs.RDS",
    swa_turnover  = "{brt_output_path}/final-BRT_SWAFR_turnover_BRTs.RDS"
  ) %>%
  map(glue) %>%
  map(read_rds)

# Describe model quality -------------------------------------------------------

model_quality <- imap_dfr(models, ~ tibble(
  model_name = .y,
  nt = .x$n.trees,
  pseudo_r2 = pseudo_r2(.x),
  pred_obs_r2 = ifelse(str_detect(.y, "richness"),
    pred_obs_r2(.x)$pred_obs_m_exp,
    pred_obs_r2(.x)$pred_obs_m
  )
))

write_csv(
  model_quality,
  glue("{output_path}/model_quality.csv")
)

# Get variables' contributions to BRT predictions ------------------------------

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
zero_contrib_vars <- map_dfr(
  .x = list(
    list(region = "Cape", response = "richness"),
    list(region = "Cape", response = "turnover"),
    list(region = "SWA", response = "richness"),
    list(region = "SWA", response = "turnover")
  ), ~
    tibble(
      region = .x$region,
      response = .x$response,
      model_name = tolower(glue("{.x$region}_{.x$response}")),
      var = get_zero_contrib_vars(.x$region, .x$response),
      rel.inf = 0
    )
)
model_contributions %<>% full_join(zero_contrib_vars)
any(model_contributions$rel.inf == 0)
model_contributions %<>% mutate(
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

write_csv(
  model_contributions,
  glue("{output_path}/model_contributions.csv")
)
