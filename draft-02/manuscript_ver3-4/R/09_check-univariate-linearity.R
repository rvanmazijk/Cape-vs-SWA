predictor_names <- c(str_replace_all(var_names, " ", "_"), "PC1")

# Import data
data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS.csv"))
)

# Define univariate model fitting helper function
fit_univariate_models <- function(response) {
  dataset <- data %$% {
    if      (response %in% c("log10(QDS_richness)", "QDS_richness")) QDS
    else if (response %in% c("log10(HDS_richness)", "HDS_richness")) HDS
    else if (response %in% c("log10(DS_richness)", "DS_richness"))  DS
  }

  univar_models <- map(predictor_names, ~ list(
    linear    = lm(glue("{response} ~ {.x}"),             dataset),
    quadratic = lm(glue("{response} ~ {.x} + I({.x}^2)"), dataset)
  ))
  names(univar_models) <- predictor_names

  univar_models %>%
    map_dfr(.id = "variable",
      ~ tibble(
        model_type = names(.x),
        model_rank = 1:2,
        model = .x
      )
    ) %>%
    group_by(variable) %>%
    mutate(
      slope           = map_dbl(model, ~tidy(.x)$estimate[2]),
      P_slope         = map_dbl(model, ~tidy(.x)$p.value[ 2]),
      quadratic_coeff = map2_dbl(model, model_type,
                          ~ ifelse(.y == "quadratic",
                            tidy(.x)$estimate[3],
                            NA
                          )
                        ),
      P_quadratic     = map2_dbl(model, model_type,
                          ~ ifelse(.y == "quadratic",
                            tidy(.x)$p.value[3],
                            NA
                          )
                        ),
      slope_sig       = case_when(
                          P_slope     < 0.001 ~ "***",
                          P_slope     < 0.01  ~ "**",
                          P_slope     < 0.05  ~ "*",
                          P_slope     < 0.1   ~ ".",
                          TRUE                ~ " "
                        ),
      quadratic_sig   = case_when(
                          P_quadratic < 0.001 ~ "***",
                          P_quadratic < 0.01  ~ "**",
                          P_quadratic < 0.05  ~ "*",
                          P_quadratic < 0.1   ~ ".",
                          TRUE                ~ " "
                        ),
      AIC             = map_dbl(model, AIC),
      delta_AIC       = AIC - min(AIC),
      best_model      = (model_rank == min(model_rank[delta_AIC < 2]))
    )
}

# Fit models (linear S response)
QDS_UVMs <- fit_univariate_models("QDS_richness")
HDS_UVMs <- fit_univariate_models("HDS_richness")
DS_UVMs  <- fit_univariate_models("DS_richness")

# Fit models (log10S response)
QDS_UVMs_log10 <- fit_univariate_models("log10(QDS_richness)")
HDS_UVMs_log10 <- fit_univariate_models("log10(HDS_richness)")
DS_UVMs_log10  <- fit_univariate_models("log10(DS_richness)")

# Make table
UVMs <- bind_rows(.id = "scale", list(
  QDS       = QDS_UVMs,
  HDS       = HDS_UVMs,
  DS        = DS_UVMs,
  QDS_log10 = QDS_UVMs_log10,
  HDS_log10 = HDS_UVMs_log10,
  DS_log10  = DS_UVMs_log10
))
UVMs_table <- UVMs %>%
  dplyr::select(
    variable, scale, model_type,
    slope, slope_sig, quadratic_coeff, quadratic_sig,
    best_model, delta_AIC
  ) %>%
  #mutate(best_model = ifelse(best_model, "*", " ")) %>%
  filter(best_model) %>%
  mutate(delta_AIC_sig = ifelse(delta_AIC > 2, "*", " ")) %>%
  mutate_if(is.numeric, ~format(round(., digits = 2), nsmall = 2)) %>%
  mutate_if(is.character, ~ifelse(str_detect(., "NA"), " ", .)) %>%
  arrange(scale, variable)

# Save table
write_csv(
  UVMs_table,
  here("draft-02/manuscript_ver3-4/results/check-univariate-linearity.csv"),
)
