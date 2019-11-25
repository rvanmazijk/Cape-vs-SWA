predictor_names <- c(str_replace_all(var_names, " ", "_"), "PC1")

# Import data
data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

# Identify outliers ------------------------------------------------------------

data %<>% map(~ mutate(.x,
  is_PC1_outlier = as_vector(scale(PC1_residual)          > 2),
  is_MV_outlier  = as_vector(scale(multivariate_residual) > 2)
))

data3 <- data %>%
  map(filter, !is_PC1_outlier)

data2 <- data %>%
  map(filter, !is_MV_outlier)

# Define univariate model fitting helper function ------------------------------

fit_univariate_models <- function(response, dataset) {

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

# Fit models -------------------------------------------------------------------

# .... With all outliers -------------------------------------------------------

# Linear S response
all_QDS_UVMs       <- fit_univariate_models("QDS_richness", data$QDS)
all_HDS_UVMs       <- fit_univariate_models("HDS_richness", data$HDS)
all_DS_UVMs        <- fit_univariate_models("DS_richness",  data$DS)

# log10S response
all_QDS_UVMs_log10 <- fit_univariate_models("log10(QDS_richness)", data$QDS)
all_HDS_UVMs_log10 <- fit_univariate_models("log10(HDS_richness)", data$HDS)
all_DS_UVMs_log10  <- fit_univariate_models("log10(DS_richness)",  data$DS)

# .... Without PC1-model outliers ----------------------------------------------

# Linear S response
PC1_QDS_UVMs       <- fit_univariate_models("QDS_richness", data3$QDS)
PC1_HDS_UVMs       <- fit_univariate_models("HDS_richness", data3$HDS)
PC1_DS_UVMs        <- fit_univariate_models("DS_richness",  data3$DS)

# log10S response
PC1_QDS_UVMs_log10 <- fit_univariate_models("log10(QDS_richness)", data3$QDS)
PC1_HDS_UVMs_log10 <- fit_univariate_models("log10(HDS_richness)", data3$HDS)
PC1_DS_UVMs_log10  <- fit_univariate_models("log10(DS_richness)",  data3$DS)

# .... Without MV-model outliers ----------------------------------------------

# Linear S response
MV_QDS_UVMs       <- fit_univariate_models("QDS_richness", data2$QDS)
MV_HDS_UVMs       <- fit_univariate_models("HDS_richness", data2$HDS)
MV_DS_UVMs        <- fit_univariate_models("DS_richness",  data2$DS)

# log10S response
MV_QDS_UVMs_log10 <- fit_univariate_models("log10(QDS_richness)", data2$QDS)
MV_HDS_UVMs_log10 <- fit_univariate_models("log10(HDS_richness)", data2$HDS)
MV_DS_UVMs_log10  <- fit_univariate_models("log10(DS_richness)",  data2$DS)

# Make tables ------------------------------------------------------------------

all_UVMs <- bind_rows(.id = "scale", list(
  QDS       = all_QDS_UVMs,
  HDS       = all_HDS_UVMs,
  DS        = all_DS_UVMs,
  QDS_log10 = all_QDS_UVMs_log10,
  HDS_log10 = all_HDS_UVMs_log10,
  DS_log10  = all_DS_UVMs_log10
))
PC1_UVMs <- bind_rows(.id = "scale", list(
  QDS       = PC1_QDS_UVMs,
  HDS       = PC1_HDS_UVMs,
  DS        = PC1_DS_UVMs,
  QDS_log10 = PC1_QDS_UVMs_log10,
  HDS_log10 = PC1_HDS_UVMs_log10,
  DS_log10  = PC1_DS_UVMs_log10
))
MV_UVMs <- bind_rows(.id = "scale", list(
  QDS       = MV_QDS_UVMs,
  HDS       = MV_HDS_UVMs,
  DS        = MV_DS_UVMs,
  QDS_log10 = MV_QDS_UVMs_log10,
  HDS_log10 = MV_HDS_UVMs_log10,
  DS_log10  = MV_DS_UVMs_log10
))
UVMs <- bind_rows(.id = "outliers", list(
  all_data = all_UVMs,
  sans_PC1 = PC1_UVMs,
  sans_MV  = MV_UVMs
))

UVMs_table <- UVMs %>%
  dplyr::select(
    variable, scale, outliers, model_type,
    slope, slope_sig, quadratic_coeff, quadratic_sig,
    best_model, delta_AIC
  ) %>%
  mutate(best_model = ifelse(best_model, "*", " ")) %>%
  #filter(best_model) %>%
  mutate(delta_AIC_sig = ifelse(delta_AIC > 2, "*", " ")) %>%
  mutate_if(is.numeric, ~format(round(., digits = 2), nsmall = 2)) %>%
  mutate_if(is.character, ~ifelse(str_detect(., "NA"), " ", .)) %>%
  arrange(scale, variable)

# Save tables
write_csv(
  UVMs_table,
  here(
    "draft-02/manuscript_ver3-4/results",
    "check-univariate-linearity.csv"
  ),
)
write_csv(
  UVMs_table[UVMs_table$best_model == "*", ],
  here(
    "draft-02/manuscript_ver3-4/results",
    "check-univariate-linearity_best-only.csv"
  ),
)
