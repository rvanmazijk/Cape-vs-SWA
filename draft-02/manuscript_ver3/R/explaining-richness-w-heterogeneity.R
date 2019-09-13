# Univariate models ------------------------------------------------------------

# .... Fit PC1 models ----------------------------------------------------------

# QDS-richness:
m1 <- lm(QDS_richness ~ PC1, data$QDS)
m2 <- lm(QDS_richness ~ PC1 + region, data$QDS)
m3 <- lm(QDS_richness ~ PC1 * region, data$QDS)
hist(residuals(m1))
hist(residuals(m2))
hist(residuals(m3))
AIC(m1, m2, m3) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# Choose m3 (heterogeneity * region interaction)
summary(m3)
# Store residuals in master dataset for use in maps below
data$QDS$PC1_residual <- m3$residuals

# HDS-richness:
m1 <- lm(HDS_richness ~ PC1, data$HDS)
m2 <- lm(HDS_richness ~ PC1 + region, data$HDS)
m3 <- lm(HDS_richness ~ PC1 * region, data$HDS)
hist(residuals(m1))
hist(residuals(m2))
hist(residuals(m3))
AIC(m1, m2, m3) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# Choose m1 (heterogeneity main effect only)
summary(m1)
# Store residuals in master dataset for use in maps below
data$HDS$PC1_residual <- m1$residuals

# DS-richness:
m1 <- lm(DS_richness ~ PC1, data$DS)
m2 <- lm(DS_richness ~ PC1 + region, data$DS)
m3 <- lm(DS_richness ~ PC1 * region, data$DS)
hist(residuals(m1))
hist(residuals(m2))
hist(residuals(m3))
AIC(m1, m2, m3) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# Choose m1 (heterogeneity main effect only)
summary(m1)
# Store residuals in master dataset for use in maps below
data$DS$PC1_residual <- m1$residuals

# .... Check for scale dependence formally w/ ANCOVA ---------------------------

#m1 <- lm(richness ~ PC1,         data_for_PC1_plots)
#m2 <- lm(richness ~ PC1 + scale, data_for_PC1_plots)
#m3 <- lm(richness ~ PC1 * scale, data_for_PC1_plots)
#AIC(m1, m2, m3) %>%
#  mutate(delta_AIC  = AIC - min(AIC))
## No evidence for interaction, only differing intercepts!
#summary(m2)
#m2 %>%
#  visreg::visreg(xvar = "PC1", by = "scale",
#    trans   = function(x) 10^x,
#    overlay = TRUE,
#    gg      = TRUE,
#    rug     = FALSE,
#    ylab    = bquote(italic("S"))
#  ) +
#  theme(legend.position = "none")

# .... Fit univariate QDS richness models --------------------------------------

predictor_names <- c(str_replace_all(var_names, " ", "_"), "PC1")

univar_models <- map(predictor_names,
  ~ list(
    non_region = lm(paste("QDS_richness ~", .x),             data$QDS),
    add_region = lm(paste("QDS_richness ~", .x, "+ region"), data$QDS),
    int_region = lm(paste("QDS_richness ~", .x, "* region"), data$QDS)
  )
)
names(univar_models) <- predictor_names

univar_model_summary <- univar_models %>%
  map_dfr(.id = "variable",
    ~ tibble(
      model_type = names(.x),
      model_rank = 1:3,
      model = .x
    )
  ) %>%
  group_by(variable) %>%
  mutate(
    slope        = map_dbl(model, ~tidy(.x)$estimate[2]),
    P_slope      = map_dbl(model, ~tidy(.x)$p.value[ 2]),
    region_coeff = map2_dbl(model, model_type,
                     ~ ifelse(.y != "non_region",
                       tidy(.x)$estimate[3],
                       NA
                   )
    ),
    P_region     = map2_dbl(model, model_type,
                     ~ ifelse(.y != "non_region",
                       tidy(.x)$p.value[3],
                       NA
                   )
    ),
    int_coeff    = map2_dbl(model, model_type,
                     ~ ifelse(.y == "int_region",
                       tidy(.x)$estimate[4],
                       NA
                   )
    ),
    P_int        = map2_dbl(model, model_type,
                     ~ ifelse(.y == "int_region",
                       tidy(.x)$p.value[4],
                       NA
                   )
    ),
    slope_sig    = ifelse(P_slope  < 0.05, "*", ""),
    region_sig   = ifelse(P_region < 0.05, "*", ""),
    int_sig      = ifelse(P_int    < 0.05, "*", ""),
    AIC          = map_dbl(model, AIC),
    delta_AIC    = AIC - min(AIC),
    best_model   = (model_rank == min(model_rank[delta_AIC < 2]))
  ) %>%
  filter(best_model) %>%
  ungroup() %>%
  mutate(
    model_type =
      case_when(
        model_type == "non_region"                   ~ "Main effect only",
        model_type == "add_region" & P_slope <  0.05 ~ "Main effect + region",
        model_type == "add_region" & P_slope >= 0.05 ~ "Region only",
        model_type == "int_region"                   ~ "Main effect * region"
      ) %>%
      factor(levels = c(
        "Main effect * region",
        "Main effect + region",
        "Main effect only",
        "Region only"
      )),
    variable = str_replace_all(variable, "_", " "),
    slope_sign  = ifelse(slope        > 0, "+", "-"),
    region_sign = ifelse(region_coeff > 0, "+", "-"),
    int_sign    = ifelse(int_coeff    > 0, "+", "-")
  ) %>%
  mutate_at(c("P_slope", "P_region", "P_int"),
    ~ case_when(
      .x < 0.001 ~ "***",
      .x < 0.010 ~ "**",
      .x < 0.050 ~ "*",
      .x < 0.100 ~ ".",
      TRUE       ~ " "
    )
  ) %>%
  mutate_if(is.character, ~ ifelse(is.na(.x), " ", .x)) %>%
  dplyr::select(
    model_type,  variable,
    slope_sign,  P_slope,
    region_sign, P_region,
    int_sign,    P_int
  ) %>%
  arrange(model_type)

# Remove variable names after first mention in table
univar_model_summary$model_type %<>% as.character()
for (pred in unique(univar_model_summary$model_type)) {
  to_remove <- which(univar_model_summary$model_type == pred)[-1]
  univar_model_summary$model_type[to_remove] <- " "
}

# No interaction models best-fitting, so remove those columns
univar_model_summary %<>% dplyr::select(-int_sign, -P_int)

# Print summary table
univar_model_summary

# .... Fit univariate HDS richness-models --------------------------------------

univar_models <- map(predictor_names,
  ~ list(
    non_region = lm(paste("HDS_richness ~", .x),             data$HDS),
    add_region = lm(paste("HDS_richness ~", .x, "+ region"), data$HDS),
    int_region = lm(paste("HDS_richness ~", .x, "* region"), data$HDS)
  )
)
names(univar_models) <- predictor_names

univar_model_summary <- univar_models %>%
  map_dfr(.id = "variable",
    ~ tibble(
      model_type = names(.x),
      model_rank = 1:3,
      model = .x
    )
  ) %>%
  group_by(variable) %>%
  mutate(
    slope        = map_dbl(model, ~tidy(.x)$estimate[2]),
    P_slope      = map_dbl(model, ~tidy(.x)$p.value[ 2]),
    region_coeff = map2_dbl(model, model_type,
                     ~ ifelse(.y != "non_region",
                       tidy(.x)$estimate[3],
                       NA
                     )
                   ),
    P_region     = map2_dbl(model, model_type,
                     ~ ifelse(.y != "non_region",
                       tidy(.x)$p.value[3],
                       NA
                     )
                   ),
    int_coeff    = map2_dbl(model, model_type,
                     ~ ifelse(.y == "int_region",
                       tidy(.x)$estimate[4],
                       NA
                     )
                   ),
    P_int        = map2_dbl(model, model_type,
                     ~ ifelse(.y == "int_region",
                       tidy(.x)$p.value[4],
                       NA
                     )
                   ),
    slope_sig    = ifelse(P_slope  < 0.05, "*", ""),
    region_sig   = ifelse(P_region < 0.05, "*", ""),
    int_sig      = ifelse(P_int    < 0.05, "*", ""),
    AIC          = map_dbl(model, AIC),
    delta_AIC    = AIC - min(AIC),
    best_model   = (model_rank == min(model_rank[delta_AIC < 2]))
  ) %>%
  filter(best_model) %>%
  ungroup() %>%
  mutate(
    model_type =
      case_when(
        model_type == "non_region"                   ~ "Main effect only",
        model_type == "add_region" & P_slope <  0.05 ~ "Main effect + region",
        model_type == "add_region" & P_slope >= 0.05 ~ "Region only",
        model_type == "int_region"                   ~ "Main effect * region"
      ) %>%
      factor(levels = c(
        "Main effect * region",
        "Main effect + region",
        "Main effect only",
        "Region only"
      )),
    variable = str_replace_all(variable, "_", " "),
    slope_sign  = ifelse(slope        > 0, "+", "-"),
    region_sign = ifelse(region_coeff > 0, "+", "-"),
    int_sign    = ifelse(int_coeff    > 0, "+", "-")
  ) %>%
  mutate_at(c("P_slope", "P_region", "P_int"),
    ~ case_when(
      .x < 0.001 ~ "***",
      .x < 0.010 ~ "**",
      .x < 0.050 ~ "*",
      .x < 0.100 ~ ".",
      TRUE       ~ " "
    )
  ) %>%
  mutate_if(is.character, ~ ifelse(is.na(.x), " ", .x)) %>%
  dplyr::select(
    model_type,  variable,
    slope_sign,  P_slope,
    region_sign, P_region,
    int_sign,    P_int
  ) %>%
  arrange(model_type)

# Remove variable names after first mention in table
univar_model_summary$model_type %<>% as.character()
for (pred in unique(univar_model_summary$model_type)) {
  to_remove <- which(univar_model_summary$model_type == pred)[-1]
  univar_model_summary$model_type[to_remove] <- " "
}

# All main effect only best-fitting, so remove other columns
univar_model_summary %<>% dplyr::select(
  -region_sign, -P_region,
  -int_sign,    -P_int
)

# Print summary table
univar_model_summary

# .... Fit univariate DS richness models ---------------------------------------

univar_models <- map(predictor_names,
  ~ list(
    non_region = lm(paste("DS_richness ~", .x),             data$DS),
    add_region = lm(paste("DS_richness ~", .x, "+ region"), data$DS),
    int_region = lm(paste("DS_richness ~", .x, "* region"), data$DS)
  )
)
names(univar_models) <- predictor_names

univar_model_summary <- univar_models %>%
  map_dfr(.id = "variable",
    ~ tibble(
      model_type = names(.x),
      model_rank = 1:3,
      model = .x
    )
  ) %>%
  group_by(variable) %>%
  mutate(
    slope        = map_dbl(model, ~tidy(.x)$estimate[2]),
    P_slope      = map_dbl(model, ~tidy(.x)$p.value[ 2]),
    region_coeff = map2_dbl(model, model_type,
                     ~ ifelse(.y != "non_region",
                       tidy(.x)$estimate[3],
                       NA
                     )
                   ),
    P_region     = map2_dbl(model, model_type,
                     ~ ifelse(.y != "non_region",
                        tidy(.x)$p.value[3],
                        NA
                     )
                   ),
    int_coeff    = map2_dbl(model, model_type,
                     ~ ifelse(.y == "int_region",
                       tidy(.x)$estimate[4],
                       NA
                     )
                   ),
    P_int        = map2_dbl(model, model_type,
                     ~ ifelse(.y == "int_region",
                       tidy(.x)$p.value[4],
                       NA
                     )
                   ),
    slope_sig    = ifelse(P_slope  < 0.05, "*", ""),
    region_sig   = ifelse(P_region < 0.05, "*", ""),
    int_sig      = ifelse(P_int    < 0.05, "*", ""),
    AIC          = map_dbl(model, AIC),
    delta_AIC    = AIC - min(AIC),
    best_model   = (model_rank == min(model_rank[delta_AIC < 2]))
  ) %>%
  filter(best_model) %>%
  ungroup() %>%
  mutate(
    model_type =
      case_when(
        model_type == "non_region"                   ~ "Main effect only",
        model_type == "add_region" & P_slope <  0.05 ~ "Main effect + region",
        model_type == "add_region" & P_slope >= 0.05 ~ "Region only",
        model_type == "int_region"                   ~ "Main effect * region"
      ) %>%
      factor(levels = c(
        "Main effect * region",
        "Main effect + region",
        "Main effect only",
        "Region only"
      )),
    variable = str_replace_all(variable, "_", " "),
    slope_sign  = ifelse(slope        > 0, "+", "-"),
    region_sign = ifelse(region_coeff > 0, "+", "-"),
    int_sign    = ifelse(int_coeff    > 0, "+", "-")
  ) %>%
  mutate_at(c("P_slope", "P_region", "P_int"),
    ~ case_when(
      .x < 0.001 ~ "***",
      .x < 0.010 ~ "**",
      .x < 0.050 ~ "*",
      .x < 0.100 ~ ".",
      TRUE       ~ " "
    )
  ) %>%
  mutate_if(is.character, ~ ifelse(is.na(.x), " ", .x)) %>%
  dplyr::select(
    model_type,  variable,
    slope_sign,  P_slope,
    region_sign, P_region,
    int_sign,    P_int
  ) %>%
  arrange(model_type)

# Remove variable names after first mention in table
univar_model_summary$model_type %<>% as.character()
for (pred in unique(univar_model_summary$model_type)) {
  to_remove <- which(univar_model_summary$model_type == pred)[-1]
  univar_model_summary$model_type[to_remove] <- " "
}

# No interaction models best-fitting, so remove those columns
univar_model_summary %<>% dplyr::select(-int_sign, -P_int)

# Print summary table
univar_model_summary

# Multivariate models ----------------------------------------------------------

# Fit multivariate models
full_formula <- predictor_names[predictor_names != "PC1"] %>%
  {c(., paste(., "* region"))} %>%
  paste(collapse = " + ")

m_QDS_richness <- lm(glue("QDS_richness ~ {full_formula}"), data$QDS)
m_HDS_richness <- lm(glue("HDS_richness ~ {full_formula}"), data$HDS)
m_DS_richness  <- lm(glue("DS_richness  ~ {full_formula}"),  data$DS)

m_QDS_richness %<>% step(direction = "backward", trace = 0)
m_HDS_richness %<>% step(direction = "backward", trace = 0)
m_DS_richness  %<>% step(direction = "backward", trace = 0)

# Reparameterise models to {*}:regionGCFR & {*}:regionSWAFR
# a.o.t. {*}*region, so that the figure of the effects actually represents
# each region, not the baseline (GCFR) and "relative SWAFR"
# (and that would cause inconsistencies too when their is no interaction with
# region term for a roughness variable).
reparameterise <- function(m) {
  response <- colnames(m$model)[[1]]
  dataset <- data %$% {
    if      (response == "QDS_richness") QDS
    else if (response == "HDS_richness") HDS
    else if (response == "DS_richness")  DS
  }
  preds_w_interactions <- m %$%
    coefficients %>%
    names() %>%
    magrittr::extract(str_which(., ":regionSWAFR"))
  reparameterisation <- preds_w_interactions %>%
    str_remove(":regionSWAFR") %>%
    {glue("-{.}")} %>%
    paste(collapse = " ")
  update(m,
    formula = glue(". ~ . {reparameterisation}"),
    data    = dataset
  )
}
# Test:
#   a <- m_HDS_richness
#   b <- reparameterise(m_HDS_richness)
#   AIC(a, b) # same model! :)
m_QDS_richness %<>% reparameterise()
m_HDS_richness %<>% reparameterise()
m_DS_richness  %<>% reparameterise()

# Summarise models
models <- list(
  QDS_richness = m_QDS_richness,
  HDS_richness = m_HDS_richness,
  DS_richness  = m_DS_richness
)
models_summary <- models %>%
  map_df(.id = "response", tidy, conf.int = TRUE) %>%
  dplyr::select(-std.error, -statistic) %>%
  filter(term != "(Intercept)")
models_R2 <- models %>%
  map_df(.id = "response", glance) %>%
  dplyr::select(response, adj.r.squared)
models_summary %<>% full_join(models_R2)
glance(m_QDS_richness)
glance(m_HDS_richness)
glance(m_DS_richness)
data$QDS$multivariate_residual <- m_QDS_richness$residuals
data$HDS$multivariate_residual <- m_HDS_richness$residuals
data$DS$multivariate_residual  <- m_DS_richness$residuals

data %>%
  bind_rows(.id = "scale") %>%
  write_csv(glue("{data_dir}/data.csv"))
