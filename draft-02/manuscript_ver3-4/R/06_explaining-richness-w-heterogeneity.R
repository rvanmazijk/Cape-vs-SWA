predictor_names <- c(str_replace_all(var_names, " ", "_"), "PC1")

# Import data ------------------------------------------------------------------

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS.csv"))
)

# Univariate models ------------------------------------------------------------

# .... Fit PC1 models manually (worthwhile) ------------------------------------

# Fiddling
#
## QDS-scale:
#m1   <- lm(QDS_richness ~ PC1_clim,                            data$QDS)
#m1.0 <- lm(QDS_richness ~ PC1_soil,                            data$QDS)
#m1.1 <- lm(QDS_richness ~ PC1_clim                   + region, data$QDS)
#m1.2 <- lm(QDS_richness ~                   PC1_soil + region, data$QDS)
#m2   <- lm(QDS_richness ~ PC1_clim        + PC1_soil,          data$QDS)
#m3   <- lm(QDS_richness ~ PC1_clim        + PC1_soil + region, data$QDS)
#m4   <- lm(QDS_richness ~ PC1_clim*region + PC1_soil,          data$QDS)
#m5   <- lm(QDS_richness ~ PC1_clim        + PC1_soil*region,   data$QDS)
#m6   <- lm(QDS_richness ~ PC1_clim*region + PC1_soil*region,   data$QDS)
#AIC(m1, m1.0, m1.1, m1.2, m2, m3, m4, m5, m6) %>%
#  mutate(delta_AIC  = AIC - min(AIC))  # m3 best (even considering m1.0--1.2)
#visreg::visreg(m3, xvar = "PC1_clim", by = "region", overlay = TRUE)
#visreg::visreg(m3, xvar = "PC1_soil", by = "region", overlay = TRUE)
#summary(m3)
#
## HDS-scale:
#m1   <- lm(HDS_richness ~ PC1_clim,                            data$HDS)
#m1.0 <- lm(HDS_richness ~ PC1_soil,                            data$HDS)
#m1.1 <- lm(HDS_richness ~ PC1_clim                   + region, data$HDS)
#m1.2 <- lm(HDS_richness ~                   PC1_soil + region, data$HDS)
#m2   <- lm(HDS_richness ~ PC1_clim        + PC1_soil,          data$HDS)
#m3   <- lm(HDS_richness ~ PC1_clim        + PC1_soil + region, data$HDS)
#m4   <- lm(HDS_richness ~ PC1_clim*region + PC1_soil,          data$HDS)
#m5   <- lm(HDS_richness ~ PC1_clim        + PC1_soil*region,   data$HDS)
#m6   <- lm(HDS_richness ~ PC1_clim*region + PC1_soil*region,   data$HDS)
#AIC(m1, m1.0, m1.1, m1.2, m2, m3, m4, m5, m6) %>%
#  mutate(delta_AIC  = AIC - min(AIC))  # m3 best (ignoring m1.0--1.2)
#visreg::visreg(m3, xvar = "PC1_clim", by = "region", overlay = TRUE)
#visreg::visreg(m3, xvar = "PC1_soil", by = "region", overlay = TRUE)
#summary(m3)
#visreg::visreg(m1.1, xvar = "PC1_clim", by = "region", overlay = TRUE)
#summary(m1.1)
#
## DS-scale:
#m1   <- lm(DS_richness ~ PC1_clim,                            data$DS)
#m1.0 <- lm(DS_richness ~ PC1_soil,                            data$DS)
#m1.1 <- lm(DS_richness ~ PC1_clim                   + region, data$DS)
#m1.2 <- lm(DS_richness ~                   PC1_soil + region, data$DS)
#m2   <- lm(DS_richness ~ PC1_clim        + PC1_soil,          data$DS)
#m3   <- lm(DS_richness ~ PC1_clim        + PC1_soil + region, data$DS)
#m4   <- lm(DS_richness ~ PC1_clim*region + PC1_soil,          data$DS)
#m5   <- lm(DS_richness ~ PC1_clim        + PC1_soil*region,   data$DS)
#m6   <- lm(DS_richness ~ PC1_clim*region + PC1_soil*region,   data$DS)
#AIC(m1, m1.0, m1.1, m1.2, m2, m3, m4, m5, m6) %>%
#  mutate(delta_AIC  = AIC - min(AIC))  # m1 best
#visreg::visreg(m1)
#summary(m1)
#
# /Fiddling

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

# Using these nos. in text:
sd(data$QDS$QDS_richness)
sd(data$HDS$HDS_richness)
sd(data$DS$DS_richness)

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

# .... Check for scale dependence formally w/ ANCOVA? --------------------------

#data_all_scales <- data %$% rbind(
#  QDS %>% transmute(
#    scale = "QDS", region = region,
#    richness = QDS_richness, PC1 = PC1
#  ),
#  HDS %>% transmute(
#    scale = "HDS", region = region,
#    richness = HDS_richness, PC1 = PC1
#  ),
#  DS %>% transmute(
#    scale = "DS", region = region,
#    richness = DS_richness, PC1 = PC1
#  )
#)
#
#m1 <- lm(richness ~ PC1 * scale,                  data_all_scales)
#m2 <- lm(richness ~ PC1 * scale + region,         data_all_scales)
#m3 <- lm(richness ~ PC1 * scale + region * scale, data_all_scales)
#m4 <- lm(richness ~ PC1 * scale * region,         data_all_scales)
#AIC(m1, m2, m3, m4) %>%
#  mutate(delta_AIC  = AIC - min(AIC))  # m2 best
#summary(m4)
#m4 %>%
#  visreg::visreg(xvar = "PC1", by = "scale",
#    overlay = TRUE,
#    gg      = TRUE,
#    ylab    = bquote(italic("S"))
#  ) +
#  theme(legend.position = "none")

# .... Fit other all vars univariate models ------------------------------------

# Define univariate model fitting helper function
fit_univariate_models <- function(response) {
  dataset <- data %$% {
    if      (response == "QDS_richness") QDS
    else if (response == "HDS_richness") HDS
    else if (response == "DS_richness")  DS
  }

  univar_models <- map(predictor_names,
    ~ list(
      non_region = lm(glue("{response} ~ {.x}"),          dataset),
      add_region = lm(glue("{response} ~ {.x} + region"), dataset),
      int_region = lm(glue("{response} ~ {.x} * region"), dataset)
    )
  )
  names(univar_models) <- predictor_names

  univar_model_summary1 <- univar_models %>%
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
      variable    = str_replace_all(variable, "_", " "),
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
    mutate_if(is.character, ~ ifelse(is.na(.x), " ", .x))

  # Make summary table
  univar_model_summary2 <- univar_model_summary1 %>%
    dplyr::select(
      model_type,  variable,
      slope,        P_slope,
      region_coeff, P_region,
      int_coeff,    P_int
    ) %>%
    mutate_if(is.numeric, ~format(round(., digits = 3), nsmall = 3)) %>%
    arrange(model_type)
  # Remove variable names after first mention in table
  univar_model_summary2$model_type %<>% as.character()
  for (pred in unique(univar_model_summary2$model_type)) {
    to_remove <- which(univar_model_summary2$model_type == pred)[-1]
    univar_model_summary2$model_type[to_remove] <- " "
  }

  # Save results to disc
  write_csv(
    univar_model_summary2,
    glue("{data_dir}/{response}_univariate_model_results.csv")
  )

  # Return summary with models
  return(univar_model_summary1)
}

QDS_UVMs <- fit_univariate_models("QDS_richness")
HDS_UVMs <- fit_univariate_models("HDS_richness")
DS_UVMs  <- fit_univariate_models("DS_richness")
QDS_UVMs$plot <- NA
HDS_UVMs$plot <- NA
DS_UVMs$plot  <- NA
QDS_UVMs$plot %<>% as.list()
HDS_UVMs$plot %<>% as.list()
DS_UVMs$plot  %<>% as.list()
my_visreg <- function(m) {
  terms <- names(m$coefficients)
  m_plot <-
    if ("regionSWAFR" %in% terms) {
      visreg::visreg(m,
        xvar = terms[[2]], by = "region",
        overlay = TRUE, gg = TRUE, points = list(alpha = 0.25)
      )
    } else {
      visreg::visreg(m,
        xvar = terms[[2]],
        overlay = TRUE, gg = TRUE, points = list(alpha = 0.25)
      )
    }
  m_plot + theme(
    axis.text.y     = element_text(angle = 90, hjust = 0.5),
    legend.position = "none"
  )
}
for (i in 1:10) {
  QDS_UVMs$model[[i]]$data <- data$QDS
  HDS_UVMs$model[[i]]$data <- data$HDS
  DS_UVMs$model[[i]]$data  <- data$DS
  QDS_UVMs$plot[[i]] <- my_visreg(QDS_UVMs$model[[i]])
  HDS_UVMs$plot[[i]] <- my_visreg(HDS_UVMs$model[[i]])
  DS_UVMs$plot[[i]]  <- my_visreg(DS_UVMs$model[[i]])
}
QDS_UVMs$plot %<>% map(~ . + theme(axis.title.x = element_blank()))
HDS_UVMs$plot %<>% map(~ . + theme(axis.title.x = element_blank()))
QDS_UVMs$plot[2:10] %<>% map(~ . + theme(axis.title.y = element_blank()))
HDS_UVMs$plot[2:10] %<>% map(~ . + theme(axis.title.y = element_blank()))
DS_UVMs$plot[2:10]  %<>% map(~ . + theme(axis.title.y = element_blank()))
UVM_plots <- plot_grid(
  plot_grid(plotlist = QDS_UVMs$plot, nrow = 1, rel_widths = c(1, rep(0.9, 9))),
  plot_grid(plotlist = HDS_UVMs$plot, nrow = 1, rel_widths = c(1, rep(0.9, 9))),
  plot_grid(plotlist = DS_UVMs$plot,  nrow = 1, rel_widths = c(1, rep(0.9, 9))),
  nrow = 3, rel_heights = c(0.9, 0.9, 1)
)
ggsave(
  here("draft-02/manuscript_ver3-4/figures/plot-univariate-models.pdf"),
  width = 20, height = 6,
  UVM_plots
)

# Multivariate models ----------------------------------------------------------

# Fiddling
#
## QDS-scale:
#m1 <- lm(
#  QDS_richness ~
#    Elevation +
#    MAP +
#    PDQ +
#    Surface_T +
#    NDVI +
#    CEC +
#    Clay +
#    Soil_C +
#    pH,
#  data = data$QDS
#)
#m1.5 <- lm(
#  QDS_richness ~
#    Elevation +
#    MAP +
#    PDQ +
#    Surface_T +
#    NDVI +
#    CEC +
#    Clay +
#    Soil_C +
#    pH +
#    region,
#  data = data$QDS
#)
#m2 <- lm(
#  QDS_richness ~
#    region*Elevation +
#    region*MAP +
#    region*PDQ +
#    region*Surface_T +
#    region*NDVI +
#    region*CEC +
#    region*Clay +
#    region*Soil_C +
#    region*pH,
#  data = data$QDS
#)
#m3 <- lm(
#  QDS_richness ~
#    Elevation +
#    MAP +
#    PDQ +
#    Surface_T +
#    NDVI,
#  data = data$QDS
#)
#m3.5 <- lm(
#  QDS_richness ~
#    Elevation +
#    MAP +
#    PDQ +
#    Surface_T +
#    NDVI +
#    region,
#  data = data$QDS
#)
#m4 <- lm(
#  QDS_richness ~
#    region*Elevation +
#    region*MAP +
#    region*PDQ +
#    region*Surface_T +
#    region*NDVI,
#  data = data$QDS
#)
#AIC(m1, m1.5, m2) %>%
#  mutate(delta_AIC = AIC - min(AIC))  # m2 best
#AIC(m3, m3.5, m4) %>%
#  mutate(delta_AIC = AIC - min(AIC))  # m4 best
#AIC(m1, m1.5, m2, m3, m3.5, m4) %>%
#  mutate(delta_AIC = AIC - min(AIC))  # m4 best
#m4 %>%
#  anova() %>%
#  tidy() %>%
#  transmute(term = term, var_explained = sumsq / sum(sumsq)) %>%
#  arrange(desc(var_explained))
#
## HDS-scale:
#m1 <- lm(
#  HDS_richness ~
#    Elevation +
#    MAP +
#    PDQ +
#    Surface_T +
#    NDVI +
#    CEC +
#    Clay +
#    Soil_C +
#    pH,
#  data = data$HDS
#)
#m1.5 <- lm(
#  HDS_richness ~
#    Elevation +
#    MAP +
#    PDQ +
#    Surface_T +
#    NDVI +
#    CEC +
#    Clay +
#    Soil_C +
#    pH +
#    region,
#  data = data$HDS
#)
#m2 <- lm(
#  HDS_richness ~
#    region*Elevation +
#    region*MAP +
#    region*PDQ +
#    region*Surface_T +
#    region*NDVI +
#    region*CEC +
#    region*Clay +
#    region*Soil_C +
#    region*pH,
#  data = data$HDS
#)
#m3 <- lm(
#  HDS_richness ~
#    Elevation +
#    MAP +
#    PDQ +
#    Surface_T +
#    NDVI,
#  data = data$HDS
#)
#m3.5 <- lm(
#  HDS_richness ~
#    Elevation +
#    MAP +
#    PDQ +
#    Surface_T +
#    NDVI +
#    region,
#  data = data$HDS
#)
#m4 <- lm(
#  HDS_richness ~
#    region*Elevation +
#    region*MAP +
#    region*PDQ +
#    region*Surface_T +
#    region*NDVI,
#  data = data$HDS
#)
#AIC(m1, m1.5, m2) %>%
#  mutate(delta_AIC = AIC - min(AIC))  # m1 best
#AIC(m3, m3.5, m4) %>%
#  mutate(delta_AIC = AIC - min(AIC))  # m3 best
#AIC(m1, m1.5, m2, m3, m3.5, m4) %>%
#  mutate(delta_AIC = AIC - min(AIC))  # m3 best
#m3 %>%
#  anova() %>%
#  tidy() %>%
#  transmute(term = term, var_explained = sumsq / sum(sumsq)) %>%
#  arrange(desc(var_explained))
#
## DS-scale:
#m1 <- lm(
#  DS_richness ~
#    Elevation +
#    MAP +
#    PDQ +
#    Surface_T +
#    NDVI +
#    CEC +
#    Clay +
#    Soil_C +
#    pH,
#  data = data$DS
#)
#m1.5 <- lm(
#  DS_richness ~
#    Elevation +
#    MAP +
#    PDQ +
#    Surface_T +
#    NDVI +
#    CEC +
#    Clay +
#    Soil_C +
#    pH +
#    region,
#  data = data$DS
#)
#m2 <- lm(
#  DS_richness ~
#    region*Elevation +
#    region*MAP +
#    region*PDQ +
#    region*Surface_T +
#    region*NDVI +
#    region*CEC +
#    region*Clay +
#    region*Soil_C +
#    region*pH,
#  data = data$DS
#)
#m3 <- lm(
#  DS_richness ~
#    Elevation +
#    MAP +
#    PDQ +
#    Surface_T +
#    NDVI,
#  data = data$DS
#)
#m3.5 <- lm(
#  DS_richness ~
#    Elevation +
#    MAP +
#    PDQ +
#    Surface_T +
#    NDVI +
#    region,
#  data = data$DS
#)
#m4 <- lm(
#  DS_richness ~
#    region*Elevation +
#    region*MAP +
#    region*PDQ +
#    region*Surface_T +
#    region*NDVI,
#  data = data$DS
#)
#AIC(m1, m1.5, m2) %>%
#  mutate(delta_AIC = AIC - min(AIC))  # m2 best
#AIC(m3, m3.5, m4) %>%
#  mutate(delta_AIC = AIC - min(AIC))  # m3 best
#AIC(m1, m1.5, m2, m3, m3.5, m4) %>%
#  mutate(delta_AIC = AIC - min(AIC))  # m2 best
#m2 %>%
#  anova() %>%
#  tidy() %>%
#  transmute(term = term, var_explained = sumsq / sum(sumsq)) %>%
#  arrange(desc(var_explained))
#
# /Fiddling

# Fit multivariate models
full_formula <- predictor_names[predictor_names != "PC1"] %>%
  {c(., paste(., "* region"))} %>%
  paste(collapse = " + ")

m_QDS_richness <- lm(glue("QDS_richness ~ {full_formula}"), data$QDS)
m_HDS_richness <- lm(glue("HDS_richness ~ {full_formula}"), data$HDS)
m_DS_richness  <- lm(glue("DS_richness  ~ {full_formula}"), data$DS)

m_QDS_richness %<>% step(direction = "backward", trace = 0)
m_HDS_richness %<>% step(direction = "backward", trace = 0)
m_DS_richness  %<>% step(direction = "backward", trace = 0)

# Summarise models (pre-reparameterisation)
models1 <- list(
  QDS_richness = m_QDS_richness,
  HDS_richness = m_HDS_richness,
  DS_richness  = m_DS_richness
)
models_summary1 <- models1 %>%
  map_df(.id = "response", tidy, conf.int = TRUE) %>%
  dplyr::select(-std.error, -statistic) %>%
  filter(term != "(Intercept)")
models1_R2 <- models %>%
  map_df(.id = "response", glance) %>%
  dplyr::select(response, adj.r.squared)
models_summary1 %<>% full_join(models1_R2)

# Save results out (especially for Tony)
models_summary1_95 <- models1 %>%
  map_df(.id = "response", tidy, conf.int = TRUE, conf.level = 0.95) %>%
  rename(conf.low.05 = conf.low, conf.high.05 = conf.high)
models_summary1_99 <- models1 %>%
  map_df(.id = "response", tidy, conf.int = TRUE, conf.level = 0.99) %>%
  rename(conf.low.01 = conf.low, conf.high.01 = conf.high)
full_join(models_summary1_95, models_summary1_99) %>%
  dplyr::select(-std.error, -statistic) %>%
  mutate_if(is.numeric, ~round(., digits = 3)) %>%
  mutate(p.value = ifelse(p.value < 0.001, "< 0.001", p.value)) %>%
  write_csv(here("alt-model-summary-for-Tony.csv"))

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

par(mfrow = c(2, 2))
plot(m_QDS_richness)
plot(m_HDS_richness)
plot(m_DS_richness)
par(op)

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

# Save results out (especially for Tony)
models_summary_95 <- models %>%
  map_df(.id = "response", tidy, conf.int = TRUE, conf.level = 0.95) %>%
  rename(conf.low.05 = conf.low, conf.high.05 = conf.high)
models_summary_99 <- models %>%
  map_df(.id = "response", tidy, conf.int = TRUE, conf.level = 0.99) %>%
  rename(conf.low.01 = conf.low, conf.high.01 = conf.high)
full_join(models_summary_95, models_summary_99) %>%
  dplyr::select(-std.error, -statistic) %>%
  mutate_if(is.numeric, ~round(., digits = 3)) %>%
  mutate(p.value = ifelse(p.value < 0.001, "< 0.001", p.value)) %>%
  write_csv(here("model-summary-for-Tony.csv"))

# Look at break down of variance explained (ANOVA) by each model
models %>%
  map(anova) %>%
  map(tidy) %>%
  map(mutate, var_explained = sumsq / sum(sumsq)) %>%
  map(dplyr::select, term, var_explained, p.value) %>%
  map(arrange, desc(var_explained))

# Save new data w/ residuals to disc
iwalk(data, ~write_csv(.x, glue("{data_dir}/data-{.y}-w-residuals.csv")))

# .... Store residuals in rasters ----------------------------------------------

# ........ QDS-scale -----------------------------------------------------------

# GCFR:
GCFR_QDS_multivariate_residuals        <- GCFR_heterogeneity$QDS$Elevation
GCFR_QDS_multivariate_residuals[]      <- NA
names(GCFR_QDS_multivariate_residuals) <- "multivariate_residual"
cell_nos <- cellFromXY(
  GCFR_QDS_multivariate_residuals,
  data %$%
    QDS %>%
    filter(region == "GCFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
GCFR_QDS_multivariate_residuals[cell_nos] <- data %$%
  QDS %>%
  filter(region == "GCFR") %>%
  pull(multivariate_residual)
# Check
plot(GCFR_QDS_multivariate_residuals)
plot(GCFR_border, add = TRUE)
# Works!
writeRaster(
  GCFR_QDS_multivariate_residuals,
  glue("{data_dir}/GCFR_QDS_multivariate_richness.tif"),
  overwrite = TRUE
)

# SWAFR:
SWAFR_QDS_multivariate_residuals        <- SWAFR_heterogeneity$QDS$Elevation
SWAFR_QDS_multivariate_residuals[]      <- NA
names(SWAFR_QDS_multivariate_residuals) <- "multivariate_residual"
cell_nos <- cellFromXY(
  SWAFR_QDS_multivariate_residuals,
  data %$%
    QDS %>%
    filter(region == "SWAFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
SWAFR_QDS_multivariate_residuals[cell_nos] <- data %$%
  QDS %>%
  filter(region == "SWAFR") %>%
  pull(multivariate_residual)
# Check
plot(SWAFR_QDS_multivariate_residuals)
plot(SWAFR_border, add = TRUE)
# Works!
writeRaster(
  SWAFR_QDS_multivariate_residuals,
  glue("{data_dir}/SWAFR_QDS_multivariate_richness.tif"),
  overwrite = TRUE
)

# ........ HDS-scale -----------------------------------------------------------

# GCFR:
GCFR_HDS_multivariate_residuals        <- GCFR_heterogeneity$HDS$Elevation
GCFR_HDS_multivariate_residuals[]      <- NA
names(GCFR_HDS_multivariate_residuals) <- "multivariate_residual"
cell_nos <- cellFromXY(
  GCFR_HDS_multivariate_residuals,
  data %$%
    HDS %>%
    filter(region == "GCFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
GCFR_HDS_multivariate_residuals[cell_nos] <- data %$%
  HDS %>%
  filter(region == "GCFR") %>%
  pull(multivariate_residual)
# Check
plot(GCFR_HDS_multivariate_residuals)
plot(GCFR_border, add = TRUE)
# Works!
writeRaster(
  GCFR_HDS_multivariate_residuals,
  glue("{data_dir}/GCFR_HDS_multivariate_richness.tif"),
  overwrite = TRUE
)

# SWAFR:
SWAFR_HDS_multivariate_residuals        <- SWAFR_heterogeneity$HDS$Elevation
SWAFR_HDS_multivariate_residuals[]      <- NA
names(SWAFR_HDS_multivariate_residuals) <- "multivariate_residual"
cell_nos <- cellFromXY(
  SWAFR_HDS_multivariate_residuals,
  data %$%
    HDS %>%
    filter(region == "SWAFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
SWAFR_HDS_multivariate_residuals[cell_nos] <- data %$%
  HDS %>%
  filter(region == "SWAFR") %>%
  pull(multivariate_residual)
# Check
plot(SWAFR_HDS_multivariate_residuals)
plot(SWAFR_border, add = TRUE)
# Works!
writeRaster(
  SWAFR_HDS_multivariate_residuals,
  glue("{data_dir}/SWAFR_HDS_multivariate_richness.tif"),
  overwrite = TRUE
)

# ........ DS-scale ------------------------------------------------------------

# GCFR:
GCFR_DS_multivariate_residuals        <- GCFR_heterogeneity$DS$Elevation
GCFR_DS_multivariate_residuals[]      <- NA
names(GCFR_DS_multivariate_residuals) <- "multivariate_residual"
cell_nos <- cellFromXY(
  GCFR_DS_multivariate_residuals,
  data %$%
    DS %>%
    filter(region == "GCFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
GCFR_DS_multivariate_residuals[cell_nos] <- data %$%
  DS %>%
  filter(region == "GCFR") %>%
  pull(multivariate_residual)
# Check
plot(GCFR_DS_multivariate_residuals)
plot(GCFR_border, add = TRUE)
# Works!
writeRaster(
  GCFR_DS_multivariate_residuals,
  glue("{data_dir}/GCFR_DS_multivariate_richness.tif"),
  overwrite = TRUE
)

# SWAFR:
SWAFR_DS_multivariate_residuals        <- SWAFR_heterogeneity$DS$Elevation
SWAFR_DS_multivariate_residuals[]      <- NA
names(SWAFR_DS_multivariate_residuals) <- "multivariate_residual"
cell_nos <- cellFromXY(
  SWAFR_DS_multivariate_residuals,
  data %$%
    DS %>%
    filter(region == "SWAFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
SWAFR_DS_multivariate_residuals[cell_nos] <- data %$%
  DS %>%
  filter(region == "SWAFR") %>%
  pull(multivariate_residual)
# Check
plot(SWAFR_DS_multivariate_residuals)
plot(SWAFR_border, add = TRUE)
# Works!
writeRaster(
  SWAFR_DS_multivariate_residuals,
  glue("{data_dir}/SWAFR_DS_multivariate_richness.tif"),
  overwrite = TRUE
)

# .... Scale-ANCOVA-like multivariate-model ------------------------------------

#vars <- c(
#  "region",
#  "richness",
#  str_replace_all(var_names, " ", "_")
#)
#data_all_scales <- data %$% rbind(
#  QDS %>%
#    rename(richness = QDS_richness) %>%
#    dplyr::select_at(vars) %>%
#    add_column(scale = "QDS"),
#  HDS %>%
#    rename(richness = HDS_richness) %>%
#    dplyr::select_at(vars) %>%
#    add_column(scale = "HDS"),
#  DS %>%
#    rename(richness = DS_richness) %>%
#    dplyr::select_at(vars) %>%
#    add_column(scale = "DS")
#)
#
#full_formula <- predictor_names[predictor_names != "PC1"] %>%
#  {c(., paste(., "* region"), paste(., "* scale"))} %>%
#  paste(collapse = " + ")
#m_all_scales <- lm(glue("richness ~ {full_formula}"), data_all_scales)
#m_all_scales %<>% step(direction = "backward")
#
#summary(m_all_scales)
#
#reparameterise <- function(m) {
#  preds_w_interactions <- m %$%
#    coefficients %>%
#    names() %>%
#    magrittr::extract(which(
#      str_detect(., ":regionSWAFR") |
#      str_detect(., ":scale(H|Q)DS")
#    ))
#  reparameterisation <- preds_w_interactions %>%
#    str_remove(":regionSWAFR") %>%
#    str_remove(":scaleQDS") %>%
#    str_remove(":scaleHDS") %>%
#    str_remove_all(":") %>%
#    {glue("-{.}")} %>%
#    paste(collapse = " ")
#  update(m,
#    formula = glue(". ~ . {reparameterisation}"),
#    data    = data_all_scales
#  )
#}
#m_all_scales %>%
#  reparameterise() %>%
#  tidy(conf.int = TRUE) %>%
#  mutate(
#    region = case_when(
#      str_detect(term, "GCFR")  ~ "GCFR",
#      str_detect(term, "SWAFR") ~ "SWAFR",
#      TRUE                      ~ ""
#    ),
#    scale = case_when(
#      str_detect(term, "QDS") ~ "QDS",
#      str_detect(term, "HDS") ~ "HDS",
#      str_detect(term, "DS")  ~ "DS",
#      TRUE                    ~ ""
#    ),
#    term_type = case_when(
#      str_detect(term, "region") ~ "region",
#      str_detect(term, "scale")  ~ "scale",
#      TRUE                       ~ ""
#    ),
#    var = term %>%
#      str_remove("region(GC|SWA)FR") %>%
#      str_remove("scale(H|Q)?DS") %>%
#      str_remove("\\(Intercept\\)") %>%
#      str_remove_all(":"),
#    sig = p.value < 0.05
#  ) %>%
#  filter(sig) %>%
#  ggplot(aes(var, estimate, colour = region, shape = scale)) +
#    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
#    geom_point() +
#    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
#    #facet_wrap(~term_type, scales = "free_x") +
#    theme(axis.text.x = element_text(angle = 90))
