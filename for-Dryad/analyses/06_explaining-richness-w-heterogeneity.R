# Import richness and heterogeneity data ---------------------------------------

richness_data <- list(
  QDS = read_csv("for-Dryad/data/richness-data-QDS.csv"),
  HDS = read_csv("for-Dryad/data/richness-data-HDS.csv"),
  DS  = read_csv("for-Dryad/data/richness-data-DS.csv")
)

heterogeneity_data <- list(
  QDS    = read_csv("for-Dryad/data/heterogeneity-data-QDS.csv"),
  HDS    = read_csv("for-Dryad/data/heterogeneity-data-HDS.csv"),
  DS     = read_csv("for-Dryad/data/heterogeneity-data-DS.csv")
)

data <- map2(richness_data, heterogeneity_data, full_join)
data %<>% map(na.exclude)

predictor_names <- var_names %>%
  str_replace_all(" ", "_") %>%
  c("PC1")

# Univariate models ------------------------------------------------------------

# .... Fit PC1 models manually (worthwhile) ------------------------------------

# QDS-richness:
m1 <- lm(QDS_richness ~ PC1,          data$QDS)
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
m1 <- lm(HDS_richness ~ PC1,          data$HDS)
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
m1 <- lm(DS_richness ~ PC1,          data$DS)
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
    here("for-Dryad", glue("{response}_univariate_model_results.csv"))
  )

  # Return summary with models
  return(univar_model_summary1)
}

QDS_UVMs <- fit_univariate_models("QDS_richness")
HDS_UVMs <- fit_univariate_models("HDS_richness")
DS_UVMs  <- fit_univariate_models("DS_richness")

#####

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
QDS_UVMs$plot[[1]] %<>% {. + ylab(bquote(italic("S")["QDS"]))}
HDS_UVMs$plot[[1]] %<>% {. + ylab(bquote(italic("S")["HDS"]))}
DS_UVMs$plot[[1]]  %<>% {. + ylab(bquote(italic("S")["DS"]))}

UVM_plots <- plot_grid(
  plot_grid(plotlist = QDS_UVMs$plot, nrow = 1, rel_widths = c(1, rep(0.9, 9))),
  plot_grid(plotlist = HDS_UVMs$plot, nrow = 1, rel_widths = c(1, rep(0.9, 9))),
  plot_grid(plotlist = DS_UVMs$plot,  nrow = 1, rel_widths = c(1, rep(0.9, 9))),
  nrow = 3, rel_heights = c(0.9, 0.9, 1)
)

# Save to disc
ggsave(
  here("draft-02/figures/plot-univariate-models.pdf"),
  width = 25, height = 8,
  UVM_plots
)
ggsave(
  here("draft-02/figures/plot-univariate-models.png"),
  width = 25, height = 8,
  UVM_plots
)

# Multivariate models ----------------------------------------------------------

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

# Check
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
  write_csv(here(
    "draft-02/results",
    "model-summary-for-Tony.csv"
  ))

# Look at break down of variance explained (ANOVA) by each model
models %>%
  map(anova) %>%
  map(tidy) %>%
  map(mutate, var_explained = sumsq / sum(sumsq)) %>%
  map(dplyr::select, term, var_explained, p.value) %>%
  map(arrange, desc(var_explained)) %>%
  bind_rows(.id = "response") %>%
  write_csv(here(
    "draft-02/results",
    "model-ANOVA-for-Tony.csv"
  ))

# Store residuals in master dataset for use in maps below
data$QDS$multivariate_residual <- m_QDS_richness$residuals
data$HDS$multivariate_residual <- m_HDS_richness$residuals
data$DS$multivariate_residual  <- m_DS_richness$residuals

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
