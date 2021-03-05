# Heterogeneity and species richness:
#   Modelling vascular plant species richness as a function of
#   environmental heterogeneity across GCFR and SWAFR grid-cells
# Ruan van Mazijk, <ruanvmazijk@gmail.com>
# CC-BY-4.0 2021

# Import data ==================================================================

# .... Species richness and heterogeneity data ---------------------------------

richness_data <- list(
  QDS = read_csv("species-richness_QDS.csv"),
  HDS = read_csv("species-richness_HDS.csv"),
  DS  = read_csv("species-richness_DS.csv")
)

# .... Environmental heterogeneity ---------------------------------------------

heterogeneity_data <- list(
  QDS    = read_csv("heterogeneity_QDS.csv"),
  HDS    = read_csv("heterogeneity_HDS.csv"),
  DS     = read_csv("heterogeneity_DS.csv")
)

# .... Merge richness and heterogeneity datasets -------------------------------

data <- map2(richness_data, heterogeneity_data, full_join)

# Remove cells with any missing data (just in case)
data %<>% map(na.exclude)

# Check for collinearity among heterogeneity variables =========================

# .... Import heterogeneity raster-layers --------------------------------------

heterogeneity_ras <- map(c(QDS = "QDS",
                           HDS = "HDS",
                           DS  = "DS"), function(each_scale) {
  vars <- brick(
    map(var_names_tidy, function(each_var) {
      raster(glue("heterogeneity-{each_var}_{each_scale}.tif"))
    })
  )
  names(vars) <- var_names_tidy
  vars
})
heterogeneity_ras

# .... Do the check proper (using pairs()) -------------------------------------

imap(heterogeneity_ras, function(each_scale, each_scales_name) {
  pdf(
    width = 10, height = 10,
    glue("{each_scales_name}-log10-heterogeneity-pairs.pdf")
  )
  pairs(each_scale, cor = TRUE)
  dev.off()
})

# Univariate models ============================================================

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
# Choose m2 (heterogeneity + region)
summary(m2)
# Store residuals in master dataset for use in maps below
data$QDS$PC1_residual <- m3$residuals

# Double check that a quadratic version of m2 isn't better fitting?
# (Re: Allouche et al., Carnicer et al.)
m4 <- lm(QDS_richness ~ PC1 + I(PC1^2) + region, data$QDS)
AIC(m2, m4) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# It is, but **not** showing a hump-backed response
summary(m4)
visreg::visreg(m4, xvar = "PC1", by = "region", overlay = TRUE)
# Sort of pseudo-exponential?

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

# Double check that a quadratic version of m1 isn't better fitting?
m4 <- lm(HDS_richness ~ PC1 + I(PC1^2), data$HDS)
AIC(m1, m4) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# It's not!

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

# Double check that a quadratic version of m1 isn't better fitting?
m4 <- lm(DS_richness ~ PC1 + I(PC1^2), data$DS)
AIC(m1, m4) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# It's not!

# .... Fit other all vars univariate models ------------------------------------

predictor_names <- c(var_names_tidy, "PC1")
# (needed within fit_univariate_models())

QDS_UVMs <- fit_univariate_models("QDS_richness")
HDS_UVMs <- fit_univariate_models("HDS_richness")
DS_UVMs  <- fit_univariate_models("DS_richness")
# NOTE: fit_univariate_models() also saves the results to disc

# Multivariate models ==========================================================

# .... Fit multivariate models -------------------------------------------------

full_formula <- predictor_names[predictor_names != "PC1"] %>%
  {c(., paste(., "* region"))} %>%
  paste(collapse = " + ")

m_QDS_richness <- lm(glue("QDS_richness ~ {full_formula}"), data$QDS)
m_HDS_richness <- lm(glue("HDS_richness ~ {full_formula}"), data$HDS)
m_DS_richness  <- lm(glue("DS_richness  ~ {full_formula}"), data$DS)

m_QDS_richness %<>% step(direction = "backward", trace = 0)
m_HDS_richness %<>% step(direction = "backward", trace = 0)
m_DS_richness  %<>% step(direction = "backward", trace = 0)

# Check model assumptions
par(mfrow = c(2, 2))
plot(m_QDS_richness)
plot(m_HDS_richness)
plot(m_DS_richness)
par(op)

# Store residuals in master dataset for use in maps below
data$QDS$multivariate_residual <- m_QDS_richness$residuals
data$HDS$multivariate_residual <- m_HDS_richness$residuals
data$DS$multivariate_residual  <- m_DS_richness$residuals

# .... Summarise models --------------------------------------------------------

models <- list(
  QDS_richness = m_QDS_richness,
  HDS_richness = m_HDS_richness,
  DS_richness  = m_DS_richness
)

# Tidy results with `broom::tidy()` and `::glance`
models_summary <- models %>%
  map_df(.id = "response", tidy, conf.int = TRUE) %>%
  dplyr::select(-std.error, -statistic) %>%
  filter(term != "(Intercept)")
models_R2 <- models %>%
  map_df(.id = "response", glance) %>%
  dplyr::select(response, adj.r.squared)
models_summary %<>% full_join(models_R2)

# .... Look at ANOVAs of each model --------------------------------------------

model_ANOVAs <- models %>%
  map(anova) %>%
  map(tidy) %>%
  map(mutate, var_explained = sumsq / sum(sumsq)) %>%
  map(dplyr::select, term, var_explained, p.value) %>%
  map(arrange, desc(var_explained)) %>%
  bind_rows(.id = "response") %>%
  mutate(
    var_explained = var_explained %>%
      round(digits = 2) %>%
      format(nsmall = 2) %>%
      {ifelse(. == "0.00", "< 0.01", .)},
    p.value = case_when(
      is.na(p.value)  ~ "-",
      p.value < 0.001 ~ "***",
      p.value < 0.010 ~ "**",
      p.value < 0.050 ~ "*",
      p.value < 0.100 ~ ".",
      TRUE            ~ " "
    )
  )

# Print
as.data.frame(model_ANOVAs)

# Identify outliers (PC1- and MV-based) ========================================

data %<>% map(~ mutate(.x,
  is_PC1_outlier = as_vector(scale(PC1_residual)          > 2),
  is_MV_outlier  = as_vector(scale(multivariate_residual) > 2)
))

# Save results to disc =========================================================

# .... Save MVM summaries to disc ----------------------------------------------

write_csv(models_summary, "multivariate-model-results.csv")
write_csv(model_ANOVAs,   "multivariate-model-ANOVAs.csv")

# .... Rasterise model-residuals' data -----------------------------------------
# (And PC1 itself)

PC1_QDS_ras <- data$QDS %>%
  dplyr::select(region, qdgc, lon, lat, PC1) %>%
  rasterise_data("PC1", Larsen_grid_QDS_ras)

PC1_HDS_ras <- data$HDS %>%
  dplyr::select(region, hdgc, lon, lat, PC1) %>%
  rasterise_data("PC1", Larsen_grid_HDS_ras)

PC1_DS_ras <- data$DS %>%
  dplyr::select(region, dgc, lon, lat, PC1) %>%
  rasterise_data("PC1", Larsen_grid_DS_ras)

PC1_residual_QDS_ras <- data$QDS %>%
  dplyr::select(region, qdgc, lon, lat, PC1_residual) %>%
  rasterise_data("PC1_residual", Larsen_grid_QDS_ras)

PC1_residual_HDS_ras <- data$HDS %>%
  dplyr::select(region, hdgc, lon, lat, PC1_residual) %>%
  rasterise_data("PC1_residual", Larsen_grid_HDS_ras)

PC1_residual_DS_ras <- data$DS %>%
  dplyr::select(region, dgc, lon, lat, PC1_residual) %>%
  rasterise_data("PC1_residual", Larsen_grid_DS_ras)

MV_residual_QDS_ras <- data$QDS %>%
  dplyr::select(region, qdgc, lon, lat, multivariate_residual) %>%
  rasterise_data("multivariate_residual", Larsen_grid_QDS_ras)

MV_residual_HDS_ras <- data$HDS %>%
  dplyr::select(region, hdgc, lon, lat, multivariate_residual) %>%
  rasterise_data("multivariate_residual", Larsen_grid_HDS_ras)

MV_residual_DS_ras <- data$DS %>%
  dplyr::select(region, dgc, lon, lat, multivariate_residual) %>%
  rasterise_data("multivariate_residual", Larsen_grid_DS_ras)

# .... Save model residuals' rasters to disc -----------------------------------
# (And PC1 itself)

writeRaster(PC1_QDS_ras,          "heterogeneity-PC1_QDS.tif", overwrite = TRUE)
writeRaster(PC1_HDS_ras,          "heterogeneity-PC1_HDS.tif", overwrite = TRUE)
writeRaster(PC1_DS_ras,           "heterogeneity-PC1_DS.tif",  overwrite = TRUE)
writeRaster(PC1_residual_QDS_ras, "PC1-residual_QDS.tif",      overwrite = TRUE)
writeRaster(PC1_residual_HDS_ras, "PC1-residual_HDS.tif",      overwrite = TRUE)
writeRaster(PC1_residual_DS_ras,  "PC1-residual_DS.tif",       overwrite = TRUE)
writeRaster(MV_residual_QDS_ras,  "MV-residual_QDS.tif",       overwrite = TRUE)
writeRaster(MV_residual_HDS_ras,  "MV-residual_HDS.tif",       overwrite = TRUE)
writeRaster(MV_residual_DS_ras,   "MV-residual_DS.tif",        overwrite = TRUE)

# .... Save outlier grid-cell codes to disc ------------------------------------

data %>%
  bind_rows(.id = "scale") %>%
  filter(is_PC1_outlier | is_MV_outlier) %>%
  dplyr::select(
    scale, region,
    lon, lat, qdgc, hdgc, dgc,
    is_PC1_outlier, is_MV_outlier
  ) %>%
  mutate_if(is.logical, ~ifelse(., "*", " ")) %>%
  mutate_if(is.character, ~ifelse(is.na(.), " ", .)) %>%
  write_csv("list-outlier-squares.csv")

# Correlate PC1- and MV-based model results ====================================

# .... Derive pred. richness of each model from obs. richness & residuals ------

data$QDS %<>% mutate(
  PC1_expected          = QDS_richness - PC1_residual,
  multivariate_expected = QDS_richness - multivariate_residual
)
data$HDS %<>% mutate(
  PC1_expected          = HDS_richness - PC1_residual,
  multivariate_expected = HDS_richness - multivariate_residual
)
data$DS %<>% mutate(
  PC1_expected          = DS_richness  - PC1_residual,
  multivariate_expected = DS_richness  - multivariate_residual
)

# .... Correlate proper pred. richness / residuals from each model -------------

data %>%
  map_dfr(.id = "scale", cor_model_results) %>%
  dplyr::select(test, scale, estimate, p.value) %>%
  arrange(test, scale)
## # A tibble: 6 x 4
##   test     scale estimate   p.value
##   <fct>    <chr>    <dbl>     <dbl>
## 1 expected DS       0.723 1.35e-  6
## 2 expected HDS      0.699 2.21e- 31
## 3 expected QDS      0.680 9.79e-124
## 4 residual DS       0.369 3.17e-  2
## 5 residual HDS      0.834 2.61e- 54
## 6 residual QDS      0.908 0.

# Refit PC1-based models w/o outliers ==========================================

data2 <- data %>%
  map(filter, !is_PC1_outlier)

PC1_models <- vector("list", length = 3)
names(PC1_models) <- c("QDS_richness", "HDS_richness", "DS_richness")

# QDS-richness:
m1 <- lm(QDS_richness ~ PC1,          data2$QDS)
m2 <- lm(QDS_richness ~ PC1 + region, data2$QDS)
m3 <- lm(QDS_richness ~ PC1 * region, data2$QDS)
AIC(m1, m2, m3) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# Choose m2 (heterogeneity + region)
summary(m2)
PC1_models$QDS_richness <- m2

# HDS-richness:
m1 <- lm(HDS_richness ~ PC1,          data2$HDS)
m2 <- lm(HDS_richness ~ PC1 + region, data2$HDS)
m3 <- lm(HDS_richness ~ PC1 * region, data2$HDS)
AIC(m1, m2, m3) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# Choose m2 (heterogeneity + region)
summary(m2)
PC1_models$HDS_richness <- m2

# DS-richness:
m1 <- lm(DS_richness ~ PC1,          data2$DS)
m2 <- lm(DS_richness ~ PC1 + region, data2$DS)
m3 <- lm(DS_richness ~ PC1 * region, data2$DS)
AIC(m1, m2, m3) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# Choose m1 (heterogeneity main effect only)
summary(m1)
PC1_models$DS_richness <- m1

# Refit MVMs w/o outliers ======================================================

data3 <- data %>%
  map(filter, !is_MV_outlier)

# Fit multivariate models
full_formula <- var_names_tidy %>%
  {c(., paste(., "* region"))} %>%
  paste(collapse = " + ")

m_QDS_richness <- lm(glue("QDS_richness ~ {full_formula}"), data3$QDS)
m_HDS_richness <- lm(glue("HDS_richness ~ {full_formula}"), data3$HDS)
m_DS_richness  <- lm(glue("DS_richness  ~ {full_formula}"), data3$DS)

m_QDS_richness %<>% step(direction = "backward", trace = 0)
m_HDS_richness %<>% step(direction = "backward", trace = 0)
m_DS_richness  %<>% step(direction = "backward", trace = 0)

# Check model assumptions
par(mfrow = c(2, 2))
plot(m_QDS_richness)
plot(m_HDS_richness)
plot(m_DS_richness)
par(op)

# Summarise models
MV_models <- list(
  QDS_richness = m_QDS_richness,
  HDS_richness = m_HDS_richness,
  DS_richness  = m_DS_richness
)
models_summary <- MV_models %>%
  map_df(.id = "response", tidy, conf.int = TRUE) %>%
  dplyr::select(-std.error, -statistic) %>%
  filter(term != "(Intercept)")
models_R2 <- MV_models %>%
  map_df(.id = "response", glance) %>%
  dplyr::select(response, adj.r.squared)
models_summary %<>% full_join(models_R2)

# Compare variation of residuals before & after outlier removal =============

# Get PC1-based models' residuals
data$QDS$PC1_residual2 <- NA
data$HDS$PC1_residual2 <- NA
data$DS$PC1_residual2  <- NA
data$QDS$PC1_residual2[!data$QDS$is_PC1_outlier] <-
  residuals(PC1_models$QDS_richness)
data$HDS$PC1_residual2[!data$HDS$is_PC1_outlier] <-
  residuals(PC1_models$HDS_richness)
data$DS$PC1_residual2[!data$DS$is_PC1_outlier] <-
  residuals(PC1_models$DS_richness)

# Get MV-based models' residuals
data$QDS$multivariate_residual2 <- NA
data$HDS$multivariate_residual2 <- NA
data$DS$multivariate_residual2  <- NA
data$QDS$multivariate_residual2[!data$QDS$is_MV_outlier] <-
  residuals(MV_models$QDS_richness)
data$HDS$multivariate_residual2[!data$HDS$is_MV_outlier] <-
  residuals(MV_models$HDS_richness)
data$DS$multivariate_residual2[!data$DS$is_MV_outlier] <-
  residuals(MV_models$DS_richness)

data_residuals <- map_dfr(data, .id = "scale", dplyr::select,
  region,
  PC1_residual,  multivariate_residual,
  PC1_residual2, multivariate_residual2
)

# Calculate standard deviations of each kind of residual
data_residuals_SDs <- data_residuals %>%
  group_by(scale, region) %>%
  summarise_if(is.numeric, sd, na.rm = TRUE)

# Perform F-tests of the residuals from
# the models with outliers against those without outliers
data_residuals_F_tests <- data_residuals %>%
  gather(residual_type, residual_value, -scale, -region) %>%
  mutate(dataset = ifelse(str_detect(residual_type, "2"),
    "sans_outliers",
    "with_outliers"
  )) %>%
  mutate(residual_type = str_remove(residual_type, "2")) %>%
  group_by(scale, dataset, residual_type) %>%
  summarise(P_F_region = var.test(residual_value ~ region)$p.value) %>%
  mutate(P_F_region = round(P_F_region, digits = 3))


# Save results w/o outliers to disc ============================================

write_csv(models_summary,         "multivariate-model-results_refit.csv")
write_csv(data_residuals_SDs,     "comparing-residuals-w-and-wo-outliers.csv")
write_csv(data_residuals_F_tests, "comparing-residuals-w-and-wo-outliers_F-tests.csv")
