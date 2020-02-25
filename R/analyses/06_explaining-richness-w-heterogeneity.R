# Import data ------------------------------------------------------------------

# .... Species richness and heterogeneity data ---------------------------------

richness_data <- list(
  QDS = read_csv(glue("{data_dir}/richness-data-QDS.csv")),
  HDS = read_csv(glue("{data_dir}/richness-data-HDS.csv")),
  DS  = read_csv(glue("{data_dir}/richness-data-DS.csv"))
)

# .... Environmental heterogeneity ---------------------------------------------

heterogeneity_data <- list(
  QDS    = read_csv(glue("{data_dir}/heterogeneity-data-QDS.csv")),
  HDS    = read_csv(glue("{data_dir}/heterogeneity-data-HDS.csv")),
  DS     = read_csv(glue("{data_dir}/heterogeneity-data-DS.csv"))
)

# .... Merge richness and heterogeneity datasets -------------------------------

data <- map2(richness_data, heterogeneity_data, full_join)
data %<>% map(na.exclude)

# .... My Larsen-type grid polygons and rasters --------------------------------

Larsen_grid_EDS <- readOGR(
  glue("{data_dir}/Larsen_grid_EDS"),
  layer = "Larsen_grid_EDS"
)
Larsen_grid_QDS <- readOGR(
  glue("{data_dir}/Larsen_grid_QDS"),
  layer = "Larsen_grid_QDS"
)
Larsen_grid_HDS <- readOGR(
  glue("{data_dir}/Larsen_grid_HDS"),
  layer = "Larsen_grid_HDS"
)

Larsen_grid_EDS_ras <- raster(glue(
  "{data_dir}/raster-layers/",
  "Larsen_grid_EDS_ras.tif"
))
Larsen_grid_QDS_ras <- raster(glue(
  "{data_dir}/raster-layers/",
  "Larsen_grid_QDS_ras.tif"
))
Larsen_grid_HDS_ras <- raster(glue(
  "{data_dir}/raster-layers/",
  "Larsen_grid_HDS_ras.tif"
))
Larsen_grid_DS_ras <- raster(glue(
  "{data_dir}/raster-layers/",
  "Larsen_grid_DS_ras.tif"
))

# Check for collinearity among heterogeneity variables -------------------------

# .... Import heterogeneity raster-layers --------------------------------------

heterogeneity_ras <- map(c(QDS = "QDS",
                           HDS = "HDS",
                           DS  = "DS"), function(each_scale) {
  vars <- brick(map(var_names_tidy, function(each_var) {
    raster(glue(
      "{data_dir}/raster-layers/",
      "heterogeneity-{each_scale}_{each_var}.tif"
    ))
  }))
  names(vars) <- var_names_tidy
  vars
})
heterogeneity_ras

# .... Do the check proper (using pairs()) -------------------------------------

imap(heterogeneity_ras, function(each_scale, each_scales_name) {
  pdf(width = 10, height = 10, here(
    "figures",
    glue("{each_scales_name}-log10-heterogeneity-pairs.pdf")
  ))
  pairs(each_scale, cor = TRUE)
  dev.off()
})

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

predictor_names <- c(var_names_tidy, "PC1")
# (needed within fit_univariate_models())

QDS_UVMs <- fit_univariate_models("QDS_richness")
HDS_UVMs <- fit_univariate_models("HDS_richness")
DS_UVMs  <- fit_univariate_models("DS_richness")
# NOTE: fit_univariate_models() also saves the results to disc

#####

if (FALSE) {
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
    plot_grid(
      plotlist = QDS_UVMs$plot, nrow = 1, rel_widths = c(1, rep(0.9, 9))
    ),
    plot_grid(
      plotlist = HDS_UVMs$plot, nrow = 1, rel_widths = c(1, rep(0.9, 9))
    ),
    plot_grid(
      plotlist = DS_UVMs$plot,  nrow = 1, rel_widths = c(1, rep(0.9, 9))
    ),
    nrow = 3, rel_heights = c(0.9, 0.9, 1)
  )

  # Save to disc
  ggsave(
    here("figures/plot-univariate-models.pdf"),
    width = 25, height = 8,
    UVM_plots
  )
  ggsave(
    here("figures/plot-univariate-models.png"),
    width = 25, height = 8,
    UVM_plots
  )
}

# Multivariate models ----------------------------------------------------------

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

# Check
if (FALSE) {
  par(mfrow = c(2, 2))
  plot(m_QDS_richness)
  plot(m_HDS_richness)
  plot(m_DS_richness)
  par(op)
}

# Store residuals in master dataset for use in maps below
data$QDS$multivariate_residual <- m_QDS_richness$residuals
data$HDS$multivariate_residual <- m_HDS_richness$residuals
data$DS$multivariate_residual  <- m_DS_richness$residuals

# Identify outliers ------------------------------------------------------------

data %<>% map(~ mutate(.x,
  is_PC1_outlier = as_vector(scale(PC1_residual)          > 2),
  is_MV_outlier  = as_vector(scale(multivariate_residual) > 2)
))

# Save outlier grid-cell codes to disc -----------------------------------------

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
  write_csv(here("results/list-outlier-squares.csv"))

# .... Summarise models --------------------------------------------------------

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

# Look at ANOVAs of each model -------------------------------------------------

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

# Save results to disc ---------------------------------------------------------

# .... Save summaries to disc --------------------------------------------------

write_csv(models_summary, here("results/multivariate-model-results.csv"))
write_csv(model_ANOVAs,   here("results/multivariate-model-ANOVAs.csv"))

# .... Save new data with PC1- and multivariate-based residuals to disc --------

iwalk(data, ~write_csv(.x, glue("{data_dir}/data-{.y}-w-residuals.csv")))

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

# .... Save model resisudals' rasters to disc ----------------------------------
# (And PC1 itself)

writeRaster(
  PC1_QDS_ras,
  glue("{data_dir}/raster-layers/PC1_QDS.tif"),
  overwrite = TRUE
)

writeRaster(
  PC1_HDS_ras,
  glue("{data_dir}/raster-layers/PC1_HDS.tif"),
  overwrite = TRUE
)

writeRaster(
  PC1_DS_ras,
  glue("{data_dir}/raster-layers/PC1_DS.tif"),
  overwrite = TRUE
)

writeRaster(
  PC1_residual_QDS_ras,
  glue("{data_dir}/raster-layers/PC1_residual_QDS.tif"),
  overwrite = TRUE
)

writeRaster(
  PC1_residual_HDS_ras,
  glue("{data_dir}/raster-layers/PC1_residual_HDS.tif"),
  overwrite = TRUE
)

writeRaster(
  PC1_residual_DS_ras,
  glue("{data_dir}/raster-layers/PC1_residual_DS.tif"),
  overwrite = TRUE
)

writeRaster(
  MV_residual_QDS_ras,
  glue("{data_dir}/raster-layers/MV_residual_QDS.tif"),
  overwrite = TRUE
)

writeRaster(
  MV_residual_HDS_ras,
  glue("{data_dir}/raster-layers/MV_residual_HDS.tif"),
  overwrite = TRUE
)

writeRaster(
  MV_residual_DS_ras,
  glue("{data_dir}/raster-layers/MV_residual_DS.tif"),
  overwrite = TRUE
)

# Correlate PC1- and MV-based model results ------------------------------------

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

cor_model_results <- function(x) {
  x %$% as_tibble(rbind(
    cbind(
      test = "expected",
      tidy(cor.test(multivariate_expected, PC1_expected))
    ),
    cbind(
      test = "residual",
      tidy(cor.test(multivariate_residual, PC1_residual))
    )
  ))
}

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
