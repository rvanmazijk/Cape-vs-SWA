# Import data ------------------------------------------------------------------

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

# Identify outliers ------------------------------------------------------------

data %<>% map(~ mutate(.x,
  is_PC1_outlier = as_vector(scale(PC1_residual) > 1.96)
))

# Save out stuff for Tony
data %>%
  bind_rows(.id = "scale") %>%
  filter(is_PC1_outlier | is_MV_outlier) %>%
  dplyr::select(
    scale, region,
    lon, lat, QDS, HDS, DS,
    is_PC1_outlier, is_MV_outlier
  ) %>%
  mutate_if(is.logical, ~ifelse(., "*", " ")) %>%
  mutate_if(is.character, ~ifelse(is.na(.), " ", .)) %>%
  write_csv(here("draft-02/manuscript_ver3-4/list-outlier-squares.csv"))

# .... Maps of outliers --------------------------------------------------------

GCFR_border  <- readOGR(here("data/derived-data/borders/GCFR_border"))
SWAFR_border <- readOGR(here("data/derived-data/borders/SWBP_Mike-Cramer"))

data %$% {
  pdf(
    here("draft-02/manuscript_ver3-4/figures/map-mv-outliers.pdf"),
    width = 5, height = 7
  )
  par(mfrow = c(3, 2), mar = c(0, 0, 0, 0))
  QDS %$% {
    plot(GCFR_border)
    points(
      .[is_MV_outlier & (multivariate_residual > 0), c("lon", "lat")],
      pch = 19, col = "red"
    )
    points(
      .[is_MV_outlier & (multivariate_residual < 0), c("lon", "lat")],
      pch = 19, col = "blue"
    )
    plot(SWAFR_border)
    points(
      .[is_MV_outlier & (multivariate_residual > 0), c("lon", "lat")],
      pch = 19, col = "red"
    )
    points(
      .[is_MV_outlier & (multivariate_residual < 0), c("lon", "lat")],
      pch = 19, col = "blue"
    )
  }
  HDS %$% {
    plot(GCFR_border)
    points(
      .[is_MV_outlier & (multivariate_residual > 0), c("lon", "lat")],
      pch = 19, col = "red"
    )
    points(
      .[is_MV_outlier & (multivariate_residual < 0), c("lon", "lat")],
      pch = 19, col = "blue"
    )
    plot(SWAFR_border)
    points(
      .[is_MV_outlier & (multivariate_residual > 0), c("lon", "lat")],
      pch = 19, col = "red"
    )
    points(
      .[is_MV_outlier & (multivariate_residual < 0), c("lon", "lat")],
      pch = 19, col = "blue"
    )
  }
  DS %$% {
    plot(GCFR_border)
    points(
      .[is_MV_outlier & (multivariate_residual > 0), c("lon", "lat")],
      pch = 19, col = "red"
    )
    points(
      .[is_MV_outlier & (multivariate_residual < 0), c("lon", "lat")],
      pch = 19, col = "blue"
    )
    plot(SWAFR_border)
    points(
      .[is_MV_outlier & (multivariate_residual > 0), c("lon", "lat")],
      pch = 19, col = "red"
    )
    points(
      .[is_MV_outlier & (multivariate_residual < 0), c("lon", "lat")],
      pch = 19, col = "blue"
    )
  }
  par(op)
  dev.off()
}

data %$% {
  pdf(
    here("draft-02/manuscript_ver3-4/figures/map-PC1-outliers.pdf"),
    width = 5, height = 7
  )
  par(mfrow = c(3, 2), mar = c(0, 0, 0, 0))
  QDS %$% {
    plot(GCFR_border)
    points(
      .[is_PC1_outlier & (PC1_residual > 0), c("lon", "lat")],
      pch = 19, col = "red"
    )
    points(
      .[is_PC1_outlier & (PC1_residual < 0), c("lon", "lat")],
      pch = 19, col = "blue"
    )
    plot(SWAFR_border)
    points(
      .[is_PC1_outlier & (PC1_residual > 0), c("lon", "lat")],
      pch = 19, col = "red"
    )
    points(
      .[is_PC1_outlier & (PC1_residual < 0), c("lon", "lat")],
      pch = 19, col = "blue"
    )
  }
  HDS %$% {
    plot(GCFR_border)
    points(
      .[is_PC1_outlier & (PC1_residual > 0), c("lon", "lat")],
      pch = 19, col = "red"
    )
    points(
      .[is_PC1_outlier & (PC1_residual < 0), c("lon", "lat")],
      pch = 19, col = "blue"
    )
    plot(SWAFR_border)
    points(
      .[is_PC1_outlier & (PC1_residual > 0), c("lon", "lat")],
      pch = 19, col = "red"
    )
    points(
      .[is_PC1_outlier & (PC1_residual < 0), c("lon", "lat")],
      pch = 19, col = "blue"
    )
  }
  DS %$% {
    plot(GCFR_border)
    points(
      .[is_PC1_outlier & (PC1_residual > 0), c("lon", "lat")],
      pch = 19, col = "red"
    )
    points(
      .[is_PC1_outlier & (PC1_residual < 0), c("lon", "lat")],
      pch = 19, col = "blue"
    )
    plot(SWAFR_border)
    points(
      .[is_PC1_outlier & (PC1_residual > 0), c("lon", "lat")],
      pch = 19, col = "red"
    )
    points(
      .[is_PC1_outlier & (PC1_residual < 0), c("lon", "lat")],
      pch = 19, col = "blue"
    )
  }
  par(op)
  dev.off()
}

# Refit models -----------------------------------------------------------------

# .... PC1 models --------------------------------------------------------------

data3 <- data %>%
  map(filter, !is_PC1_outlier)

# QDS-richness:
m1 <- lm(log10(QDS_richness) ~ PC1,          data3$QDS)
m2 <- lm(log10(QDS_richness) ~ PC1 + region, data3$QDS)
m3 <- lm(log10(QDS_richness) ~ PC1 * region, data3$QDS)
AIC(m1, m2, m3) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# Choose m3 (heterogeneity * region interaction)
summary(m3)

# HDS-richness:
m1 <- lm(log10(HDS_richness) ~ PC1,          data3$HDS)
m2 <- lm(log10(HDS_richness) ~ PC1 + region, data3$HDS)
m3 <- lm(log10(HDS_richness) ~ PC1 * region, data3$HDS)
AIC(m1, m2, m3) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# Choose m2 (heterogeneity + region difference)
summary(m2)

# DS-richness:
m1 <- lm(log10(DS_richness) ~ PC1,          data3$DS)
m2 <- lm(log10(DS_richness) ~ PC1 + region, data3$DS)
m3 <- lm(log10(DS_richness) ~ PC1 * region, data3$DS)
AIC(m1, m2, m3) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# Choose m1 (heterogeneity main effect only)
summary(m1)

# ........ Plot ----------------------------------------------------------------

#map(heterogeneity_PCAs, summary)[2:4]
#>          Proportion of Variance (PC1)
#> $QDS                           0.4244
#> $HDS                           0.3902
#> $DS                            0.4126

m_QDS <- lm(log10(QDS_richness) ~ PC1 * region, data3$QDS)
m_HDS <- lm(log10(HDS_richness) ~ PC1 + region, data3$HDS)
m_DS  <- lm(log10(DS_richness)  ~ PC1,          data3$DS)

PC1_seq <- seq(from = -10, to = 10, by = 0.01)

pdf(
  here("draft-02/manuscript_ver3-4/figures/plot-refit-PC1-models.pdf"),
  width = 8, height = 3
)
par(mfrow = c(1, 3))

par(mar = c(4, 4, 3, 0))
plot(
  QDS_richness ~ PC1, data3$QDS,
  xlab = "PC1 (42.44%)",
  ylab = expression(paste(italic("S"))),
  ylim = c(0, max(data$DS$DS_richness)),
  pch = 21, cex = 1.0,
  bg = ifelse(data3$QDS$region == "GCFR", "black", "white")
)
title(expression(paste("(a) QDS ("*italic("R")^2 == "foo)")), adj = 0)
fit_GCFR <-
  predict.lm(m_QDS, newdata = data.frame(region = "GCFR",  PC1 = PC1_seq))
fit_SWAFR <-
  predict.lm(m_QDS, newdata = data.frame(region = "SWAFR", PC1 = PC1_seq))
lines(PC1_seq, 10^fit_GCFR,  col = "black",  lwd = 3)
lines(PC1_seq, 10^fit_SWAFR, col = "grey50", lwd = 3)
legend(
  x = -8, y = 4200,
  legend = unique(data3$QDS$region),
  pch = 21, pt.bg = c("black", "white"),
  box.col = NA
)

par(mar = c(4, 2, 3, 2))
plot(
  HDS_richness ~ PC1, data3$HDS,
  xlab = "PC1 (39.02%)",
  yaxt = "n",
  ylab = "",
  ylim = c(0, max(data$DS$DS_richness)),
  pch = 21, cex = 1.25,
  bg = ifelse(data3$HDS$region == "GCFR", "black", "white")
)
title(expression(paste("(b) HDS ("*italic("R")^2 == "foo)")), adj = 0)
fit_GCFR <-
  predict.lm(m_HDS, newdata = data.frame(region = "GCFR",  PC1 = PC1_seq))
fit_SWAFR <-
  predict.lm(m_HDS, newdata = data.frame(region = "SWAFR", PC1 = PC1_seq))
lines(PC1_seq, 10^fit_GCFR,  col = "black",  lwd = 3)
lines(PC1_seq, 10^fit_SWAFR, col = "grey50", lwd = 3)

par(mar = c(4, 0, 3, 4))
plot(
  DS_richness ~ PC1, data3$DS,
  xlab = "PC1 (41.26%)",
  yaxt = "n",
  ylab = "",
  ylim = c(0, max(data$DS$DS_richness)),
  pch = 21, cex = 1.5,
  bg = ifelse(data3$DS$region == "GCFR", "black", "white")
)
title(expression(paste("(c) DS ("*italic("R")^2 == "foo)")), adj = 0)
fit <- predict.lm(m_DS, newdata = data.frame(PC1 = PC1_seq))
lines(PC1_seq, 10^fit,  col = "black",  lwd = 3)

par(op)
dev.off()
