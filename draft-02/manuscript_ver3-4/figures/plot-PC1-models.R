# Plot PC1 univariate models

#map(heterogeneity_PCAs, summary)[2:4]
#>          Proportion of Variance (PC1)
#> $QDS                           0.4244
#> $HDS                           0.3902
#> $DS                            0.4126

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

m_QDS <- lm(log10(QDS_richness) ~ PC1 * region, data$QDS)
m_HDS <- lm(log10(HDS_richness) ~ PC1 + region, data$HDS)
m_DS  <- lm(log10(DS_richness)  ~ PC1,          data$DS)

PC1_seq <- seq(from = -10, to = 10, by = 0.01)

pdf(
  here("draft-02/manuscript_ver3-4/figures/plot-PC1-models.pdf"),
  width = 8, height = 3
)
par(mfrow = c(1, 3))

par(mar = c(4, 4, 3, 0))
plot(
  QDS_richness ~ PC1, data$QDS,
  xlab = "PC1 (42.44%)",
  ylab = expression(paste(italic("S"))),
  ylim = c(0, max(data$DS$DS_richness)),
  pch = 21, cex = 1.0,
  bg = ifelse(data$QDS$region == "GCFR", "black", "white")
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
  legend = unique(data$QDS$region),
  pch = 21, pt.bg = c("black", "white"),
  box.col = NA
)

par(mar = c(4, 2, 3, 2))
plot(
  HDS_richness ~ PC1, data$HDS,
  xlab = "PC1 (39.02%)",
  yaxt = "n",
  ylab = "",
  ylim = c(0, max(data$DS$DS_richness)),
  pch = 21, cex = 1.25,
  bg = ifelse(data$HDS$region == "GCFR", "black", "white")
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
  DS_richness ~ PC1, data$DS,
  xlab = "PC1 (41.26%)",
  yaxt = "n",
  ylab = "",
  ylim = c(0, max(data$DS$DS_richness)),
  pch = 21, cex = 1.5,
  bg = ifelse(data$DS$region == "GCFR", "black", "white")
)
title(expression(paste("(c) DS ("*italic("R")^2 == "foo)")), adj = 0)
fit <- predict.lm(m_DS, newdata = data.frame(PC1 = PC1_seq))
lines(PC1_seq, 10^fit,  col = "black",  lwd = 3)

par(op)
dev.off()
