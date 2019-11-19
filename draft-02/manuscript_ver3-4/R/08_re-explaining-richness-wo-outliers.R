# Import data ------------------------------------------------------------------
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

data2 <- data %>%
  map(filter, !is_MV_outlier)
data3 <- data %>%
  map(filter, !is_PC1_outlier)

# .... PC1 models --------------------------------------------------------------

# QDS-richness:
m1 <- lm(QDS_richness ~ PC1,          data3$QDS)
m2 <- lm(QDS_richness ~ PC1 + region, data3$QDS)
m3 <- lm(QDS_richness ~ PC1 * region, data3$QDS)
AIC(m1, m2, m3) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# Choose m2 (heterogeneity + region difference)
summary(m2)

# HDS-richness:
m1 <- lm(HDS_richness ~ PC1,          data3$HDS)
m2 <- lm(HDS_richness ~ PC1 + region, data3$HDS)
m3 <- lm(HDS_richness ~ PC1 * region, data3$HDS)
AIC(m1, m2, m3) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# Choose m2 (heterogeneity + region difference)
summary(m2)

# DS-richness:
m1 <- lm(DS_richness ~ PC1,          data3$DS)
m2 <- lm(DS_richness ~ PC1 + region, data3$DS)
m3 <- lm(DS_richness ~ PC1 * region, data3$DS)
AIC(m1, m2, m3) %>%
  mutate(delta_AIC  = AIC - min(AIC))
# Choose m1 (heterogeneity main effect only)
summary(m2)

# ........ Plot ----------------------------------------------------------------

#map(heterogeneity_PCAs, summary)[2:4]
#>          Proportion of Variance (PC1)
#> $QDS                           0.4244
#> $HDS                           0.3902
#> $DS                            0.4126

m_QDS <- lm(QDS_richness ~ PC1 + region, data3$QDS)
m_HDS <- lm(HDS_richness ~ PC1 + region, data3$HDS)
m_DS  <- lm(DS_richness ~ PC1,           data3$DS)

m_plots <- list(
  QDS = m_QDS %>%
    visreg::visreg(
      xvar = "PC1", by = "region",
      overlay = TRUE, gg = TRUE, points = list(alpha = 0.25)
    ) +
    ggtitle(bquote(
      "(a)  QDS"~
      "("*italic("R")^"2" == .(round(glance(m_QDS)$r.squared, digits = 2))*")"
    )),
  HDS = m_HDS %>%
    visreg::visreg(
      xvar = "PC1", by = "region",
      overlay = TRUE, gg = TRUE, points = list(alpha = 0.25)
    ) +
    ggtitle(bquote(
      "(b)  HDS"~
      "("*italic("R")^"2" == .(round(glance(m_HDS)$r.squared, digits = 2))*")"
    )),
  DS = m_DS %>%
    visreg::visreg(
      xvar = "PC1",
      gg = TRUE
    ) +
    ggtitle(bquote(
      "(c)  DS"~
      "("*italic("R")^"2" == .(round(glance(m_DS)$r.squared, digits = 2))*")"
    ))
)
m_plots %<>% map(~ . +
  ylab(bquote(italic("S"))) +
  ylim(0, max(data2$DS$DS_richness)) +
  theme(
    axis.text.y     = element_text(angle = 90, hjust = 0.5),
    legend.position = "none"
  )
)
m_plots[2:3] %<>% map(~ . + theme(
  axis.title.y = element_blank(),
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank()
))
m_plots2 <- plot_grid(plotlist = m_plots, nrow = 1, rel_widths = c(1, 0.9, 0.9))

# Save to disc
ggsave(
  here(
    "draft-02/manuscript_ver3-4/figures",
    "plot-refit-PC1-models.pdf"
  ),
  m_plots2,
  width = 9, height = 3
)

summary(m_QDS)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  250.462     13.520  18.525  < 2e-16 ***
## PC1           55.177      4.486  12.300  < 2e-16 ***
## regionSWAFR  101.188     17.804   5.684 1.81e-08 ***
summary(m_HDS)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   629.30      50.06  12.570  < 2e-16 ***
## PC1           145.82      17.24   8.459 3.33e-15 ***
## regionSWAFR   219.28      63.46   3.455 0.000656 ***
summary(m_DS)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  1541.36      78.98  19.517  < 2e-16 ***
## PC1           213.23      44.93   4.746 1.44e-05 ***

# ........ Compare variation of residuals before & after outlier removal -------

data %$% {
  par(mfrow = c(3, 2))
  QDS %$% {
    hist(
      PC1_residual,
      main = bquote(
        italic("SD") ==
        .(round(sd(PC1_residual),     digits = 2))
      ),
      xlab = bquote("Res."~italic("S")["QDS"])
    )
    hist(
      residuals(m_QDS),
      main = bquote(
        italic("SD") ==
        .(round(sd(residuals(m_QDS)), digits = 2))
      ),
      xlab = bquote("Res."~italic("S")["QDS"]~"(sans outliers)")
    )
  }
  HDS %$% {
    hist(
      PC1_residual,
      main = bquote(
        italic("SD") ==
        .(round(sd(PC1_residual),     digits = 2))
      ),
      xlab = bquote("Res."~italic("S")["HDS"])
    )
    hist(
      residuals(m_HDS),
      main = bquote(
        italic("SD") ==
        .(round(sd(residuals(m_HDS)), digits = 2))
      ),
      xlab = bquote("Res."~italic("S")["HDS"]~"(sans outliers)")
    )
  }
  DS %$% {
    hist(
      PC1_residual,
      main = bquote(
        italic("SD") ==
        .(round(sd(PC1_residual),     digits = 2))
      ),
      xlab = bquote("Res."~italic("S")["DS"])
    )
    hist(
      residuals(m_DS),
      main = bquote(
        italic("SD") ==
        .(round(sd(residuals(m_DS)),  digits = 2))
      ),
      xlab = bquote("Res."~italic("S")["DS"]~"(sans outliers)")
    )
  }
  par(op)
}

# .... Multivariate models -----------------------------------------------------

predictor_names <- c(str_replace_all(var_names, " ", "_"), "PC1")

full_formula <- predictor_names[predictor_names != "PC1"] %>%
  {c(., paste(., "* region"))} %>%
  paste(collapse = " + ")

m_QDS_richness <- lm(glue("QDS_richness ~ {full_formula}"), data2$QDS)
m_HDS_richness <- lm(glue("HDS_richness ~ {full_formula}"), data2$HDS)
m_DS_richness  <- lm(glue("DS_richness  ~ {full_formula}"), data2$DS)

m_QDS_richness %<>% step(direction = "backward", trace = 0)
m_HDS_richness %<>% step(direction = "backward", trace = 0)
m_DS_richness  %<>% step(direction = "backward", trace = 0)

reparameterise <- function(m) {
  response <- colnames(m$model)[[1]]
  dataset <- data2 %$% {
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

models_R2adjs <- models_summary %>%
  group_by(response) %>%
  summarise(adj.r.squared = adj.r.squared %>%
    unique() %>%
    round(digits = 2)
  )

# ........ Plot ----------------------------------------------------------------

models_summary_for_plot <- models_summary %>%
  mutate(
    response = case_when(
      response == "QDS_richness" ~ "(a)~~QDS~(italic(R)[adj]^2=='0.24')",
      response == "HDS_richness" ~ "(b)~~HDS~(italic(R)[adj]^2=='0.33')",
      response == "DS_richness"  ~ "(c)~~DS~(italic(R)[adj]^2=='0.61')"
    ),
    region =
      case_when(
        str_detect(term, "regionSWAFR") ~ "SWAFR",
        str_detect(term, "regionGCFR")  ~ "GCFR",
        TRUE                            ~ "Main effect only"
      ) %>%
      factor(levels = c("Main effect only", "GCFR", "SWAFR")),
    term = term %>%
      str_replace_all("_", " ") %>%
      str_remove_all("regionSWAFR:") %>%
      str_remove_all("regionGCFR:") %>%
      str_replace_all("regionSWAFR", "SWAFR") %>%
      factor(levels = c(rev(var_names), "SWAFR")),
    sig = ifelse(p.value < 0.05, "< 0.05", "NS")
  )

model_summary_plot <- ggplot(models_summary_for_plot) +
  aes(
    estimate, term, estimate,
    fill = region, group = region, shape = region,
    alpha = sig
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey75") +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.1
  ) +
  geom_point(size = 2) +
  labs(
    x = bquote("Effect"~~"("*italic("S")*")"),
    y = "Heterogeneity predictor"
  ) +
  scale_fill_manual(values = c(NA, "black", "white")) +
  scale_shape_manual(values = c(4, 21, 21)) +
  scale_alpha_manual(values = c(1, 0.25)) +
  facet_wrap(
    response ~ .,
    nrow     = 1,
    scales   = "free_x",
    labeller = label_parsed
  ) +
  guides(
    fill = FALSE,
    shape = guide_legend(
      title = "Effect type",
      override.aes = list(fill = c(NA, "black", "white"))
    ),
    alpha = guide_legend(
      title = bquote(italic("P")["Effect"]),
      override.aes = list(alpha = c(1, 0.25), linetype = NA)
    )
  ) +
  theme(strip.text.x = element_text(angle =  0, hjust = 0))

model_summary_plot

# Save to disc
ggsave(
  here(
    "draft-02/manuscript_ver3-4/figures",
    "plot-refit-multivariate-models.pdf"
  ),
  model_summary_plot,
  width = 7, height = 4
)

# Junk -------------------------------------------------------------------------

ggplot(data$QDS, aes(PC1_residual, fill = region)) +
  geom_histogram(position = "dodge", colour = "black") +
  labs(x = bquote("Residual"~italic("S")["QDS"]~"(PC1)"), y = "No. QDS") +
  scale_fill_manual(name = "Region", values = c("black", "white"))
ggplot(data$QDS, aes(multivariate_residual, fill = region)) +
  geom_histogram(position = "dodge", colour = "black") +
  labs(x = bquote("Residual"~italic("S")["QDS"]~"(PC1)"), y = "No. QDS") +
  scale_fill_manual(name = "Region", values = c("black", "white"))

#...
# %>%
#  ggplot(aes(region, PC1_residual)) +
#    geom_jitter(aes(colour = is_PC1_outlier)) +
#    geom_boxplot(fill = NA, outlier.colour = NA)
