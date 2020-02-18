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
  write_csv(here("results/list-outlier-squares.csv"))

# Refit models -----------------------------------------------------------------

# .... PC1 models --------------------------------------------------------------

data3 <- data %>%
  map(filter, !is_PC1_outlier)

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
m_DS  <- lm(DS_richness  ~ PC1,          data3$DS)

PC1_seq <- seq(from = -10, to = 10, by = 0.01)

plot_refit_PC1_models <- function(ext = c("pdf", "png")) {
  if (ext == "pdf") {
    pdf(
      here("figures/plot-refit-PC1-models.pdf"),
      width = 8, height = 3
    )
  } else if (ext == "png") {
    png(
      here("figures/plot-refit-PC1-models.png"),
      width = 8, height = 3, units = "in",
      res = 600
    )
  }
  par(mfrow = c(1, 3))

  par(mar = c(4, 4, 3, 0))
  plot(
    QDS_richness ~ PC1, data3$QDS,
    xlab = "PC1 (42.44%)",
    ylab = expression(paste(italic("S"))),
    ylim = c(0, max(data3$DS$DS_richness)),
    pch  = 21, cex = 1.0,
    bg   = ifelse(data3$QDS$region == "GCFR", "black", "white")
  )

  glance(m_QDS)$r.squared
  ## [1] 0.151429
  title(adj = 0, expression(paste(
    "(a) QDS ("*italic("R")^2 == "0.15)"
  )))
  fit_GCFR <- predict(
    m_QDS,
    newdata = data.frame(region = "GCFR", PC1 = PC1_seq)
  )
  fit_SWAFR <- predict.lm(
    m_QDS,
    newdata = data.frame(region = "SWAFR", PC1 = PC1_seq)
  )
  lines(PC1_seq, fit_GCFR,  col = "black",  lwd = 3)
  lines(PC1_seq, fit_SWAFR, col = "grey50", lwd = 3)
  legend(
    x = -8, y = 2800,
    legend  = unique(data3$QDS$region),
    pch     = 21,
    pt.bg   = c("black", "white"),
    box.col = NA
  )

  par(mar = c(4, 2, 3, 2))
  plot(
    HDS_richness ~ PC1, data3$HDS,
    xlab = "PC1 (39.02%)",
    yaxt = "n",
    ylab = "",
    ylim = c(0, max(data3$DS$DS_richness)),
    pch  = 21, cex = 1.25,
    bg   = ifelse(data3$HDS$region == "GCFR", "black", "white")
  )

  glance(m_HDS)$r.squared
  ## [1] 0.242504
  title(adj = 0, expression(paste(
    "(b) HDS ("*italic("R")^2 == "0.24)"
  )))
  fit_GCFR <- predict(
    m_HDS,
    newdata = data.frame(region = "GCFR",  PC1 = PC1_seq)
  )
  fit_SWAFR <- predict.lm(
    m_HDS,
    newdata = data.frame(region = "SWAFR", PC1 = PC1_seq)
  )
  lines(PC1_seq, fit_GCFR,  col = "black",  lwd = 3)
  lines(PC1_seq, fit_SWAFR, col = "grey50", lwd = 3)

  par(mar = c(4, 0, 3, 4))
  plot(
    DS_richness ~ PC1, data3$DS,
    xlab = "PC1 (41.26%)",
    yaxt = "n",
    ylab = "",
    ylim = c(0, max(data3$DS$DS_richness)),
    pch  = 21, cex = 1.5,
    bg   = ifelse(data3$DS$region == "GCFR", "black", "white")
  )

  glance(m_DS)$r.squared
  ## [1] 0.2832038
  title(adj = 0, expression(paste(
    "(c) DS ("*italic("R")^2 == "0.28)"
  )))
  fit <- predict.lm(m_DS, newdata = data.frame(PC1 = PC1_seq))
  lines(PC1_seq, fit,  col = "black",  lwd = 3)

  par(op)
  dev.off()
}

plot_refit_PC1_models("pdf")
plot_refit_PC1_models("png")

# .... Multivariate models -----------------------------------------------------

data2 <- data %>%
  map(filter, !is_MV_outlier)

# Fit multivariate models
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
  write_csv(here("results/refit-model-summary-for-Tony.csv"))

models_R2adjs <- models_summary %>%
  group_by(response) %>%
  summarise(adj.r.squared = adj.r.squared %>%
    unique() %>%
    round(digits = 2)
  )
models_R2adjs

# ........ Plot ----------------------------------------------------------------

models_summary_for_plot <- models_summary %>%
  mutate(
    response = case_when(
      response == "QDS_richness" ~ "(a)~~QDS~(italic(R)[adj]^2=='0.30')",
      response == "HDS_richness" ~ "(b)~~HDS~(italic(R)[adj]^2=='0.36')",
      response == "DS_richness"  ~ "(c)~~DS~(italic(R)[adj]^2=='0.74')"
    ),
    region =
      case_when(
        str_detect(term, "regionSWAFR") ~ "SWAFR",
        TRUE                            ~ "Main effect"
      ) %>%
      factor(levels = c("Main effect", "SWAFR")),
    term = term %>%
      str_replace_all("_", "~") %>%
      str_remove_all(":regionSWAFR") %>%
      str_replace_all("regionSWAFR", "SWAFR") %>%
      str_replace("^SWAFR$", "Region[SWAFR]") %>%
      factor(levels = c(rev(str_replace(var_names, " ", "~")), "Region[SWAFR]")),
    sig = ifelse(p.value < 0.05, "< 0.05", "NS")
  ) %>%
  group_by(response, term) %>%
  mutate(term_type = case_when(
    #(n() == 2) & (region == "SWAFR")                     ~ "SWAFR vs GCFR",
    #(n() == 2) & (region == "Main effect")               ~ "GCFR",
    #(n() == 1) & (region %in% c("Main effect", "SWAFR")) ~ "Main effect only"
    (region == "SWAFR")       ~ "SWAFR vs GCFR",
    (n() == 2) & (region == "Main effect") ~ "GCFR",
    (n() == 1) & (region == "Main effect") ~ "Main effect only"
  ))
models_summary_for_plot$term_type %<>% factor(levels = c(
  "Main effect only",
  "GCFR",
  "SWAFR vs GCFR"
))

model_summary_plot <- ggplot(models_summary_for_plot) +
  aes(
    estimate, term, estimate,
    fill = term_type, group = term_type, shape = term_type,
    alpha = sig
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey75") +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0
  ) +
  geom_point(size = 2) +
  labs(
    x = bquote("Partial effect"~~"("*italic("S")*")"),
    y = "Predictor"
  ) +
  scale_y_discrete(labels = parse(text = levels(models_summary_for_plot$term))) +
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
      title = "Effect type", order = 1,
      override.aes = list(fill = c(NA, "black", "white"))
    ),
    alpha = guide_legend(
      title = bquote(italic("P")["Effect"]),
      override.aes = list(alpha = c(1, 0.25), linetype = NA)
    )
  ) +
  theme(
    strip.text.x = element_text(angle =  0, hjust = 0),
    axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

model_summary_plot

# Save to disc
ggsave(
  here("figures/plot-refit-multivariate-models.pdf"),
  model_summary_plot,
  width = 7, height = 4
)
ggsave(
  here("figures/plot-refit-multivariate-models.png"),
  model_summary_plot, dpi = 600,
  width = 7, height = 4
)

# ........ Compare variation of residuals before & after outlier removal -------

data$QDS$PC1_residual2 <- NA
data$HDS$PC1_residual2 <- NA
data$DS$PC1_residual2  <- NA

data$QDS$PC1_residual2[!data$QDS$is_PC1_outlier] <- residuals(m_QDS)
data$HDS$PC1_residual2[!data$HDS$is_PC1_outlier] <- residuals(m_HDS)
data$DS$PC1_residual2[!data$DS$is_PC1_outlier]   <- residuals(m_DS)

data$QDS$multivariate_residual2 <- NA
data$HDS$multivariate_residual2 <- NA
data$DS$multivariate_residual2  <- NA

data$QDS$multivariate_residual2[!data$QDS$is_MV_outlier] <-
  residuals(m_QDS_richness)
data$HDS$multivariate_residual2[!data$HDS$is_MV_outlier] <-
  residuals(m_HDS_richness)
data$DS$multivariate_residual2[!data$DS$is_MV_outlier] <-
  residuals(m_DS_richness)

data %>%
  map(dplyr::select,
    region,
    PC1_residual,  multivariate_residual,
    PC1_residual2, multivariate_residual2
  ) %>%
  bind_rows(.id = "scale") %>%
  group_by(scale, region) %>%
  summarise_if(is.numeric, sd, na.rm = TRUE) %>%
  write_csv(here("sd-of-residuals-w-and-wo-outliers.csv"))

data %$% {
  QDS %$% {
    print(round(var.test(PC1_residual           ~ region)$p.value, digits = 3))
    print(round(var.test(PC1_residual2          ~ region)$p.value, digits = 3))
    print(round(var.test(multivariate_residual  ~ region)$p.value, digits = 3))
    print(round(var.test(multivariate_residual2 ~ region)$p.value, digits = 3))
  }
  HDS %$% {
    print(round(var.test(PC1_residual           ~ region)$p.value, digits = 3))
    print(round(var.test(PC1_residual2          ~ region)$p.value, digits = 3))
    print(round(var.test(multivariate_residual  ~ region)$p.value, digits = 3))
    print(round(var.test(multivariate_residual2 ~ region)$p.value, digits = 3))
  }
  DS %$% {
    print(round(var.test(PC1_residual           ~ region)$p.value, digits = 3))
    print(round(var.test(PC1_residual2          ~ region)$p.value, digits = 3))
    print(round(var.test(multivariate_residual  ~ region)$p.value, digits = 3))
    print(round(var.test(multivariate_residual2 ~ region)$p.value, digits = 3))
  }
}

# ............ Plot ------------------------------------------------------------

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
