plot_PC1_models <- function(dataset,
                            keep_outliers = c(FALSE, TRUE),
                            filename = "plot-PC1-models",
                            ext = c("pdf", "png")) {



  # Make changes to filename and data if needed --------------------------------


  if (keep_outliers) {
    # Pull out outlier points to plot as triangles
    outliers <- map(dataset, filter, is_PC1_outlier)
  } else {
    # Otherwise just don't plot the outliers and make the filename clear
    filename %<>% paste0("_refit")
  }


  # Initialise plotting environment on disc ------------------------------------

  filename <- here("figures", glue("{filename}.{ext}"))
  if (ext == "pdf") {
    pdf(filename, width = 8, height = 3)
  } else if (ext == "png") {
    png(filename, width = 8, height = 3, units = "in", res = 600)
  }

  par(mfrow = c(1, 3))

  # Define axis limits
  my_ylim <- c(0, max(dataset$DS$DS_richness))
  my_xlims <- map(dataset, ~range(.$PC1))

  # Remove outliers from data used to plot dots (not triangles)
  dataset %<>% map(filter, !is_PC1_outlier)

  # Plot panel (a) (QDS) -------------------------------------------------------

  par(mar = c(4, 4, 3, 0))

  plot(
    QDS_richness ~ PC1, dataset$QDS,
    xlab = "PC1 (XX.XX%)",
    xlim = my_xlims$QDS,
    ylab = expression(paste(italic("S"))),
    ylim = c(0, max(dataset$DS$DS_richness)),
    pch  = 21, cex = 1.0,
    bg   = ifelse(dataset$QDS$region == "GCFR", "black", "white")
  )

  if (keep_outliers) {
    points(
      outliers$QDS$PC1, outliers$QDS$QDS_richness,
      pch = 24, cex = 1.0,
      bg  = ifelse(outliers$QDS$region == "GCFR", "black", "white")
    )
  }

  title(adj = 0, expression(paste(
    "(a) QDS ("*italic("R")^2 == "0.XX)"
  )))

  #fit_GCFR <- predict(
  #  m_QDS,
  #  newdata = data.frame(region = "GCFR", PC1 = PC1_seq)
  #)
  #fit_SWAFR <- predict.lm(
  #  m_QDS,
  #  newdata = data.frame(region = "SWAFR", PC1 = PC1_seq)
  #)
  #lines(PC1_seq, fit_GCFR,  col = "black",  lwd = 3)
  #lines(PC1_seq, fit_SWAFR, col = "grey50", lwd = 3)

  legend(
    x = -6, y = 3800,
    legend  = unique(dataset$QDS$region),
    pch     = 21,
    pt.bg   = c("black", "white"),
    box.col = NA
  )

  # Plot panel (b) (HDS) -------------------------------------------------------

  par(mar = c(4, 2, 3, 2))

  plot(
    HDS_richness ~ PC1, dataset$HDS,
    xlab = "PC1 (XX.XX%)",
    xlim = my_xlims$HDS,
    yaxt = "n",
    ylab = "",
    ylim = c(0, max(dataset$DS$DS_richness)),
    pch  = 21, cex = 1.25,
    bg   = ifelse(dataset$HDS$region == "GCFR", "black", "white")
  )

  if (keep_outliers) {
    points(
      outliers$HDS$PC1, outliers$HDS$HDS_richness,
      pch = 24, cex = 1.25,
      bg  = ifelse(outliers$HDS$region == "GCFR", "black", "white")
    )
  }

  title(adj = 0, expression(paste(
    "(b) HDS ("*italic("R")^2 == "0.XX)"
  )))

  #fit_GCFR <- predict(
  #  m_HDS,
  #  newdata = data.frame(region = "GCFR",  PC1 = PC1_seq)
  #)
  #fit_SWAFR <- predict.lm(
  #  m_HDS,
  #  newdata = data.frame(region = "SWAFR", PC1 = PC1_seq)
  #)
  #lines(PC1_seq, fit_GCFR,  col = "black",  lwd = 3)
  #lines(PC1_seq, fit_SWAFR, col = "grey50", lwd = 3)

  # Plot panel (c) (DS) --------------------------------------------------------

  par(mar = c(4, 0, 3, 4))

  plot(
    DS_richness ~ PC1, dataset$DS,
    xlab = "PC1 (XX.XX%)",
    xlim = my_xlims$DS,
    yaxt = "n",
    ylab = "",
    ylim = c(0, max(dataset$DS$DS_richness)),
    pch  = 21, cex = 1.5,
    bg   = ifelse(dataset$DS$region == "GCFR", "black", "white")
  )

  if (keep_outliers) {
    points(
      outliers$DS$PC1, outliers$HDS$DS_richness,
      pch = 24, cex = 1.5,
      bg  = ifelse(outliers$DS$region == "GCFR", "black", "white")
    )
  }

  title(adj = 0, expression(paste(
    "(c) DS ("*italic("R")^2 == "0.XX)"
  )))

  #fit <- predict.lm(m_DS, newdata = data.frame(PC1 = PC1_seq))
  #lines(PC1_seq, fit,  col = "black",  lwd = 3)

  # Close & reset plotting environment -----------------------------------------

  dev.off()
  par(op)
}

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

plot_PC1_models(data, keep_outliers = FALSE, ext = "pdf")
plot_PC1_models(data, keep_outliers = FALSE, ext = "png")
plot_PC1_models(data, keep_outliers = TRUE,  ext = "pdf")
plot_PC1_models(data, keep_outliers = TRUE,  ext = "png")

# Import data ------------------------------------------------------------------


# Identify outliers ------------------------------------------------------------

data %<>% map(~ mutate(.x,
  is_PC1_outlier = as_vector(scale(PC1_residual)          > 2),
  is_MV_outlier  = as_vector(scale(multivariate_residual) > 2)
))

#map(heterogeneity_PCAs, summary)[2:4]
#>          Proportion of Variance (PC1)
#> $QDS                           0.4244
#> $HDS                           0.3902
#> $DS                            0.4126

m_QDS <- lm(QDS_richness ~ PC1 * region, data$QDS)
#visreg::visreg(m_QDS, xvar = "PC1", by = "region", overlay = TRUE, gg = TRUE)
glance(m_QDS)$r.squared

m_HDS <- lm(HDS_richness ~ PC1, data$HDS)
glance(m_HDS)$r.squared

m_DS <- lm(DS_richness ~ PC1, data$DS)
glance(m_DS)$r.squared

master_ylim <- c(0, max(data$DS$DS_richness))

QDS_plot <- ggplot(data$QDS, aes(PC1, QDS_richness, fill = region)) +
  geom_point(aes(shape = is_PC1_outlier), size = 1.25) +
  geom_smooth(aes(colour = region), method = lm, se = FALSE) +
  labs(
    x     = "PC1 (42.44%)",
    y     = bquote(italic("S")),
    title = bquote("(a)  QDS ("*italic("R")^{"2"}*" = 0.13)")
  ) +
  scale_fill_manual(values = c("black", "white")) +
  scale_colour_manual(values = c("black", "grey50")) +
  scale_shape_manual(values = c(21, 24)) +
  coord_cartesian(ylim = master_ylim) +
  theme(
    legend.position = "none",
    axis.text.y     = element_text(angle = 90, hjust = 0.5)
  )

HDS_plot <- ggplot(data$HDS, aes(PC1, HDS_richness)) +
  geom_point(aes(fill = region, shape = is_PC1_outlier), size = 1.5) +
  geom_smooth(method = lm, colour = "black", se = FALSE) +
  labs(
    x     = "PC1 (39.02%)",
    title = bquote("(b)  HDS ("*italic("R")^{"2"}*" = 0.22)")
  ) +
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21, 24)) +
  coord_cartesian(ylim = master_ylim) +
  theme(
    legend.position = "none",
    axis.ticks.y    = element_blank(),
    axis.text.y     = element_blank(),
    axis.title.y    = element_blank()
  )

DS_plot <- ggplot(data$DS, aes(PC1, DS_richness)) +
  geom_point(aes(fill = region, shape = is_PC1_outlier), size = 2.0) +
  geom_smooth(method = lm, colour = "black", se = FALSE) +
  labs(
    x      = "PC1 (41.26%)",
    title  = bquote("(c)  DS ("*italic("R")^{"2"}*" = 0.49)")
  ) +
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21, 24), labels = c("\n\nHotspots", "")) +
  coord_cartesian(ylim = master_ylim) +
  guides(
    fill = guide_legend(
      title = "Region", order = 1,
      override.aes = list(shape = c(21, 21), fill = c("black", "white"))
    ),
    shape = guide_legend(
      title = NULL, order = 2,
      override.aes = list(shape = c(24, 24), fill = c("black", "white"))
    )
  ) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank()
  )

my_legend <- get_legend(DS_plot)
DS_plot <- DS_plot + theme(legend.position = "none")

all_scales_plots <- plot_grid(
  QDS_plot, HDS_plot, DS_plot, my_legend,
  nrow = 1, rel_widths = c(1, 0.9, 0.9, 0.3)
)

# Save to disc
ggsave(
  here("figures/plot-PC1-models.pdf"),
  all_scales_plots,
  width = 9, height = 3
)
ggsave(
  here("figures/plot-PC1-models.png"),
  all_scales_plots, dpi = 600,
  width = 9, height = 3
)

# Plot PC1-based models (refit) ------------------------------------------------

data3 <- data %>%
  map(filter, ...)

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
