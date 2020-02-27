plot_PC1_models <- function(dataset,
                            keep_outliers = c(TRUE, FALSE),
                            filename      = "plot-PC1-models",
                            ext           = c("pdf", "png")) {

  # Internal functions ---------------------------------------------------------

  choose_cex <- function(scale = c("QDS", "HDS", "DS")) {
    case_when(
      scale == "QDS" ~ 1.00,
      scale == "HDS" ~ 1.25,
      scale == "DS"  ~ 1.50,
    )
  }

  plot_panel <- function(scale = c("QDS", "HDS", "DS"),
                         my_ylab, PC1_explains) {
    response <- glue("{scale}_richness")
    plot(
      dataset[[scale]]$PC1, dataset[[scale]][[response]],
      yaxt = "n",
      xlab = glue("PC1 ({PC1_explains}%)"),
      ylab = my_ylab,
      xlim = my_xlims[[scale]],  # depends on this object existing
      ylim = my_ylim,            # ''
      pch  = 21, cex = choose_cex(scale),
      bg   = ifelse(dataset[[scale]]$region == "GCFR", "black", "white")
    )
  }

  plot_outliers <- function(scale = c("QDS", "HDS", "DS")) {
    response <- glue("{scale}_richness")
    points(
      outliers[[scale]]$PC1, outliers[[scale]][[response]],
      pch = 24, cex = choose_cex(scale),
      bg = ifelse(outliers[[scale]]$region == "GCFR", "black", "white")
    )
  }

  fit_model_dirty <- function(scale = c("QDS", "HDS", "DS")) {
    response <- glue("{scale}_richness")
    predictors <- "PC1"
    if ((scale == "QDS") |
        (scale == "HDS" & !keep_outliers)) {
      predictors %<>% paste("+ region")
    }
    dataset2 <- dataset[[scale]]
    if (!keep_outliers) {
      dataset2 %<>% filter(!is_PC1_outlier)
    }
    lm(glue("{response} ~ {predictors}"), dataset2)
  }

  R2 <- function(label, x) {
    title(adj = 0, bquote(
      .(label)~"("*italic("R")^2 == .(x)*")"
    ))
  }

  # Fit models quickly and dirtily ---------------------------------------------
  # (Before data modifications below)

  m_QDS <- fit_model_dirty("QDS")
  m_HDS <- fit_model_dirty("HDS")
  m_DS  <- fit_model_dirty("DS")

  # For plotting model fits:
  PC1_seq <- seq(from = -7, to = 7, by = 0.1)

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

  # Define axis limits ---------------------------------------------------------

  my_ylim <- c(0, max(dataset$DS$DS_richness))
  my_xlims <- map(dataset, ~range(.$PC1))

  # Remove outliers from data used to plot dots (not triangles) ----------------
  dataset %<>% map(filter, !is_PC1_outlier)

  # Plot panels ----------------------------------------------------------------

  par(mfrow = c(1, 3))

  # .... (a) QDS ---------------------------------------------------------------

  par(mar = c(4, 4, 3, 0))

  # Plot points
  plot_panel(
    scale        = "QDS",
    my_ylab      = expression(paste(italic("S"))),
    PC1_explains = "XX.XX"
  )
  if (keep_outliers) {
    plot_outliers("QDS")
  }
  R2("(a) QDS", glance(m_QDS)$r.squared)

  # Plot model fits
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

  # Add y-axis
  axis(2,
    at     = c( 0,  500,  1000,  1500,  2000,  2500,  3000,  3500,  4000),
    labels = c("0",  "", "1000",   "", "2000",   "", "3000",   "", "4000")
  )

  # Legend
  legend(
    x = -6, y = 3800,
    legend  = unique(dataset$QDS$region),
    pch     = 21,
    pt.bg   = c("black", "white"),
    box.col = NA
  )

  # .... (b) HDS ---------------------------------------------------------------

  par(mar = c(4, 2, 3, 2))

  # Plot points
  plot_panel(
    scale        = "HDS",
    my_ylab      = "",
    PC1_explains = "XX.XX"
  )
  if (keep_outliers) {
    plot_outliers("HDS")
  }
  R2("(b) HDS", glance(m_HDS)$r.squared)

  # Plot model fits
  if (keep_outliers) {
    fit <- predict.lm(m_HDS, newdata = data.frame(PC1 = PC1_seq))
    lines(PC1_seq, fit,  col = "black",  lwd = 3)
  } else {
    fit_GCFR <- predict(
      m_HDS,
      newdata = data.frame(region = "GCFR", PC1 = PC1_seq)
    )
    fit_SWAFR <- predict.lm(
      m_HDS,
      newdata = data.frame(region = "SWAFR", PC1 = PC1_seq)
    )
    lines(PC1_seq, fit_GCFR,  col = "black",  lwd = 3)
    lines(PC1_seq, fit_SWAFR, col = "grey50", lwd = 3)
  }

  # .... (c) DS ----------------------------------------------------------------

  par(mar = c(4, 0, 3, 4))

  # Plot points
  plot_panel(
    scale        = "DS",
    my_ylab      = "",
    PC1_explains = "XX.XX"
  )
  if (keep_outliers) {
    plot_outliers("DS")
  }
  R2("(c) DS", "0.XX")

  # Plot model fits
  fit <- predict.lm(m_DS, newdata = data.frame(PC1 = PC1_seq))
  lines(PC1_seq, fit,  col = "black",  lwd = 3)

  # Close & reset plotting environment -----------------------------------------

  dev.off()
  par(op)

  # Return models for checking -------------------------------------------------

  list(
    m_QDS = m_QDS,
    m_HDS = m_HDS,
    m_DS  = m_DS
  )
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
