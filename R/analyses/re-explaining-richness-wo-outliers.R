# Plot PC1-based models (refit) ------------------------------------------------

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

# Plot MV-based models (refit) ------------------------------------------------

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
