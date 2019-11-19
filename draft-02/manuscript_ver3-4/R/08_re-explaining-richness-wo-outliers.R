# Import data ------------------------------------------------------------------
data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

# Identify outliers ------------------------------------------------------------

data %<>% map(~ mutate(.x,
  is_PC1_outlier = as_vector(scale(PC1_residual)          > 1.96),
  is_MV_outlier  = as_vector(scale(multivariate_residual) > 1.96)
))

# Plots ------------------------------------------------------------------------

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

# Refit models -----------------------------------------------------------------

predictor_names <- c(str_replace_all(var_names, " ", "_"), "PC1")

# Fit multivariate models
full_formula <- predictor_names[predictor_names != "PC1"] %>%
  {c(., paste(., "* region"))} %>%
  paste(collapse = " + ")

data2 <- data %>%
  map(filter, !is_MV_outlier)

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
