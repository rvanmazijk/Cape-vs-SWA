# Plot multivariate models

models_R2adjs <- models_summary %>%
  group_by(response) %>%
  summarise(adj.r.squared = adj.r.squared %>%
    unique() %>%
    round(digits = 2)
  )
models_R2adjs

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
        TRUE                            ~ "Main effect"
      ) %>%
      factor(levels = c("Main effect", "SWAFR")),
    term = term %>%
      str_replace_all("_", " ") %>%
      str_remove_all(":regionSWAFR") %>%
      str_replace_all("regionSWAFR", "SWAFR") %>%
      factor(levels = c(rev(var_names), "SWAFR")),
    sig = ifelse(p.value < 0.05, "< 0.05", "NS")
  ) %>%
  group_by(response, term) %>%
  mutate(term_type = case_when(
    (n() == 2) & (region == "SWAFR")                     ~ "SWAFR vs GCFR",
    (n() == 2) & (region == "Main effect")               ~ "GCFR",
    (n() == 1) & (region %in% c("Main effect", "SWAFR")) ~ "Main effect only"
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

# Save to disc
ggsave(
  here(
    "draft-02/manuscript_ver3/figures",
    "plot-multivariate-models.pdf"
  ),
  model_summary_plot,
  width = 7, height = 4
)
ggsave(
  here(
    "draft-02/manuscript_ver3/figures",
    "plot-multivariate-models.png"
  ),
  model_summary_plot, dpi = 600,
  width = 7, height = 4
)
