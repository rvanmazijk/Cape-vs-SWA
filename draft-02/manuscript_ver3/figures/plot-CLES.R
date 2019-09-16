# Plot for CLES analysis

# Neaten variable labels to include panel letters
CLES_results %<>% mutate(
  variable = variable %>%
    as.character() %>%
    str_replace_all("_", " "),
  letters = rep(letters[1:10], 4),
  label = glue("({letters}) {variable}")
)

# Create empty panels
empty_plots <- ggplot(CLES_results, aes(scale, CLES_value)) +
  geom_hline(yintercept = 0.5, colour = "grey75", lty = "dashed") +
  facet_wrap(~label, nrow = 2) +
  scale_x_continuous(
    name   = "Scale (º)",
    breaks = c(0.10, 0.25, 0.50, 1.00)
  ) +
  scale_y_continuous(
    name   = bquote(italic("CLES")~~~"(GCFR > SWAFR)")
  ) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 90, vjust = 0.5),
    axis.text.y     = element_text(angle = 90, hjust = 0.5),
    strip.text.x    = element_text(hjust = 0)
  )
# Create dataset without data for NS regressions (above),
# so that geom_smooth() can't plot for those variables
sig_fits <- CLES_model_summaries %>%
  filter(sig != " ") %>%
  pull(variable) %>%
  str_replace_all("_", " ")
CLES_results_sans_NS <- CLES_results %>% mutate(
  CLES_value = ifelse(variable %in% sig_fits, CLES_value, NA)
)
# Add fits to empty plots
CLES_plots <- empty_plots +
  geom_smooth(
    data    = CLES_results_sans_NS,
    mapping = aes(group = label),
    method  = lm,
    se      = FALSE,
    colour  = "grey50"
  ) +
  # Plot full dataset on top of fits for clarity
  geom_point(data = CLES_results, aes(shape = P_U < 0.05), size = 2) +
  scale_shape_manual(values = c(1, 19))

# Save to disc
ggsave(
  here(
    "draft-02/manuscript_ver3/figures",
    "plot-CLES.pdf"
  ),
  CLES_plots,
  width = 7, height = 4
)
ggsave(
  here(
    "draft-02/manuscript_ver3/figures",
    "plot-CLES.png"
  ),
  CLES_plots, dpi = 600,
  width = 7, height = 4
)
