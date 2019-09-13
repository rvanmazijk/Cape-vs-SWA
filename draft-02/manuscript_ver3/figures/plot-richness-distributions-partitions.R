# Plot richness distributions & partitions

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

data_for_plot <- data %$%
  rbind(
    QDS %>%
      dplyr::select(region, QDS_richness) %>%
      rename(richness = QDS_richness) %>%
      add_column(scale = "QDS", turnover_prop = NA),
    HDS %>%
      dplyr::select(region, HDS_richness, QDS_turnover_prop) %>%
      rename(richness = HDS_richness, turnover_prop = QDS_turnover_prop) %>%
      add_column(scale = "HDS"),
    DS %>%
      dplyr::select(region, DS_richness, HDS_turnover_prop) %>%
      rename(richness = DS_richness, turnover_prop = HDS_turnover_prop) %>%
      add_column(scale = "DS")
  ) %>%
  as_tibble() %>%
  gather(metric, metric_value, richness, turnover_prop) %>%
  unite(metric_scale, scale, metric) %>%
  na.exclude()

x_axis_labels <- list(
  QDS_richness      = bquote(italic("S")["QDS"]),
  HDS_richness      = bquote(italic("S")["HDS"]),
  HDS_turnover_prop = bquote(italic("T")["QDS"]/bar(italic("S"))["QDS"]),
  DS_richness       = bquote(italic("S")["DS"]),
  DS_turnover_prop  = bquote(italic("T")["HDS"]/bar(italic("S"))["HDS"])
)
hist_plots <- map(unique(data_for_plot$metric_scale),
  ~ data_for_plot %>%
    filter(metric_scale == .x) %>%
    ggplot(aes(metric_value, fill = region)) +
    geom_histogram(
      bins = case_when(
        str_detect(.x, "QDS") ~ 30,
        str_detect(.x, "HDS") ~ 20,
        str_detect(.x, "DS")  ~ 10
      ),
      position = "dodge",
      colour = "black"
    ) +
    scale_fill_manual(name = "Region", values = c("black", "white")) +
    labs(
      x = x_axis_labels[[.x]],
      y = case_when(
        str_detect(.x, "QDS") ~ "No. QDS",
        str_detect(.x, "HDS") ~ "No. HDS",
        str_detect(.x, "DS")  ~ "No. DS"
      )
    ) +
    theme(
      legend.position = c(0.8, 0.8),
      axis.text.y     = element_text(angle = 90, hjust = 0.5)
    )
)
names(hist_plots) <- unique(data_for_plot$metric_scale)

plot_lim <- data %$%
  HDS %$%
  ceiling(max(c(mean_QDS_richness, QDS_turnover))) + 10
S_HDS_background <- plot_lim %>%
  {seq(from = 0, to = ., by = 10)} %>%
  {expand.grid(x = ., y = .)} %>%
  mutate(z = x + y)
S_HDS_background_plot <- ggplot(S_HDS_background) +
  lims(x = c(0, plot_lim), y = c(0, plot_lim)) +
  geom_contour(
    mapping     = aes(x, y, z = z),
    binwidth    = 500,
    colour      = "grey90",
    show.legend = TRUE
  ) +
  geom_abline(
    intercept = 0, slope = 1,
    linetype = "dashed", colour = "grey25"
  ) +
  labs(
    x = bquote(italic("T")["QDS"]),
    y = bquote(bar(italic("S"))["QDS"])
  ) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

partition_plot <- S_HDS_background_plot +
  geom_point(
    data    = data$HDS,
    mapping = aes(QDS_turnover, mean_QDS_richness, fill = region),
    shape   = 21
  ) +
  scale_fill_manual(values = c("black", "white")) +
  theme(legend.position = "none")

# Add labels to S_HDS contours manually
label1500 <- bquote(italic("S")["HDS"] == 1500)
partition_plot <- partition_plot +
  geom_text(
    data = tibble(
      add_turnover      = c( 10,   10,        60,  560, 1060),
      mean_QDS_richness = c(500, 1000,      1450, 1500, 1500),
      HDS_richness      = c(500, 1000, label1500, 2000, 2500)
    ),
    mapping = aes(add_turnover, mean_QDS_richness, label = HDS_richness),
    angle = -45, vjust = -0.5, colour = "grey50", size = 2.5,
    parse = TRUE
  ) +
  # Flip partition plot to get axes to line up across panels (b/o text heights)
  coord_flip()

hist_plots[c("QDS_richness", "HDS_turnover_prop")] %<>% map(
  ~.x + theme(legend.position = "none")
)
# Plot panels
final_plot <- hist_plots %$% plot_grid(
  QDS_richness,   HDS_richness,
  partition_plot, HDS_turnover_prop,
  nrow = 2,
  labels = glue("({letters[1:4]})"),
  label_x = 0.125, label_y = 0.975
)

# Save to disc
ggsave(
  here(
    "draft-02/manuscript_ver3/figures",
    "plot-richness-distributions-partitions.pdf"
  ),
  final_plot,
  width = 7, height = 6
)

# TODO: Plot DS-scale versions for SI
