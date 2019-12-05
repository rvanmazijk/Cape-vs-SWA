# Import & tidy data -----------------------------------------------------------

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

data_for_plot <- data %$%
  rbind(
    QDS %>%
      dplyr::select(region, QDS_richness, QDS) %>%
      rename(
        richness      = QDS_richness,
        cell_name     = QDS
      ) %>%
      add_column(scale = "QDS", turnover_prop = NA),
    HDS %>%
      dplyr::select(region, HDS_richness, QDS_turnover_prop, HDS) %>%
      rename(
        richness      = HDS_richness,
        turnover_prop = QDS_turnover_prop,
        cell_name     = HDS
      ) %>%
      add_column(scale = "HDS"),
    DS %>%
      dplyr::select(region, DS_richness, HDS_turnover_prop, DS) %>%
      rename(
        richness      = DS_richness,
        turnover_prop = HDS_turnover_prop,
        cell_name     = DS
      ) %>%
      add_column(scale = "DS")
  ) %>%
  as_tibble() %>%
  gather(metric, metric_value, richness, turnover_prop) %>%
  unite(metric_scale, scale, metric) %>%
  na.exclude()

# Distribution plots -----------------------------------------------------------

x_axis_labels <- list(
  QDS_richness      = bquote(italic("S")),#["QDS"]),
  HDS_richness      = bquote(italic("S")),#["HDS"]),
  HDS_turnover_prop = bquote(italic("T")/italic("S")),#["QDS"]; ["HDS"]
  DS_richness       = bquote(italic("S")),#["DS"]),
  DS_turnover_prop  = bquote(italic("T")/italic("S"))#["HDS"]; ["DS"]
)
hist_plots <- map(unique(data_for_plot$metric_scale),
  ~ data_for_plot %>%
    filter(metric_scale == .x) %>%
    ggplot(aes(metric_value, fill = region)) +
      geom_histogram(
        # Scale to frequencies/proportions of cells for each region separately
        aes(y = 2*(..density..)/sum(..density..)),
        bins = 10,
        position = "dodge",
        colour = "black"
      ) +
      scale_fill_manual(name = "Region", values = c("black", "white")) +
      labs(
        x = x_axis_labels[[.x]],
        y = case_when(
          str_detect(.x, "QDS") ~ "Prop. QDS",
          str_detect(.x, "HDS") ~ "Prop. HDS",
          str_detect(.x, "DS")  ~ "Prop. DS"
        )
      ) +
      coord_cartesian(
        xlim = case_when(
          str_detect(.x, "richness") ~ c(0, 5000),
          str_detect(.x, "turnover") ~ c(0,    1)
        ),
        ylim = c(0, 0.6)
      ) +
      theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
)
names(hist_plots) <- unique(data_for_plot$metric_scale)

my_legend <- get_legend(hist_plots$QDS_richness)

hist_plots %<>% map(~ .x + theme(legend.position = "none"))

hist_plots[c("DS_turnover_prop", "HDS_turnover_prop")] %<>% map(
  ~ .x +
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.title.y = element_blank()
    )
)

hist_plots[c("DS_richness", "DS_turnover_prop", "HDS_richness")] %<>% map(
  ~ .x +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.title.x = element_blank()
    )
)

# Partition plots --------------------------------------------------------------

DS_plot_lim <- data %$%
  DS %$%
  ceiling(max(c(mean_HDS_richness, HDS_turnover))) + 10

S_HDS_background <- DS_plot_lim %>%
  {seq(from = 0, to = ., by = 10)} %>%
  {expand.grid(x = ., y = .)} %>%
  mutate(z = x + y)
S_HDS_background_plot <- ggplot(S_HDS_background) +
  lims(x = c(0, DS_plot_lim), y = c(0, DS_plot_lim)) +
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
HDS_partition_plot <- S_HDS_background_plot +
  geom_point(
    data    = data$HDS,
    mapping = aes(QDS_turnover, mean_QDS_richness, fill = region),
    shape   = 21
  ) +
  scale_fill_manual(values = c("black", "white")) +
  theme(legend.position = "none")

S_DS_background <- DS_plot_lim %>%
  {seq(from = 0, to = ., by = 10)} %>%
  {expand.grid(x = ., y = .)} %>%
  mutate(z = x + y)
S_DS_background_plot <- ggplot(S_DS_background) +
  lims(x = c(0, DS_plot_lim), y = c(0, DS_plot_lim)) +
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
    x = bquote(italic("T")["HDS"]),
    y = bquote(bar(italic("S"))["HDS"])
  ) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
DS_partition_plot <- S_DS_background_plot +
  geom_point(
    data    = data$DS,
    mapping = aes(HDS_turnover, mean_HDS_richness, fill = region),
    shape   = 21
  ) +
  scale_fill_manual(values = c("black", "white")) +
  theme(legend.position = "none")

label2000 <- bquote(italic("S")["HDS"] == 2000)
HDS_partition_plot <- HDS_partition_plot +
  geom_text(
    data = tibble(
      mean_QDS_richness = c(500, 1000, 1500,      1950, 2100, 2100, 2100), # "x"
      add_turnover      = c( 10,   10,   10,        60,  560, 1060, 1560), # "y"
      HDS_richness      = c(500, 1000, 1500, label2000, 2500, 3000, 3500)
    ),
    mapping = aes(add_turnover, mean_QDS_richness, label = HDS_richness),
    angle = -45, vjust = -0.5, colour = "grey50", size = 2.5,
    parse = TRUE
  ) +
  # Flip partition plot to get axes to line up across panels (b/o text heights)
  coord_flip()

label2000 <- bquote(italic("S")["DS"] == 2000)
DS_partition_plot <- DS_partition_plot +
  geom_text(
    data = tibble(
      mean_HDS_richness = c(500, 1000, 1500,      1950, 2100, 2100, 2100), # "x"
      add_turnover      = c( 10,   10,   10,        60,  560, 1060, 1560), # "y"
      DS_richness       = c(500, 1000, 1500, label2000, 2500, 3000, 3500)
    ),
    mapping = aes(add_turnover, mean_HDS_richness, label = DS_richness),
    angle = -45, vjust = -0.5, colour = "grey50", size = 2.5,
    parse = TRUE
  ) +
  coord_flip()

# Plot panels ------------------------------------------------------------------

richness_plots <- hist_plots %$% plot_grid(
  DS_richness,
  HDS_richness,
  QDS_richness,
  nrow = 3,rel_heights = c(0.85, 0.9, 1),
  labels = c("(a)", "(b)", "(c)"), label_fontface = "plain",
  label_x = 0.125, label_y = 0.975
)

turnover_plots <- hist_plots %$% plot_grid(
  DS_turnover_prop,
  HDS_turnover_prop,
  my_legend,
  nrow = 3, rel_heights = c(0.85, 1.05, 0.85),
  labels = c("(d)", "(e)", ""), label_fontface = "plain",
  label_x = 0.025, label_y = 0.975
)

partition_plots <- plot_grid(
  DS_partition_plot + theme(
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank()
  ),
  HDS_partition_plot + ylab(bquote(
    bar(italic("S"))["QDS"]~"or"~bar(italic("S"))["HDS"]
  )),
  white_rect,
  nrow = 3, rel_heights = c(0.85, 1.05, 0.85),
  labels = c("(f)", "(g)", ""), label_fontface = "plain",
  label_x = 0.15, label_y = 0.975
)

final_plot <- plot_grid(
  richness_plots,
  turnover_plots,
  partition_plots,
  nrow = 1, rel_widths = c(1, 0.9, 1)
)

# Save to disc
ggsave(
  here(
    "draft-02/figures",
    "plot-richness-distributions-partitions.pdf"
  ),
  final_plot,
  width = 8, height = 7
)
ggsave(
  here(
    "draft-02/figures",
    "plot-richness-distributions-partitions.png"
  ),
  final_plot, dpi = 600,
  width = 8, height = 7
)
