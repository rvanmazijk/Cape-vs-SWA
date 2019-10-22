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
  mutate(richness_prop = 1 - turnover_prop) %>%
  gather(metric, metric_value, richness, turnover_prop, richness_prop) %>%
  na.exclude() %>%
  group_by(region, metric, scale) %>%
  summarise(mean_value = mean(metric_value)) %>%
  ungroup() %>%
  mutate(
    scale = case_when(
      scale == "QDS" ~ 0.25,
      scale == "HDS" ~ 0.50,
      scale == "DS"  ~ 1.00
    ),
    y = ifelse(region == "GCFR", 1, -1)
  ) %>%
  rename(spatial_scale = scale) %>%
  spread(metric, mean_value) %>%
  mutate(region_scale = paste(region, spatial_scale)) %>%
  dplyr::select(-region) %>%
  mutate(
    richness_prop = ifelse(is.na(richness_prop), 1, richness_prop),
    turnover_prop = ifelse(is.na(turnover_prop), 0, turnover_prop)
  )
ggplot() +
  geom_scatterpie(
    aes(x = spatial_scale * 1e4, y = y * 2e3, group = region_scale, r = richness),
    data = data_for_plot,
    cols = c("turnover_prop", "richness_prop"), color = NA
  ) +
  coord_equal() +
  geom_scatterpie_legend(data_for_plot$richness, x = -3000, y = 0, n = 5)

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
  na.exclude() %>%
  group_by(region, metric, scale) %>%
  summarise(mean_value = mean(metric_value), sd_value = sd(metric_value)) %>%
  ungroup() %>%
  mutate(scale = case_when(
    scale == "QDS" ~ 0.25,
    scale == "HDS" ~ 0.50,
    scale == "DS"  ~ 1.00
  )) %>%
  add_row(
    region = "GCFR", metric = "turnover_prop", scale = 0.25,
    mean_value = 1, sd_value = 0
  ) %>%
  add_row(
    region = "SWAFR", metric = "turnover_prop", scale = 0.25,
    mean_value = 1, sd_value = 0
  ) %>%
  mutate(
    upp_value = mean_value + sd_value,
    low_value = mean_value - sd_value
  )

data_for_plot <- full_join(
  data_for_plot %>%
    filter(metric == "richness") %>%
    dplyr::select(region, scale, mean_value, low_value, upp_value),
  data_for_plot %>%
    filter(metric == "turnover_prop") %>%
    dplyr::select(region, scale, mean_value, low_value, upp_value) %>%
    rename(
      to_mean_value = mean_value,
      to_low_value  = low_value,
      to_upp_value  = upp_value
    )
)
ggplot(data_for_plot, aes(mean_value, to_mean_value, colour = region, shape = as.factor(scale))) +
  geom_errorbarh(
    aes(xmin = low_value, xmax = upp_value),
    position = position_dodge(width = 0.1),
    height = 0
  ) +
  geom_errorbar(
    aes(ymin = to_low_value, ymax = to_upp_value),
    position = position_dodge(width = 0.1),
    width = 0
  ) +
  geom_point(position = position_dodge(width = 0.1))
ggplot(data_for_plot) +
  geom_col(
    aes(region, mean_value * to_mean_value),
    colour = "black", fill = "grey50"
  ) +
  geom_errorbar(
    aes(region, ymin = mean_value * to_low_value, ymax = mean_value * to_upp_value),
    width = 0,
  ) +
  geom_col(
    aes(region, mean_value),
    colour = "black", fill = NA
  ) +
  geom_errorbar(
    aes(region, ymin = low_value, ymax = upp_value),
    width = 0, linetype = "dashed"
  ) +
  facet_grid(~scale)

ggplot(data$DS, aes(DS_richness, HDS_turnover_prop, colour = region)) +
  geom_point()

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
  na.exclude()
ggplot(data_for_plot, aes(metric_value, fill = region)) +
  geom_histogram(colour = "black", position = "dodge") +
  labs(
    x = bquote(
      italic("S")~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~italic("T")/italic("S")
    ),
    y = paste(
      "No. QDS                                          ",
      "No. HDS                                          ",
      "No. DS"
    )
  ) +
  #geom_vline(aes(fill = metric), xintercept = 0.5, linetype = "dashed") +
  facet_grid(scale ~ metric, scales = "free", labeller = label_parsed) +
  scale_fill_manual(name = "Region", values = c("black", "white")) +
  scale_colour_manual(values = c(NA, "black")) +
  theme(
    strip.text      = element_blank(),
    legend.position = c(0.75, 0.15),
    panel.spacing   = unit(0L, "cm")
  )

data_for_plot <- data %$%
  rbind(
    QDS %>%
      dplyr::select(region, QDS_richness) %>%
      rename(richness = QDS_richness) %>%
      add_column(scale = "QDS", turnover_prop = 1),
    HDS %>%
      dplyr::select(region, HDS_richness, QDS_turnover_prop) %>%
      rename(richness = HDS_richness, turnover_prop = QDS_turnover_prop) %>%
      add_column(scale = "HDS"),
    DS %>%
      dplyr::select(region, DS_richness, HDS_turnover_prop) %>%
      rename(richness = DS_richness, turnover_prop = HDS_turnover_prop) %>%
      add_column(scale = "DS")
  ) %>%
  as_tibble()
ggplot(data_for_plot, aes(turnover_prop, richness, colour = region)) +
  geom_point() +
  facet_grid(scale ~ ., scales = "free_y")
