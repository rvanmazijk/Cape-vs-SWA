# Plot richness distributions & partitions

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

data$QDS %>%
  dplyr::select(region, QDS, QDS_richness) %>%
  group_by(region)


data$HDS %>%
  mutate(DS = str_remove(HDS, "[A-D]$")) %>%
  dplyr::select(region, DS, HDS, HDS_richness) %>%
  full_join(data$DS[, c("DS", "DS_richness")]) %>%
  ggplot(aes(HDS_richness, DS_richness, colour = region)) +
    geom_abline(
      intercept = 0, slope = 1,
      linetype = "dashed", colour = "grey25"
    ) +
    geom_point()

data$QDS %>%
  mutate(HDS = str_remove(QDS, "[A-D]$")) %>%
  dplyr::select(region, HDS, QDS, QDS_richness) %>%
  full_join(data$HDS[, c("HDS", "HDS_richness")]) %>%
  ggplot(aes(QDS_richness, HDS_richness, colour = region)) +
    geom_abline(
      intercept = 0, slope = 1,
      linetype = "dashed", colour = "grey25"
    ) +
    geom_point()

data$QDS %>%
  mutate(HDS = str_remove(QDS, "[A-D]$")) %>%
  dplyr::select(region, HDS, QDS, QDS_richness) %>%
  full_join(data$HDS[, c("HDS", "HDS_richness")]) %>%
  mutate(foo = HDS_richness - QDS_richness) %>%
  group_by(region, HDS) %>%
  summarise(
    HDS_richness = unique(HDS_richness),
    foo = mean(foo, na.rm = TRUE)
  ) %>%
  ggplot(aes(1, foo, colour = region)) +
    geom_boxplot()
