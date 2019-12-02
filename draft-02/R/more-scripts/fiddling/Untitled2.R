data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

ggplot(data$HDS, aes(PC1, PC1_residual, fill = HDS_richness, shape = region)) +
  geom_point(size = 4, alpha = 0.75) +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  scale_shape_manual(values = c(21, 24))

ggplot(data$HDS, aes(PC1, HDS_richness, fill = PC1_residual, shape = region)) +
  geom_point(size = 4, alpha = 0.75) +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  scale_shape_manual(values = c(21, 24))

ggplot(data$HDS, aes(PC1, HDS_richness, fill = QDS_turnover_prop, shape = region)) +
  geom_point(size = 4, alpha = 0.75) +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  scale_shape_manual(values = c(21, 24))

ggplot(data$DS, aes(PC1, DS_richness, fill = HDS_turnover_prop, shape = region)) +
  geom_point(size = 4, alpha = 0.75) +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  scale_shape_manual(values = c(21, 24))

ggplot(data$HDS, aes(mean_QDS_richness, QDS_turnover, fill = PC1, shape = region)) +
  geom_point(size = 4, alpha = 0.75) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey25") +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  scale_shape_manual(values = c(21, 24))
ggplot(data$HDS, aes(mean_QDS_richness, QDS_turnover, colour = PC1, shape = region)) +
  geom_point(size = 4, alpha = 0.75) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey25") +
  scale_colour_viridis_c(direction = -1, limits = c(-4, 4))

ggplot(data$DS, aes(mean_HDS_richness, HDS_turnover, fill = PC1, shape = region)) +
  geom_point(size = 4, alpha = 0.75) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey25") +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  scale_shape_manual(values = c(21, 24))
ggplot(data$DS, aes(mean_HDS_richness, HDS_turnover, colour = PC1, shape = region)) +
  geom_point(size = 4, alpha = 0.75) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey25") +
  scale_colour_viridis_c(direction = -1, limits = c(-4, 4))

ggplot(data$DS, aes(mean_HDS_richness, HDS_turnover, fill = PC1_residual, shape = region)) +
  geom_point(size = 4, alpha = 0.75) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey25") +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  scale_shape_manual(values = c(21, 24))

ggplot(data$HDS, aes(mean_QDS_richness, QDS_turnover, fill = PC1_residual, shape = region)) +
  geom_point(size = 4, alpha = 0.75) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey25") +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  scale_shape_manual(values = c(21, 24))

ggplot(data$HDS, aes(PC1_residual, QDS_turnover_prop, fill = HDS_richness, shape = region)) +
  geom_point(size = 4, alpha = 0.75) +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  scale_shape_manual(values = c(21, 24))

ggplot(data$DS, aes(PC1_residual, HDS_turnover_prop, fill = DS_richness, shape = region)) +
  geom_point(size = 4, alpha = 0.75) +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  scale_shape_manual(values = c(21, 24))
