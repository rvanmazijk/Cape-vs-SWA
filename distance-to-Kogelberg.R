data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)
data2 <- map(data, ~filter(.x, region == "GCFR"))
data2 %<>% map(~mutate(.x, dist_Kogelberg = sqrt((18.5 - lon)^2 + (-34.5 - lat)^2)))

plot_grid(plotlist = map(data2,
  ~ ggplot(.x, aes(lon, lat, colour = dist_Kogelberg)) +
    geom_point(size = 3) +
    scale_colour_viridis_c()
))

ggplot(data2$QDS, aes(dist_Kogelberg, QDS_richness, colour = PC1)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  scale_y_log10() +
  scale_x_log10()
ggplot(data2$HDS, aes(dist_Kogelberg, HDS_richness, colour = PC1)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  scale_y_log10() +
  scale_x_log10()
ggplot(data2$DS, aes(dist_Kogelberg, DS_richness, colour = PC1)) +
  geom_point(size = 3) +
  scale_colour_viridis_c() +
  scale_y_log10() +
  scale_x_log10()

ggplot(data2$QDS, aes(dist_Kogelberg, PC1_residual)) +
  geom_point(size = 3) +
  scale_x_log10()
ggplot(data2$HDS, aes(dist_Kogelberg, PC1_residual)) +
  geom_point(size = 3) +
  scale_x_log10()
ggplot(data2$DS, aes(dist_Kogelberg, PC1_residual)) +
  geom_point(size = 3) +
  scale_x_log10()

m <- lm(PC1_residual ~ log10(dist_Kogelberg), data2$QDS)
summary(m)
visreg::visreg(m)

m <- lm(PC1_residual ~ log10(dist_Kogelberg), data2$HDS)
summary(m)
visreg::visreg(m)

m <- lm(PC1_residual ~ log10(dist_Kogelberg), data2$DS)
summary(m)
visreg::visreg(m)

m <- lm(QDS_richness ~ log10(dist_Kogelberg), data2$QDS)
summary(m)
visreg::visreg(m)

m <- lm(HDS_richness ~ log10(dist_Kogelberg), data2$HDS)
summary(m)
visreg::visreg(m)

m <- lm(DS_richness ~ log10(dist_Kogelberg), data2$DS)
summary(m)
visreg::visreg(m)

m <- lm(QDS_richness ~ PC1 * (dist_Kogelberg), data2$QDS)
summary(m)
visreg::visreg(m, xvar = "dist_Kogelberg")
visreg::visreg(m, xvar = "PC1", by = "dist_Kogelberg", overlay = TRUE)
