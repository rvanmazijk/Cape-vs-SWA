# HDS richness, mean QDS richness and turnover surface figures
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
library(lattice)

source(here::here("data/01_import-region-polygons.R"))
source(here::here("data/02_import-floral-data.R"))

GCFR_spp <- readOGR(here::here(
  "outputs/species-turnover-and-richness/GCFR_spp_2018-08-10"
))
SWAFR_spp <- readOGR(here::here(
  "outputs/species-turnover-and-richness/SWAFR_spp_2018-08-10"
))
names(GCFR_spp)[6:8] <-
  c("HDS_richness", "mean_QDS_richness", "mean_QDS_turnover")
names(SWAFR_spp)[6:8] <-
  c("HDS_richness", "mean_QDS_richness", "mean_QDS_turnover")
GCFR_spp_data <- GCFR_spp@data %>%
  select(hdgc, HDS_richness, mean_QDS_richness, mean_QDS_turnover) %>%
  distinct()
SWAFR_spp_data <- SWAFR_spp@data %>%
  select(hdgc, HDS_richness, mean_QDS_richness, mean_QDS_turnover) %>%
  distinct()
richness_turnover_data <- as_tibble(rbind(
  cbind(region = "Cape", GCFR_spp_data),
  cbind(region = "SWA", SWAFR_spp_data)
))
richness_turnover_data %<>% mutate(
  add_residual_turnover = HDS_richness - mean_QDS_richness,
  add_mean_QDS_richness_prop = mean_QDS_richness / HDS_richness,
  add_residual_turnover_prop = add_residual_turnover / HDS_richness,
  prod_residual_turnover = HDS_richness / mean_QDS_richness
)

# Fit models -------------------------------------------------------------------

Cape_m3 <- lm(
  HDS_richness ~ mean_QDS_richness * mean_QDS_turnover,
  data = na.exclude(filter(richness_turnover_data, region == "Cape"))
)
SWA_m3 <- lm(
  HDS_richness ~ mean_QDS_richness * mean_QDS_turnover,
  data = na.exclude(filter(richness_turnover_data, region == "SWA"))
)

mean_QDS_richness_range <- seq(0, 1500, by = 1500 / 100)
mean_QDS_turnover_range <- seq(0.5, 1, by = 1 / 100)
Cape_pred <- map_df(
  .x = mean_QDS_turnover_range,
  .f = function(.x) {
    pred <- predict(Cape_m3, newdata = data.frame(
      mean_QDS_richness = mean_QDS_richness_range,
      mean_QDS_turnover = .x
    ))
    data.frame(
      pred_HDS_richness = pred,
      mean_QDS_richness = mean_QDS_richness_range,
      mean_QDS_turnover = .x
    )
  }
)
SWA_pred <- map_df(
  .x = mean_QDS_turnover_range,
  .f = function(.x) {
    pred <- predict(SWA_m3, newdata = data.frame(
      mean_QDS_richness = mean_QDS_richness_range,
      mean_QDS_turnover = .x
    ))
    data.frame(
      pred_HDS_richness = pred,
      mean_QDS_richness = mean_QDS_richness_range,
      mean_QDS_turnover = .x
    )
  }
)
pred <- rbind(
  cbind(region = "Cape", Cape_pred),
  cbind(region = "SWA", SWA_pred)
)

#HDS_richness_residuals <- data.frame(rbind(
#  cbind(region = "Cape", HDS_richness_residual = Cape_m3$residuals),
#  cbind(region = "SWA", HDS_richness_residual = SWA_m3$residuals)
#))
#HDS_richness_residuals %<>% mutate(
#  HDS_richness_residual = as.numeric(as.character(HDS_richness_residual))
#)
#HDS_richness_residuals <- richness_turnover_data %>%
#  na.exclude() %$%
#  cbind(
#    mean_QDS_richness,
#    mean_QDS_turnover,
#    HDS_richness_residuals
#  )


heat_map <- ggplot(pred, aes(mean_QDS_turnover, mean_QDS_richness)) +
  geom_tile(aes(fill = pred_HDS_richness)) +
  geom_point(
    data = richness_turnover_data,
    aes(col = HDS_richness)
  ) +
  geom_point(
    data = richness_turnover_data,
    pch = 21, col = "white", fill = NA, alpha = 0.5
  ) +
  facet_grid(~ region, scales = "fixed") +
  labs(
    x = "Mean QDS turnover",
    y = "Mean QDS richness"
  ) +
  scale_fill_viridis_c(name = "Est. HDS richness") +
  scale_color_viridis_c(name = "Obs. HDS richness") +
  guides(alpha = guide_legend(
    title = "Obs. HDS richness",
    override.aes = list(col = "grey75")
  ))
ggsave(
  here::here("figures/WIP/richness-turnpover-surface-plot_heatmap.png"),
  heat_map,
  width = 8, height = 4
)

op <- par()
par(mfrow = c(1, 2))
pdf(here::here("figures/WIP/richness-turnpover-surface-plot_lattice.pdf"))
pred %>%
  filter(
    region == "Cape",
    mean_QDS_richness %in% seq(0, 1500, by = 1500 / 10),
    mean_QDS_turnover %in% seq(0.5, 1, by = 1 / 10)
  ) %>%
  wireframe(
    pred_HDS_richness ~ mean_QDS_richness * mean_QDS_turnover,
    data = .,
    main = "Cape",
    zlab = list("Est. HDS richness", rot = 90),
    xlab = "Mean QDS richness",
    ylab = "Mean QDS turnover",
    zlim = c(0, 5000),
    #drape = TRUE,
    #colorkey = TRUE,
    screen = list(z = -50, x = -90),
    scales = list(arrows = FALSE, col = 1),
    par.settings = list(axis.line = list(col = "transparent"))
  )
pred %>%
  filter(
    region == "SWA",
    mean_QDS_richness %in% seq(0, 1500, by = 1500 / 10),
    mean_QDS_turnover %in% seq(0.5, 1, by = 1 / 10)
  ) %>%
  wireframe(
    pred_HDS_richness ~ mean_QDS_richness * mean_QDS_turnover,
    data = .,
    main = "SWA",
    zlab = list("Est. HDS richness", rot = 90),
    xlab = "Mean QDS richness",
    ylab = "Mean QDS turnover",
    zlim = c(0, 5000),
    #drape = TRUE,
    #colorkey = TRUE,
    screen = list(z = -50, x = -90),
    scales = list(arrows = FALSE, col = 1),
    par.settings = list(axis.line = list(col = "transparent"))
  )
# TODO: residuals as lines from surface
dev.off()
par(op)
