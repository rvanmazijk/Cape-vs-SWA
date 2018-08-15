# Make Fig. 2 (Exploring species richness and turnover)---version 2
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("figures/figure-setup.R"))

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

# Plots ------------------------------------------------------------------------

richness_turnover_data %>%
  na.exclude() %>%
  ggplot(aes(mean_QDS_richness, HDS_richness, col = region)) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(
      x = "Mean QDS richness",
      y = "HDS richness"
    ) +
    scale_colour_manual(name = "Region", values = my_palette)

richness_turnover_data %>%
  na.exclude() %>%
  ggplot(aes(mean_QDS_turnover, HDS_richness, col = region)) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(
      x = "Mean QDS turnover",
      y = "HDS richness"
    ) +
    scale_colour_manual(name = "Region", values = my_palette)

richness_turnover_data %>%
  na.exclude() %>%
  ggplot(aes(
    mean_QDS_turnover * mean_QDS_richness * 3,
    HDS_richness,
    col = region
  )) +
    geom_point() +
    geom_smooth(method = lm) +
    geom_abline(intercept = 0, slope = 1, col = "grey50", linetype = "dashed") +
    labs(
      x = "Mean QDS richness * mean QDS turnover",
      y = "HDS richness"
    ) +
    scale_colour_manual(name = "Region", values = my_palette)
# Remarkable!!!

# NOTE: Compare to expected HDS richness from QDS on first principles
expect_HDS_richness <- function(richness, turnover, n) {
  richness * ((n - 1) + turnover)
}
# E.g.
# 2 site case: complete turnover
expect_HDS_richness(richness = 10, turnover = 1.0, n = 2)
# 2 site case: incomplete turnover
expect_HDS_richness(richness = 10, turnover = 0.5, n = 2)
# 4 site case: incomplete turnover
expect_HDS_richness(richness = 10, turnover = 1.0, n = 4)
# 4 site case: complete turnover
expect_HDS_richness(richness = 10, turnover = 0.5, n = 4)
richness_turnover_data %<>% mutate(
  expect_HDS_richness = expect_HDS_richness(
    mean_QDS_richness,
    mean_QDS_turnover,
    n = 4  # TODO: generate n_QDS in loops above, for use here
  )
)
