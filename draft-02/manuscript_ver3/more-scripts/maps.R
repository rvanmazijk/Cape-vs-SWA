# Load packages
library(here)
library(tidyverse)

# Global ggplot2 theme settings
theme_set(theme_bw() + theme(
  strip.background = element_blank(),
  panel.grid = element_blank()
))

# Import data
HDS <- read_csv(here("draft-02/outputs/QDS_data_cells.csv"))
QDS <- read_csv(here("draft-02/outputs/EDS_data_cells.csv"))

# Combine datasets for figures
richness_data <- rbind(
  QDS %>%
    dplyr::select(lon, lat, region, QDS_richness) %>%
    rename(response_value = QDS_richness) %>%
    mutate(response = "italic(S)[QDS]"),
  HDS %>%
    dplyr::select(lon, lat, region, HDS_richness) %>%
    rename(response_value = HDS_richness) %>%
    mutate(response = "italic(S)[HDS]"),
  HDS %>%
    dplyr::select(lon, lat, region, add_turnover_prop) %>%
    rename(response_value = add_turnover_prop) %>%
    mutate(response = "italic(T)[QDS]/italic(S)[HDS]")
)

# Plots
richness_data %>%
  mutate(response = factor(response, levels = c(
    "italic(S)[QDS]",
    "italic(S)[HDS]",
    "italic(T)[QDS]/italic(S)[HDS]"
  ))) %>%
  group_by(response) %>%
  mutate(response_value = scale(response_value)) %>%
  ggplot(aes(lon, lat, colour = response_value)) +
    geom_point() +
    facet_grid(response ~ region, scales = "free_x", labeller = label_parsed) +
    scale_colour_viridis_c() +
    theme(strip.text.y = element_text(angle = 0, hjust = 0))
# NOTE: Gaps in S_QDS data is where QDS has < 2 EDS


# Fix wonky HDS-coords
sort(unique(c(
  richness_data$lon %% 1,
  richness_data$lat %% 1
)))
fix_wonky <- function(x) {
  ifelse((x %% 0.05) > 0,
    x - (x %% 0.05) + 0.05,
    x
  )
}
richness_data %>%
  filter(response == "italic(S)[HDS]") %>%
  mutate(lon = fix_wonky(lon), lat = fix_wonky(lat)) %>%
  spread(response, response_value) %>%
  ggplot(aes(lon, lat, colour = `italic(S)[HDS]`)) +
    geom_point(shape = 15, size = 5) +
    labs(x = "Longitude (°)", y = "Latitude (°)") +
    facet_grid(~region, scales = "free_x") +
    scale_colour_viridis_c(name = bquote(italic("S")["HDS"])) +
    theme(strip.text.y = element_text(angle = 0, hjust = 0))
