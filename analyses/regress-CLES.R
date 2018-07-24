# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
import_objects(output_paths[1])

var_names <- c(
  "Elevation",
  "MAP",
  "PDQ",
  "Surface T",
  "NDVI",
  "CEC",
  "Clay",
  "Soil C",
  "pH"
)
var_shapes <- c(
  17,  # triangle      for elevation
  16,  # filled circle for MAP
  1,   # open circle   for PDQ
  15,  # square        for surfact T
  4,   # x             for NDVI,
  17,  # triangle      for CEC
  16,  # filled circle for clay
  1,   # open circle   for soil C
  15   # square        for pH
)
var_colours <- c(
  # <https://colourco.de/>
  "grey50",   # grey   for elevation
  "#507CC5",  # blue   for climate
  "#37A541",  # greeen for NDVI
  "#BA793E"   # brown  for soils
)

data <- test_results_CLES_for_plot %>%
  mutate(
    variable_type = case_when(
      variable == "Elevation"      ~ "Elevation",
      variable == "NDVI"           ~ "NDVI",
      variable %in% var_names[2:4] ~ "Climate",
      variable %in% var_names[6:9] ~ "Soil"
    ),
    CLES = 1 - CLES  # Make CLES Cape - SWA, not SWA - Cape
  ) %>%
  full_join(
    test_results_summary %>%
      gather(resolution, sig, -variable) %>%
      mutate(sig = ifelse(sig, "", "NS"))
  ) %>%
  mutate(
    variable = factor(variable,
      levels = var_names
    ),
    variable_type = factor(variable_type,
      levels = c("Elevation", "Climate", "NDVI", "Soil")
    ),
    resolution = factor(resolution,
      levels = c("0.05º", "QDS", "HDS", "3QDS")
    )
  )

ggplot(data, aes(resolution, CLES, col = variable_type)) +
  geom_point(aes(shape = variable), size = 2) +
  geom_line(aes(group = variable)) +
  geom_text(aes(label = sig), size = 3, col = "black", nudge_x = 0.2) +
  scale_colour_manual(values = var_colours) +
  scale_shape_manual(values = var_shapes) +
  guides(
    colour = guide_legend(title = "", nrow = 3, ncol = 2),
    shape = guide_legend(ncol = 2)
  )
