# Make Fig. 1 (Environmental heterogeneity and scales)
# (Simpler version, emphasising CLES)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
import_objects(output_paths[[1]])

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

# Data-wrangling ---------------------------------------------------------------

jackknifed_CLES_summary_QDS %<>%
  gather(variable, CLES) %>%
  mutate(mean_or_sd = ifelse(str_detect(variable, "_mean"),
                             "CLES_mean",
                             "CLES_sd")) %>%
  mutate(variable = str_remove(variable, "_mean")) %>%
  mutate(variable = str_remove(variable, "_sd")) %>%
  spread(mean_or_sd, CLES) %>%
  mutate(resolution = "QDS")
jackknifed_CLES_summary_QDS$variable %<>% factor(levels = var_names)
jackknifed_CLES_summary_HDS %<>%
  gather(variable, CLES) %>%
  mutate(mean_or_sd = ifelse(str_detect(variable, "_mean"),
                             "CLES_mean",
                             "CLES_sd")) %>%
  mutate(variable = str_remove(variable, "_mean")) %>%
  mutate(variable = str_remove(variable, "_sd")) %>%
  spread(mean_or_sd, CLES) %>%
  mutate(resolution = "HDS")
jackknifed_CLES_summary_HDS$variable %<>% factor(levels = var_names)

jackknifed_CLES_summary <- full_join(
  jackknifed_CLES_summary_QDS,
  jackknifed_CLES_summary_HDS
)
jackknifed_CLES_summary %<>% mutate(
  CLES_upper = CLES_mean + CLES_sd,
  CLES_lower = CLES_mean - CLES_sd
)
test_results_summary %<>%
  gather(resolution, sig, -variable) %>%
  mutate(sig = ifelse(sig, "", "NS")) %>%
  left_join(jackknifed_CLES_summary)

data <- test_results_CLES_for_plot %>%
  mutate(
    variable_type = case_when(  # for colouring variables' lines etc.
      variable == "Elevation"      ~ "Elevation",
      variable == "NDVI"           ~ "NDVI",
      variable %in% var_names[2:4] ~ "Climate",
      variable %in% var_names[6:9] ~ "Soil"
    ),
    variable_type2 = case_when(  # for grouping variables into panels
      variable == "Elevation"      ~ "Elevation",
      variable == "NDVI"           ~ "NDVI",
      variable %in% var_names[2:4] ~ "Climate",
      TRUE                         ~ variable
    ),
    variable_type3 = case_when(  # ...
      variable %in% var_names[1:5] ~ "Non-soil",
      TRUE                         ~ variable
    ),
    CLES = 1 - CLES  # Make CLES Cape - SWA, not SWA - Cape
  ) %>%
  full_join(test_results_summary) %>%
  mutate(
    variable = factor(variable,
      levels = var_names
    ),
    variable_type = factor(variable_type,
      levels = c("Elevation", "Climate", "NDVI", "Soil")
    ),
    variable_type2 = factor(variable_type2,
      levels = c("Elevation", "Climate", "NDVI", "CEC", "Clay", "Soil C", "pH")
    ),
    variable_type3 = factor(variable_type3,
      levels = c("Non-soil", "CEC", "Clay", "Soil C", "pH")
    ),
    resolution = factor(resolution,
      levels = c("0.05º", "QDS", "HDS", "3QDS")
    )
  )

pd <- position_dodge(0.4)

ggplot(data, aes(resolution, CLES, col = variable_type)) +
  geom_point(
    aes(shape = variable),
    size = 2#, position = pd
  ) +
  geom_line(
    aes(group = variable)#, position = pd
  ) +
  #geom_errorbar(
  #  aes(
  #    ymin = CLES_lower,
  #    ymax = CLES_upper,
  #    group = paste(variable, resolution)
  #  ),
  #  width = 0, alpha = 0.5,
  #  position = pd
  #) +
  #geom_point(
  #  aes(y = CLES_mean, shape = variable),
  #  size = 2, alpha = 0.5,
  #  position = pd
  #) +
  geom_text(aes(label = sig), size = 2, col = "black", nudge_x = 0.4) +
  scale_colour_manual(values = var_colours, guide = FALSE) +
  scale_shape_manual(values = var_shapes) +
  facet_wrap(~ variable_type, nrow = 1, dir = "h") +
  xlab("Spatial resolution") +
  ylab("CLES (Cape > SWA)") +
  ylim(0.4, 1) +
  guides(shape = guide_legend(
    title = "Environmental variables",
    nrow = 5, ncol = 2,
    direction = "vertical",
    override.aes = list(col = c(
      var_colours[1],
      rep(var_colours[2], 3),
      var_colours[3],
      rep(var_colours[4], 4)
    ))
  ))
