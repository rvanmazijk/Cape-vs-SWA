# Relate floral species turnover, and richness, with environmental roughness
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
map(pre_analysis_import_paths, source)

GCFR_roughness_QDS <- map(GCFR_variables_QDS, focal_sd)
names(GCFR_roughness_QDS) %<>% paste0("_rough")

SWAFR_roughness_QDS <- map(SWAFR_variables_QDS, focal_sd)
names(SWAFR_roughness_QDS) %<>% paste0("_rough")

GCFR <-
  c(
    richness = mask(GCFR_richness_QDS, GCFR_border_buffered),
    GCFR_variables_QDS,
    GCFR_roughness_QDS
  ) %>%
  map(getValues) %>%
  as_tibble()
SWAFR <-
  c(
    richness = mask(SWAFR_richness_QDS, SWAFR_border_buffered),
    SWAFR_variables_QDS,
    SWAFR_roughness_QDS
  ) %>%
  map(getValues) %>%
  as_tibble()

# Create master data-frame -----------------------------------------------------

data <-
  rbind(
    cbind(region = "GCFR", GCFR),
    cbind(region = "SWAFR", SWAFR)
  ) %>%
  as_tibble() %>%
  na.exclude() %>%
  # Adjust environmental values stored at x10 etc.
  mutate(
    `Surface T` = `Surface T` - 273.15,
    NDVI        = NDVI / 1e+07,
    NDVI_rough  = NDVI_rough / 1e+07,
    pH          = pH / 10,
    pH_rough    = pH_rough / 10
  )
  # TODO: Check which other vars have been x10
  # TODO: Check units for NDVI

# <https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset>
remove_outliers <- function(x, na.rm = TRUE, ...) {
  if (!is.numeric(x)) {
    return(x)
  } else {
    qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    return(y)
  }
}
data2 <- map_df(data, remove_outliers)

# Gather data into long-form
data2 %<>%
  gather(
    key = variable, value = value,
    -region, -richness
  ) %>%
  mutate(
    absolute_or_rough = ifelse(str_detect(variable, "_rough"),
      "Roughness",
      "Absolute"
    ),
    variable = str_remove_all(variable, "_rough")
  )

# Plots ------------------------------------------------------------------------

# Create plots in a for-loop
max_richness <- max(
  cellStats(GCFR_richness_QDS, max),
  cellStats(SWAFR_richness_QDS, max)
)
enviro_scatters <- foreach(var = var_names) %do% {
  data2 %>%
    filter(variable == var) %>%
    ggplot(aes(value, richness, col = region)) +
      geom_density_2d() +
      geom_smooth(method = "lm") +
      #ylim(0, max_richness) +
      facet_grid(variable ~ absolute_or_rough, scales = "free_x") +
      scale_color_manual(values = my_palette) +
      theme(
        strip.text.x = element_blank(),
        axis.title = element_blank(),
        legend.position = "none"
      )
}
names(enviro_scatters) <- var_names
# Combine plots with special care for whitespace,
enviro_scatters <- plot_grid(ncol = 2, labels = c("A", "B"),
  plotlist = enviro_scatters %$% list(
    plot_grid(ncol = 1, rel_heights = c(0.2, 1, 1, 1, 1, 1), plotlist = list(
      grid.rect(gp = gpar(col = "white")),
      Elevation,
      MAP,
      PDQ,
      `Surface T`,
      NDVI
    )),
    plot_grid(ncol = 1, rel_heights = c(0.2, 1, 1, 1, 1, 1), plotlist = list(
      grid.rect(gp = gpar(col = "white")),
      CEC,
      Clay,
      `Soil C`,
      pH,
      grid.rect(gp = gpar(col = "white"))
    ))
  )
)
# ... and new outermost x and y axis labels
enviro_scatters +
  geom_text(
    label = "Environmental value / Roughness value",
    x = 0.5, y = 0.02,
    angle = 0
  ) +
  geom_text(
    label = "Species richness",
    x = 0.02, y = 0.5,
    angle = 90
  )
# TODO: boxplots along sides of each panel

tidy_var_names <- function(x) str_replace_all(tolower(x), " ", "_")
names(GCFR_variables_QDS) %<>% tidy_var_names()
names(SWAFR_variables_QDS) %<>% tidy_var_names()

foo <- lm(
  log(richness + 1) ~
    elevation + ndvi +
    map + pdq + surface_t +
    cec + clay + soil_c + ph +
    (elevation*region) + (ndvi*region) +
    (map*region) + (pdq*region) + (surface_t*region) +
    (cec*region) + (clay*region) + (soil_c*region) + (ph*region),
  data
)
visreg::visreg(
  foo,
  xvar = "clay",
  by = "region",
  overlay = TRUE,
  trans = function(x) log(x + 1)
)
