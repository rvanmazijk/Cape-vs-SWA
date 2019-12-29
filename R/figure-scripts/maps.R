# Import data ------------------------------------------------------------------
# All at HDS-scale for main figure

# .... Raster data -------------------------------------------------------------

raster_list_template <- list(QDS = "QDS", HDS = "HDS", DS = "DS")

# Richness rasters
GCFR_richness <- SWAFR_richness <- raster_list_template
GCFR_richness  %<>% map(~raster(glue("{data_dir}/GCFR_{.x}_richness.tif")))
SWAFR_richness %<>% map(~raster(glue("{data_dir}/SWAFR_{.x}_richness.tif")))

# PC1 rasters
GCFR_PC1 <- SWAFR_PC1 <- raster_list_template
GCFR_PC1  %<>% map(~raster(glue("{data_dir}/GCFR_{.x}_PC1.tif")))
SWAFR_PC1 %<>% map(~raster(glue("{data_dir}/SWAFR_{.x}_PC1.tif")))

# Multivariate model residual rasters
# (Filenames are correct, I promise!)
GCFR_MV_residuals <- SWAFR_MV_residuals <- raster_list_template
GCFR_MV_residuals  %<>% map(~raster(glue(
  "{data_dir}/GCFR_{.x}_multivariate_richness.tif"
)))
SWAFR_MV_residuals %<>% map(~raster(glue(
  "{data_dir}/SWAFR_{.x}_multivariate_richness.tif"
)))

# NOTE: See below, where I generate PC1 model residual rasters quickly

# .... Border shapefiles -------------------------------------------------------

# Import WGS84-CRS shapes
GCFR_border  <- readOGR(here("data/derived-data/borders/GCFR_border"))
SWAFR_border <- readOGR(here("data/derived-data/borders/SWBP_Mike-Cramer"))

# Save WGS84-CRS
std_CRS <- proj4string(GCFR_border)
# Check
proj4string(SWAFR_border) == std_CRS

# Reproject to planar coordinates
# (so GEOS package can dissolve border & slightly buffer)
temp_CRS <- CRS("+init=epsg:3347")
GCFR_border  <- spTransform(GCFR_border,  temp_CRS)
SWAFR_border <- spTransform(SWAFR_border, temp_CRS)

# Dissolve & slightly buffer borders
GCFR_border_dissolved  <- buffer(GCFR_border,  width = 2.5e4, dissolve = TRUE)
SWAFR_border_dissolved <- buffer(SWAFR_border, width = 2.5e4, dissolve = TRUE)

# Reproject **back** to WGS84-CRS for plotting
GCFR_border_dissolved  <- spTransform(GCFR_border_dissolved,  std_CRS)
SWAFR_border_dissolved <- spTransform(SWAFR_border_dissolved, std_CRS)

# Check
par(mfrow = c(1, 2))
plot(GCFR_border_dissolved)
plot(SWAFR_border_dissolved)
par(op)

# .... Outlier squares ---------------------------------------------------------

outliers <- read_csv(here("results/list-outlier-squares.csv"))

# Set colour palette for these maps --------------------------------------------

my_palette <- rev(viridis::viridis(10))

# Make ggplot-borders for repeated use -----------------------------------------

GCFR_border_gg <- geom_polygon(
  data = GCFR_border_dissolved,
  aes(x = long, y = lat, group = group),
  colour = "black", fill = NA
)
SWAFR_border_gg <- geom_polygon(
  data = SWAFR_border_dissolved,
  aes(x = long, y = lat, group = group),
  colour = "black", fill = NA
)

# Make ggplot-points for cities ------------------------------------------------

CT_point <- geom_point(
  aes(x = 18.4241, y = -33.9249),
  size = 2
)
CT_text <- geom_text(
  aes(x = 18.4241, y = -33.9249, label = "Cape\nTown"),
  size = 3,
  nudge_x = -1.5
)

PE_point <- geom_point(
  aes(x = 25.6022, y = -33.9608),
  size = 2
)
PE_text <- geom_text(
  aes(x = 25.6022, y = -33.9608, label = "Port Elizabeth"),
  size = 3,
  nudge_y = -0.75
)

PR_point <- geom_point(
  aes(x = 115.8605, y = -31.9505),
  size = 2
)
PR_text <- geom_text(
  aes(x = 115.8605, y = -31.9505, label = "Perth"),
  size = 3,
  nudge_x = -1.5
)

ES_point <- geom_point(
  aes(x = 121.8914, y = -33.8613),
  size = 2
)
ES_text <- geom_text(
  aes(x = 121.8914, y = -33.8613, label = "Esperance"),
  size = 3,
  nudge_y = -0.75
)

# Richness maps ----------------------------------------------------------------

# Define richness limits for scales
richness_lims <- map2(GCFR_richness, SWAFR_richness,
  ~range(c(.x[], .y[]), na.rm = TRUE)
)
richness_lims$QDS[[2]] <- richness_lims$QDS[[2]] + 250
richness_lims$HDS[[2]] <- richness_lims$HDS[[2]] + 250
richness_lims$DS[[2]]  <- richness_lims$DS[[2]]  + 250

# Make each region's map
GCFR_richness_plots <- imap(GCFR_richness,
  ~ gplot(.x) +
    geom_tile(aes(fill = value)) +
    GCFR_border_gg +
    CT_point + CT_text +
    PE_point + PE_text +
    labs(title = "GCFR", y = "Latitude (º)") +
    annotate("text", x = 17, y = -26, label = "(a)", hjust = 1, vjust = -0.8) +
    scale_x_continuous(breaks = c(18, 22, 26)) +
    scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
    scale_fill_gradientn(
      limits   = richness_lims[[.y]],
      colours  = my_palette,
      na.value = NA
    ) +
    theme(
      axis.ticks.x    = element_blank(),
      axis.text.x     = element_blank(),
      axis.title.x    = element_blank(),
      plot.title      = element_text(hjust = 0.5),
      legend.position = "none"
    )
)
SWAFR_richness_plots <- imap(SWAFR_richness,
  ~ gplot(.x) +
    geom_tile(aes(fill = value)) +
    SWAFR_border_gg +
    PR_point + PR_text +
    ES_point + ES_text +
    ggtitle("SWAFR") +
    geom_label(
      aes(x = 113, y = -26, label = "(b)"),
      nudge_y = 0.5,
      fill = "white", label.size = 0
    ) +
    scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
    scale_fill_gradientn(
      name     = bquote(italic("S")[.(.y)]),
      limits   = richness_lims[[.y]],
      colours  = my_palette,
      na.value = NA
    ) +
    theme(
      axis.ticks.x         = element_blank(),
      axis.text.x          = element_blank(),
      axis.title.x         = element_blank(),
      axis.ticks.y         = element_blank(),
      axis.text.y          = element_blank(),
      axis.title.y         = element_blank(),
      plot.title           = element_text(hjust = 0.5),
      legend.direction     = "horizontal",
      legend.position      = c(1, 0.9),
      legend.justification = "right",
      legend.background    = element_rect(fill = NA)
    )
)

# PC1 maps ---------------------------------------------------------------------

PC1_lims <- map2(GCFR_PC1, SWAFR_PC1,
  ~range(c(.x[], .y[]), na.rm = TRUE)
)

GCFR_PC1_plots <- imap(GCFR_PC1,
  ~ gplot(.x) +
    geom_tile(aes(fill = value)) +
    GCFR_border_gg +
    CT_point + CT_text +
    PE_point + PE_text +
    ylab("Latitude (º)") +
    annotate("text", x = 17, y = -26, label = "(c)", hjust = 1, vjust = -0.8) +
    scale_x_continuous(breaks = c(18, 22, 26)) +
    scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
    scale_fill_gradientn(
      limits   = PC1_lims[[.y]],
      colours  = my_palette,
      na.value = NA
    ) +
    theme(
      axis.ticks.x    = element_blank(),
      axis.text.x     = element_blank(),
      axis.title.x    = element_blank(),
      legend.position = "none"
    )
)
SWAFR_PC1_plots <- imap(SWAFR_PC1,
  ~ gplot(.x) +
    geom_tile(aes(fill = value)) +
    SWAFR_border_gg +
    PR_point + PR_text +
    ES_point + ES_text +
    geom_label(
      aes(x = 113, y = -26, label = "(d)"),
      nudge_y = 0.5,
      fill = "white", label.size = 0
    ) +
    scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
    scale_fill_gradientn(
      name     = "PC1",
      limits   = PC1_lims[[.y]],
      colours  = my_palette,
      na.value = NA
    ) +
    theme(
      axis.ticks.x         = element_blank(),
      axis.text.x          = element_blank(),
      axis.title.x         = element_blank(),
      axis.ticks.y         = element_blank(),
      axis.text.y          = element_blank(),
      axis.title.y         = element_blank(),
      legend.direction     = "horizontal",
      legend.position      = c(1, 0.9),
      legend.justification = "right",
      legend.background    = element_rect(fill = NA)
    )
)

# PC1 residuals maps -----------------------------------------------------------

# .... Generate residuals ------------------------------------------------------

# Regress richness against PC1 here for convenience
# to keep residuals in raster form

values_sans_NAs <- function(x) {
  x[!is.na(x[])]
}

PC1_residuals <- pmap(list(         GCFR_richness,      SWAFR_richness,
                                    GCFR_PC1,           SWAFR_PC1),
                      function(each_GCFR_richness, each_SWAFR_richness,
                               each_GCFR_PC1,      each_SWAFR_PC1) {

  each_GCFR_richness_values  <- values_sans_NAs(each_GCFR_richness)
  each_SWAFR_richness_values <- values_sans_NAs(each_SWAFR_richness)

  each_GCFR_PC1_values       <- values_sans_NAs(each_GCFR_PC1)
  each_SWAFR_PC1_values      <- values_sans_NAs(each_SWAFR_PC1)

  m <- lm(
    c(each_GCFR_richness_values, each_SWAFR_richness_values) ~
    c(each_GCFR_PC1_values,      each_SWAFR_PC1_values)
  )

  GCFR_residuals <- each_GCFR_richness
  GCFR_residuals[] <- NA
  GCFR_residuals[!is.na(each_GCFR_richness[])] <- residuals(m)[
    1:length(each_GCFR_richness_values)
  ]

  SWAFR_residuals <- each_SWAFR_richness
  SWAFR_residuals[] <- NA
  SWAFR_residuals[!is.na(each_SWAFR_richness[])] <- residuals(m)[
    (length(each_GCFR_richness_values) + 1) :
    (length(each_SWAFR_richness_values) + length(each_SWAFR_richness_values))
  ]

  list(
    GCFR  = GCFR_residuals,
    SWAFR = SWAFR_residuals
  )

})

# .... Make maps proper --------------------------------------------------------

residuals_lims <- map(PC1_residuals,
  ~range(c(.$SWAFR[], .$GCFR[]), na.rm = TRUE)
)

residuals_lims$QDS[[2]] <- residuals_lims$QDS[[2]] + 250
residuals_lims$HDS[[2]] <- residuals_lims$HDS[[2]] + 250
residuals_lims$DS[[2]]  <- residuals_lims$DS[[2]]  + 250

GCFR_PC1_residuals_plots <- PC1_residuals %$%
  list(QDS = QDS$GCFR, HDS = HDS$GCFR, DS = DS$GCFR) %>%
  imap(~ gplot(.x) +
    geom_tile(aes(fill = value)) +
    GCFR_border_gg +
    CT_point + CT_text +
    PE_point + PE_text +
    ylab("Latitude (º)") +
    annotate("text", x = 17, y = -26, label = "(e)", hjust = 1, vjust = -0.8) +
    scale_x_continuous(breaks = c(18, 22, 26)) +
    scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
    scale_fill_gradientn(
      limits   = residuals_lims[[.y]],
      colours  = my_palette,
      na.value = NA
    ) +
    theme(
      axis.ticks.x    = element_blank(),
      axis.text.x     = element_blank(),
      axis.title.x    = element_blank(),
      legend.position = "none"
    )
  )
SWAFR_PC1_residuals_plots <- PC1_residuals %$%
  list(QDS = QDS$SWAFR, HDS = HDS$SWAFR, DS = DS$SWAFR) %>%
  imap(~ gplot(.x) +
    geom_tile(aes(fill = value)) +
    SWAFR_border_gg +
    PR_point + PR_text +
    ES_point + ES_text +
    geom_label(
      aes(x = 113, y = -26, label = "(f)"),
      nudge_y = 0.5,
      fill = "white", label.size = 0
    ) +
    scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
    scale_fill_gradientn(
      name     = bquote("Res."~italic("S")[.(.y)]~"(PC1)"),
      limits   = residuals_lims[[.y]],
      colours  = my_palette,
      na.value = NA
    ) +
    theme(
      axis.ticks.x         = element_blank(),
      axis.text.x          = element_blank(),
      axis.title.x         = element_blank(),
      axis.ticks.y         = element_blank(),
      axis.text.y          = element_blank(),
      axis.title.y         = element_blank(),
      legend.direction     = "horizontal",
      legend.position      = c(1, 0.9),
      legend.justification = "right",
      legend.background    = element_rect(fill = NA)
    )
  )

# Multivariate residuals maps --------------------------------------------------

GCFR_MV_residuals_plots <- imap(GCFR_MV_residuals,
  ~ gplot(.x) +
    geom_tile(aes(fill = value)) +
    GCFR_border_gg +
    CT_point + CT_text +
    PE_point + PE_text +
    labs(
      x = "Longitude (º)",
      y = "Latitude (º)"
    ) +
    annotate("text", x = 17, y = -26, label = "(g)", hjust = 1, vjust = -0.8) +
    scale_x_continuous(breaks = c(18, 22, 26)) +#, limits = c(16, 28)) +
    scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
    scale_fill_gradientn(
      limits   = residuals_lims[[.y]],
      colours  = my_palette,
      na.value = NA
    ) +
    theme(legend.position = "none")
)
SWAFR_MV_residuals_plots <- imap(SWAFR_MV_residuals,
  ~ gplot(.x) +
    geom_tile(aes(fill = value)) +
    SWAFR_border_gg +
    PR_point + PR_text +
    ES_point + ES_text +
    xlab("Longitude (º)") +
    geom_label(
      aes(x = 113, y = -26, label = "(h)"),
      nudge_y = 0.5,
      fill = "white", label.size = 0
    ) +
    scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
    scale_fill_gradientn(
      name     = bquote("Res."~italic("S")[.(.y)]~"(MV)"),
      limits   = residuals_lims[[.y]],
      colours  = my_palette,
      na.value = NA
    ) +
    theme(
      axis.ticks.y         = element_blank(),
      axis.text.y          = element_blank(),
      axis.title.y         = element_blank(),
      legend.direction     = "horizontal",
      legend.position      = c(1, 0.9),
      legend.justification = "right",
      legend.background    = element_rect(fill = NA)
    )
)

# Outlier maps -----------------------------------------------------------------

# .... Confirm that outliers are same in these rasters as in dataset -----------

# ........ PC1-outliers --------------------------------------------------------

# To merge regions' rasters together, ammend origins to be same
set_origin <- function(r, ox = 0, oy = 0) {
  origin(r) <- c(ox, oy)
  r
}

# Merge
PC1_residuals2 <- PC1_residuals %>%
  map(map, set_origin) %>%
  map(~merge(.$GCFR, .$SWAFR))

# Check
PC1_residuals2 %>%
  map(~which(scale(.[]) > 2)) %>%
  map(length)
outliers %>%
  filter(is_PC1_outlier == "*") %>%
  group_by(scale) %>%
  summarise(n())
# FIXME: not the same

# ........ MV-outliers ---------------------------------------------------------

# Ammend origins to be same
GCFR_MV_residuals2  <- map(GCFR_MV_residuals,  set_origin)
SWAFR_MV_residuals2 <- map(SWAFR_MV_residuals, set_origin)

# Check
map(GCFR_MV_residuals2, origin)
map(SWAFR_MV_residuals2, origin)

# Merge
MV_residuals2 <- map2(GCFR_MV_residuals2, SWAFR_MV_residuals2, merge)

# Check
MV_residuals2 %>%
  map(~which(scale(.[]) > 2)) %>%
  map(length)
outliers %>%
  filter(is_MV_outlier == "*") %>%
  group_by(scale) %>%
  summarise(n())
# Same :)

# .... Plot outliers -----------------------------------------------------------

border_gg   <- list(GCFR = GCFR_border_gg, SWAFR = SWAFR_border_gg)

city1_point <- list(GCFR = CT_point,       SWAFR = PR_point)
city1_text  <- list(GCFR = CT_point,       SWAFR = PR_text)

city2_point <- list(GCFR = PE_point,       SWAFR = ES_point)
city2_text  <- list(GCFR = PE_text,        SWAFR = ES_text)

outlier_maps <-
  map(list(PC1 = "is_PC1_outlier", MV = "is_MV_outlier"),
      function(each_outlier_type) {
    map(list(QDS = "QDS", HDS = "HDS", DS = "DS"),
        function(each_scale) {
      map(list(GCFR = "GCFR", SWAFR = "SWAFR"),
          function(each_region) {
        outliers %>%
          filter(region == each_region, scale == each_scale) %>%
          ggplot() +
            aes_string(
              "lon", "lat",
              colour = each_outlier_type,
              fill = each_outlier_type
              ) +
            geom_tile(size = 0.5) +
            border_gg[[each_region]] +
            city1_point[[each_region]] + city1_text[[each_region]] +
            city2_point[[each_region]] + city2_text[[each_region]] +
            labs(
              x = "Longitude (º)",
              y = "Latitude (º)"
            ) +
            scale_y_continuous(
              breaks = c(-34, -30, -26),
              limits = c(-35.5, -25)
            ) +
            scale_colour_manual(values = "black", na.value = NA) +
            scale_fill_manual(values = "red",     na.value = NA) +
            theme(legend.position = "none")
      })
    })
  })

outlier_maps

# Panel all together -----------------------------------------------------------

all_plots <- pmap(list(GCFR_richness_plots,      SWAFR_richness_plots,
                       GCFR_PC1_plots,           SWAFR_PC1_plots,
                       GCFR_PC1_residuals_plots, SWAFR_PC1_residuals_plots,
                       GCFR_MV_residuals_plots,  SWAFR_MV_residuals_plots),
  ~ plot_grid(
    ..1, ..2,
    ..3, ..4,
    ..5, ..6,
    ..7, ..8,
    nrow = 4, rel_heights = c(1, 0.9, 0.9, 1)
  )
)

# Save to disc -----------------------------------------------------------------

imap(all_plots, ~ {
  ggsave(
    here("figures", glue("maps-{.y}.pdf")),
    .x,
    width = 7, height = 12
  )
  ggsave(
    here("figures", glue("maps-{.y}.png")),
    .x, dpi = 600,
    width = 7, height = 12
  )
})
