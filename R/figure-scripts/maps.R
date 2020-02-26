# Import raster data -----------------------------------------------------------

raster_list_template <- list(QDS = "QDS", HDS = "HDS", DS = "DS")

richness <- map(raster_list_template, ~raster(glue(
  "{data_dir}/raster-layers/",
  "{.x}-richness_{.x}.tif"
)))
PC1 <- map(raster_list_template, ~raster(glue(
  "{data_dir}/raster-layers/",
  "PC1_{.x}.tif"
)))
PC1_residual <- map(raster_list_template, ~raster(glue(
  "{data_dir}/raster-layers/",
  "PC1_residual_{.x}.tif"
)))
MV_residual <- map(raster_list_template, ~raster(glue(
  "{data_dir}/raster-layers/",
  "MV_residual_{.x}.tif"
)))

# Split rasters up into each regions' ------------------------------------------

GCFR_richness  <- map(richness, crop, GCFR_border_buffered)
SWAFR_richness <- map(richness, crop, SWAFR_border_buffered)

GCFR_PC1  <- map(PC1, crop, GCFR_border_buffered)
SWAFR_PC1 <- map(PC1, crop, SWAFR_border_buffered)

GCFR_PC1_residual  <- map(PC1_residual, crop, GCFR_border_buffered)
SWAFR_PC1_residual <- map(PC1_residual, crop, SWAFR_border_buffered)

GCFR_MV_residual  <- map(MV_residual, crop, GCFR_border_buffered)
SWAFR_MV_residual <- map(MV_residual, crop, SWAFR_border_buffered)

# Import list of outlier grid-cells --------------------------------------------

outliers <- read_csv(here("results/list-outlier-squares.csv"))

# Set palette & some reusable gg-objects ---------------------------------------

my_palette <- rev(viridis::viridis(10))

no_x_axis <- theme(
  axis.ticks.x = element_blank(),
  axis.text.x  = element_blank(),
  axis.title.x = element_blank()
)
no_y_axis <- theme(
  axis.ticks.y = element_blank(),
  axis.text.y  = element_blank(),
  axis.title.y = element_blank()
)

# Make ggplot-borders for repeated use -----------------------------------------

# Import WGS84-CRS shapes
GCFR_border  <- readOGR(here("data/derived-data/borders/GCFR_border"))
SWAFR_border <- readOGR(here("data/derived-data/borders/SWBP_Mike-Cramer"))

# Save WGS84-CRS
std_CRS <- proj4string(GCFR_border)
# Check
proj4string(SWAFR_border) == std_CRS
SWAFR_border <- spTransform(SWAFR_border, std_CRS)

# Reproject to planar coordinates
# (so GEOS package can dissolve border & slightly buffer)
temp_CRS <- CRS("+init=epsg:3347")
GCFR_border  <- spTransform(GCFR_border,  temp_CRS)
SWAFR_border <- spTransform(SWAFR_border, temp_CRS)

GCFR_border_dissolved  <- buffer(GCFR_border,  width = 2.5e4, dissolve = TRUE)
SWAFR_border_dissolved <- buffer(SWAFR_border, width = 2.5e4, dissolve = TRUE)

# Project back again
GCFR_border_dissolved  %<>% spTransform(std_CRS)
SWAFR_border_dissolved %<>% spTransform(std_CRS)

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
  size = 1.5
)
CT_text <- geom_text(
  aes(x = 18.4241, y = -33.9249, label = "Cape\nTown"),
  size = 3,
  nudge_x = -1.5
)

PE_point <- geom_point(
  aes(x = 25.6022, y = -33.9608),
  size = 1.5
)
PE_text <- geom_text(
  aes(x = 25.6022, y = -33.9608, label = "Port Elizabeth"),
  size = 3,
  nudge_y = -0.75
)

PR_point <- geom_point(
  aes(x = 115.8605, y = -31.9505),
  size = 1.5
)
PR_text <- geom_text(
  aes(x = 115.8605, y = -31.9505, label = "Perth"),
  size = 3,
  nudge_x = -1.5
)

ES_point <- geom_point(
  aes(x = 121.8914, y = -33.8613),
  size = 1.5
)
ES_text <- geom_text(
  aes(x = 121.8914, y = -33.8613, label = "Esperance"),
  size = 3,
  nudge_y = -0.75
)

# Richness maps ----------------------------------------------------------------

# .... Define richness limits for scales ---------------------------------------

richness_lims <- map2(GCFR_richness, SWAFR_richness,
  ~range(c(.x[], .y[]), na.rm = TRUE)
)
richness_lims$QDS[[2]] <- richness_lims$QDS[[2]] + 250
richness_lims$HDS[[2]] <- richness_lims$HDS[[2]] + 250
richness_lims$DS[[2]]  <- richness_lims$DS[[2]]  + 250

# .... Make each region's map --------------------------------------------------

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
      plot.title      = element_text(hjust = 0.5),
      legend.position = "none"
    ) +
    no_x_axis
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
      plot.title           = element_text(hjust = 0.5),
      legend.direction     = "horizontal",
      legend.position      = c(1, 0.8),
      legend.justification = "right",
      legend.background    = element_rect(fill = NA)
    ) +
    no_x_axis +
    no_y_axis
)

# PC1 maps ---------------------------------------------------------------------

# .... Define PC1 limits for scales --------------------------------------------

PC1_lims <- map2(GCFR_PC1, SWAFR_PC1,
  ~range(c(.x[], .y[]), na.rm = TRUE)
)

# .... Make each region's map --------------------------------------------------

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
    theme(legend.position = "none") +
    no_x_axis
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
      legend.direction     = "horizontal",
      legend.position      = c(1, 0.8),
      legend.justification = "right",
      legend.background    = element_rect(fill = NA)
    ) +
    no_x_axis +
    no_y_axis
)

# PC1 residuals maps -----------------------------------------------------------

# .... Define Res. S limits for scales -----------------------------------------

residuals_lims <- map2(GCFR_PC1_residual, SWAFR_PC1_residual,
  ~range(c(.x[], .y[]), na.rm = TRUE)
)
residuals_lims$QDS[[2]] <- residuals_lims$QDS[[2]] + 250
residuals_lims$HDS[[2]] <- residuals_lims$HDS[[2]] + 250
residuals_lims$DS[[2]]  <- residuals_lims$DS[[2]]  + 250

# .... Make each region's map --------------------------------------------------

GCFR_PC1_residuals_plots <- imap(GCFR_PC1_residual,
  ~ gplot(.x) +
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
    theme(legend.position = "none") +
    no_x_axis
)
SWAFR_PC1_residuals_plots <- imap(SWAFR_PC1_residual,
  ~ gplot(.x) +
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
      legend.direction     = "horizontal",
      legend.position      = c(1, 0.8),
      legend.justification = "right",
      legend.background    = element_rect(fill = NA)
    ) +
    no_x_axis +
    no_y_axis
)

# Multivariate residuals maps --------------------------------------------------

# .... Make each region's map --------------------------------------------------

GCFR_MV_residuals_plots <- imap(GCFR_MV_residual,
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
SWAFR_MV_residuals_plots <- imap(SWAFR_MV_residual,
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
      legend.direction     = "horizontal",
      legend.position      = c(1, 0.8),
      legend.justification = "right",
      legend.background    = element_rect(fill = NA)
    ) +
    no_y_axis
)

# Outlier maps -----------------------------------------------------------------

# Combine annotations for each region for use in loop below
border_gg   <- list(GCFR = GCFR_border_gg, SWAFR = SWAFR_border_gg)
city1_point <- list(GCFR = CT_point,       SWAFR = PR_point)
city1_text  <- list(GCFR = CT_text,        SWAFR = PR_text)
city2_point <- list(GCFR = PE_point,       SWAFR = ES_point)
city2_text  <- list(GCFR = PE_text,        SWAFR = ES_text)

# Loops to make all possible panels
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
              fill   = each_outlier_type
            ) +
            geom_tile(
              size = 0.5,
              height = case_when(
                each_scale == "QDS" ~ 0.25,
                each_scale == "HDS" ~ 0.50,
                each_scale == "DS"  ~ 1.00
              ),
              width = case_when(
                each_scale == "QDS" ~ 0.25,
                each_scale == "HDS" ~ 0.50,
                each_scale == "DS"  ~ 1.00
              )
            ) +
            border_gg[[each_region]] +
            city1_point[[each_region]] + city1_text[[each_region]] +
            city2_point[[each_region]] + city2_text[[each_region]] +
            labs(
              x = "Longitude (º)",
              y = "Latitude (º)"
            ) +
            geom_label(
              aes(
                x = ifelse(each_region == "GCFR", 17.5, 113.5),
                y = -25.5, size = 1,
                label = case_when(
                  (each_region == "GCFR")  & (each_scale == "QDS") ~ "(a) QDS",
                  (each_region == "SWAFR") & (each_scale == "QDS") ~ "(b)",
                  (each_region == "GCFR")  & (each_scale == "HDS") ~ "(c) HDS",
                  (each_region == "SWAFR") & (each_scale == "HDS") ~ "(d)",
                  (each_region == "GCFR")  & (each_scale == "DS")  ~ "(e) DS",
                  (each_region == "SWAFR") & (each_scale == "DS")  ~ "(f)"
                ),
              ),
              nudge_y = 0.5,
              fill = NA, label.size = 0
            ) +
            # FIXME: causes blank panels...
            scale_x_continuous(breaks =
                   if (each_region == "GCFR")  c(18, 22, 26)
              else if (each_region == "SWAFR") c(114, 118, 122, 126)) +
            scale_y_continuous(
              breaks = c(-34, -30, -26),
              limits = c(-35.5, -24.5)
            ) +
            scale_colour_manual(values = "black",  na.value = NA) +
            scale_fill_manual(values   = "grey75", na.value = NA) +
            theme(legend.position = "none")
      })
    })
  })

# Tidy up
# No outliers for these
outlier_maps$MV$DS <- NULL

# Panel all together -----------------------------------------------------------

rotate_legend_text <- theme(legend.text = element_text(
  angle  = 90,
  hjust  =  1,
  vjust  =  0.5,
  margin = margin(t = -10, unit = "pt")
))

# For main variables' maps
all_plots <- pmap(list(GCFR_richness_plots,      SWAFR_richness_plots,
                       GCFR_PC1_plots,           SWAFR_PC1_plots,
                       GCFR_PC1_residuals_plots, SWAFR_PC1_residuals_plots,
                       GCFR_MV_residuals_plots,  SWAFR_MV_residuals_plots),
  ~ plot_grid(
    ..1, ..2 + rotate_legend_text,
    ..3, ..4 + rotate_legend_text,
    ..5, ..6 + rotate_legend_text,
    ..7, ..8 + rotate_legend_text,
    nrow = 4, rel_heights = c(1, 0.9, 0.9, 1)
  )
)

# For outlier maps
PC1_outlier_maps <- outlier_maps$PC1 %$% plot_grid(
  nrow = 1, rel_widths = c(0.9, 1),
  plot_grid(
    nrow = 3,
    QDS$GCFR +
      no_x_axis +
      ggtitle("GCFR") +
      theme(plot.title = element_text(hjust = 0.5)),
    HDS$GCFR + no_x_axis,
    DS$GCFR
  ),
  plot_grid(
    nrow = 3, rel_heights = c(0.95, 1.05, 0.85),
    QDS$SWAFR +
      no_x_axis +
      no_y_axis +
      ggtitle("SWAFR") +
      theme(plot.title = element_text(hjust = 0.5)),
    HDS$SWAFR + no_y_axis,
    white_rect
  )
)
MV_outlier_maps <- outlier_maps$MV %$% plot_grid(
    nrow = 1, rel_widths = c(0.9, 1),
  plot_grid(
    nrow = 3,
    QDS$GCFR +
      no_x_axis +
      ggtitle("GCFR") +
      theme(plot.title = element_text(hjust = 0.5)),
    HDS$GCFR + no_x_axis,
    white_rect
  ),
  plot_grid(
    nrow = 3, rel_heights = c(0.95, 1.05, 0.85),
    QDS$SWAFR +
      no_x_axis +
      no_y_axis +
      ggtitle("SWAFR") +
      theme(plot.title = element_text(hjust = 0.5)),
    HDS$SWAFR + no_y_axis,
    white_rect
  )
)

# Save all to disc -------------------------------------------------------------

iwalk(all_plots, ~ {
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

ggsave(
  here("figures/map-PC1-outliers.pdf"),
  PC1_outlier_maps,
  width = 7, height = 10
)
ggsave(
  here("figures/map-PC1-outliers.png"),
  PC1_outlier_maps, dpi = 600,
  width = 7, height = 10
)

ggsave(
  here("figures/map-mv-outliers.pdf"),
  MV_outlier_maps,
  width = 7, height = 10
)
ggsave(
  here("figures/map-mv-outliers.png"),
  MV_outlier_maps, dpi = 600,
  width = 7, height = 10
)
