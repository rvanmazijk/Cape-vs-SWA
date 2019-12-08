# Import data ------------------------------------------------------------------
# All at HDS-scale for main figure

# .... Raster data -------------------------------------------------------------

# Richness rasters
GCFR_HDS_richness  <- raster(glue("{data_dir}/GCFR_HDS_richness.tif"))
SWAFR_HDS_richness <- raster(glue("{data_dir}/SWAFR_HDS_richness.tif"))

# PC1 rasters
GCFR_HDS_PC1  <- raster(glue("{data_dir}/GCFR_HDS_PC1.tif"))
SWAFR_HDS_PC1 <- raster(glue("{data_dir}/SWAFR_HDS_PC1.tif"))

# PC1 model residual rasters
# (See below)

# Multivariate model residual rasters
GCFR_multivariate_residuals <-
  raster(glue("{data_dir}/GCFR_HDS_multivariate_richness.tif"))
SWAFR_multivariate_residuals <-
  raster(glue("{data_dir}/SWAFR_HDS_multivariate_richness.tif"))

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
  aes(x = 18.4241, y = -33.9249, label = "Cape Town"),
  size = 2,
  nudge_x = -1.5
)

PE_point <- geom_point(
  aes(x = 25.6022, y = -33.9608),
  size = 2
)
PE_text <- geom_text(
  aes(x = 25.6022, y = -33.9608, label = "Port Elizabeth"),
  size = 2,
  nudge_y = -0.75
)

PR_point <- geom_point(
  aes(x = 115.8605, y = -31.9505),
  size = 2
)
PR_text <- geom_text(
  aes(x = 115.8605, y = -31.9505, label = "Perth"),
  size = 2,
  nudge_x = -1.5
)

ES_point <- geom_point(
  aes(x = 121.8914, y = -33.8613),
  size = 2
)
ES_text <- geom_text(
  aes(x = 121.8914, y = -33.8613, label = "Esperance"),
  size = 2,
  nudge_y = -0.75
)

# Richness maps ----------------------------------------------------------------

richness_lims <- range(
  c(SWAFR_HDS_richness[], GCFR_HDS_richness[]),
  na.rm = TRUE
)
richness_lims[[2]] <- richness_lims[[2]] + 250

GCFR_richness_plot <- gplot(GCFR_HDS_richness) +
  geom_tile(aes(fill = value)) +
  GCFR_border_gg +
  CT_point +
  CT_text +
  PE_point +
  PE_text +
  labs(
    title = "GCFR",
    y     = "Latitude (º)"
  ) +
  annotate("text", x = 17, y = -26, label = "(a)", hjust = 1, vjust = -0.8) +
  scale_x_continuous(breaks = c(18, 22, 26)) +#, limits = c(16, 28)) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
  scale_fill_gradientn(
    limits   = richness_lims,
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
SWAFR_richness_plot <- gplot(SWAFR_HDS_richness) +
  geom_tile(aes(fill = value)) +
  SWAFR_border_gg +
  PR_point +
  PR_text +
  ES_point +
  ES_text +
  ggtitle("SWAFR") +
  geom_label(
    aes(x = 113, y = -26, label = "(b)"),
    nudge_y = 0.5,
    fill = "white", label.size = 0
  ) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
  scale_fill_gradientn(
    name     = bquote(italic("S")["HDS"]),
    limits   = richness_lims,
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

# PC1 maps ---------------------------------------------------------------------

PC1_lims <- range(
  c(SWAFR_HDS_PC1[], GCFR_HDS_PC1[]),
  na.rm = TRUE
)

GCFR_PC1_plot <- gplot(GCFR_HDS_PC1) +
  geom_tile(aes(fill = value)) +
  GCFR_border_gg +
  CT_point +
  CT_text +
  PE_point +
  PE_text +
  ylab("Latitude (º)") +
  annotate("text", x = 17, y = -26, label = "(c)", hjust = 1, vjust = -0.8) +
  scale_x_continuous(breaks = c(18, 22, 26)) +#, limits = c(16, 28)) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
  scale_fill_gradientn(
    limits   = PC1_lims,
    colours  = my_palette,
    na.value = NA
  ) +
  theme(
    axis.ticks.x    = element_blank(),
    axis.text.x     = element_blank(),
    axis.title.x    = element_blank(),
    legend.position = "none"
  )
SWAFR_PC1_plot <- gplot(SWAFR_HDS_PC1) +
  geom_tile(aes(fill = value)) +
  SWAFR_border_gg +
  PR_point +
  PR_text +
  ES_point +
  ES_text +
  geom_label(
    aes(x = 113, y = -26, label = "(d)"),
    nudge_y = 0.5,
    fill = "white", label.size = 0
  ) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
  scale_fill_gradientn(
    name     = "PC1",
    limits   = PC1_lims,
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

# PC1 residuals maps -----------------------------------------------------------

# .... Generate residuals ------------------------------------------------------

# Regress richness against PC1 here for convenience
# to keep residuals in raster form

GCFR_richness_values  <- GCFR_HDS_richness[!is.na(GCFR_HDS_richness[])]
SWAFR_richness_values <- SWAFR_HDS_richness[!is.na(SWAFR_HDS_richness[])]
GCFR_PC1_values       <- GCFR_HDS_PC1[!is.na(GCFR_HDS_PC1[])]
SWAFR_PC1_values      <- SWAFR_HDS_PC1[!is.na(SWAFR_HDS_PC1[])]
m <- lm(
  c(GCFR_richness_values, SWAFR_richness_values) ~
  c(GCFR_PC1_values,      SWAFR_PC1_values)
)
# Check
plot(
  c(GCFR_richness_values, SWAFR_richness_values) ~
  c(GCFR_PC1_values,      SWAFR_PC1_values)
)
abline(m)
# Works!

GCFR_HDS_residuals <- GCFR_HDS_richness
GCFR_HDS_residuals[] <- NA
GCFR_HDS_residuals[!is.na(GCFR_HDS_richness[])] <- residuals(m)[
  1:length(GCFR_richness_values)
]

SWAFR_HDS_residuals <- SWAFR_HDS_richness
SWAFR_HDS_residuals[] <- NA
SWAFR_HDS_residuals[!is.na(SWAFR_HDS_richness[])] <- residuals(m)[
  (length(GCFR_richness_values) + 1) :
  (length(GCFR_richness_values) + length(SWAFR_richness_values))
]

# .... Make maps proper --------------------------------------------------------

residuals_lims <- range(
  c(SWAFR_HDS_residuals[], GCFR_HDS_residuals[]),
  na.rm = TRUE
)
residuals_lims[[2]] <- residuals_lims[[2]] + 250

GCFR_residuals_plot <- gplot(GCFR_HDS_residuals) +
  geom_tile(aes(fill = value)) +
  GCFR_border_gg +
  CT_point +
  CT_text +
  PE_point +
  PE_text +
  ylab("Latitude (º)") +
  annotate("text", x = 17, y = -26, label = "(e)", hjust = 1, vjust = -0.8) +
  scale_x_continuous(breaks = c(18, 22, 26)) +#, limits = c(16, 28)) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
  scale_fill_gradientn(
    limits   = residuals_lims,
    colours  = my_palette,
    na.value = NA
  ) +
  theme(
    axis.ticks.x    = element_blank(),
    axis.text.x     = element_blank(),
    axis.title.x    = element_blank(),
    legend.position = "none"
  )
SWAFR_residuals_plot <- gplot(SWAFR_HDS_residuals) +
  geom_tile(aes(fill = value)) +
  SWAFR_border_gg +
  PR_point +
  PR_text +
  ES_point +
  ES_text +
  geom_label(
    aes(x = 113, y = -26, label = "(f)"),
    nudge_y = 0.5,
    fill = "white", label.size = 0
  ) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
  scale_fill_gradientn(
    name     = bquote("Res."~italic("S")["HDS"]~"(PC1)"),
    limits   = residuals_lims,
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

# Multivariate residuals maps --------------------------------------------------

GCFR_mresiduals_plot <- gplot(GCFR_multivariate_residuals) +
  geom_tile(aes(fill = value)) +
  GCFR_border_gg +
  CT_point +
  CT_text +
  PE_point +
  PE_text +
  labs(
    x = "Longitude (º)",
    y = "Latitude (º)"
  ) +
  annotate("text", x = 17, y = -26, label = "(g)", hjust = 1, vjust = -0.8) +
  scale_x_continuous(breaks = c(18, 22, 26)) +#, limits = c(16, 28)) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
  scale_fill_gradientn(
    limits   = residuals_lims,
    colours  = my_palette,
    na.value = NA
  ) +
  theme(legend.position = "none")
SWAFR_mresiduals_plot <- gplot(SWAFR_multivariate_residuals) +
  geom_tile(aes(fill = value)) +
  SWAFR_border_gg +
  PR_point +
  PR_text +
  ES_point +
  ES_text +
  xlab("Longitude (º)") +
  geom_label(
    aes(x = 113, y = -26, label = "(h)"),
    nudge_y = 0.5,
    fill = "white", label.size = 0
  ) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25)) +
  scale_fill_gradientn(
    name     = bquote("Res."~italic("S")["HDS"]~"(MV)"),
    limits   = residuals_lims,
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

# Panel all together -----------------------------------------------------------

all_maps <- plot_grid(
  GCFR_richness_plot,   SWAFR_richness_plot,
  GCFR_PC1_plot,        SWAFR_PC1_plot,
  GCFR_residuals_plot,  SWAFR_residuals_plot,
  GCFR_mresiduals_plot, SWAFR_mresiduals_plot,
  nrow = 4, rel_heights = c(1, 0.9, 0.9, 1)
)

# Save to disc -----------------------------------------------------------------

ggsave(
  here("draft-02/figures/maps.pdf"),
  all_maps,
  width = 7, height = 12
)
ggsave(
  here("draft-02/figures/maps.png"),
  all_maps, dpi = 600,
  width = 7, height = 12
)
