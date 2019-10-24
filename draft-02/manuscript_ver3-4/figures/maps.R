# Import data ------------------------------------------------------------------
# All at HDS-scale for main figure

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

# TODO: Import border shapefiles
#ZA_border <- read_rds(here("data/raw-data/ZA-border/GADM_2.8_ZAF_adm0.rds"))
#ZA_border_dissolved <- buffer(ZA_border, width = 0.1, dissolve = TRUE)
#GCFR_border <- readOGR(here("data/derived-data/borders/GCFR_border"))
#GCFR_border_dissolved <- buffer(GCFR_border, width = 0, dissolve = TRUE)
#AU_border <- read_rds(here("data/raw-data/ZA-border/GADM_2.8_ZAF_adm0.rds"))

my_palette <- rev(viridis::viridis(10)) #rev(terrain.colors(10))

# Richness maps ----------------------------------------------------------------

richness_lims <- range(
  c(SWAFR_HDS_richness[], GCFR_HDS_richness[]),
  na.rm = TRUE
)
richness_lims[[2]] <- richness_lims[[2]] + 250

GCFR_richness_plot <- gplot(GCFR_HDS_richness) +
  geom_tile(aes(fill = value)) +
  labs(
    title = "GCFR",
    y     = "Latitude (º)"
  ) +
  annotate("text", x = 17, y = -26, label = "(a)", hjust = 1, vjust = -0.8) +
  scale_x_continuous(breaks = c(18, 22, 26)) +#, limits = c(16, 28)) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25.5)) +
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
  ggtitle("SWAFR") +
  annotate("text", x = 113, y = -26, label = "(b)", vjust = -0.8) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25.5)) +
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
  ylab("Latitude (º)") +
  annotate("text", x = 17, y = -26, label = "(c)", hjust = 1, vjust = -0.8) +
  scale_x_continuous(breaks = c(18, 22, 26)) +#, limits = c(16, 28)) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25.5)) +
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
  annotate("text", x = 113, y = -26, label = "(d)", vjust = -0.8) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25.5)) +
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
  ylab("Latitude (º)") +
  annotate("text", x = 17, y = -26, label = "(e)", hjust = 1, vjust = -0.8) +
  scale_x_continuous(breaks = c(18, 22, 26)) +#, limits = c(16, 28)) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25.5)) +
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
  annotate("text", x = 113, y = -26, label = "(f)", vjust = -0.8) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25.5)) +
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
  labs(
    x = "Longitude (º)",
    y = "Latitude (º)"
  ) +
  annotate("text", x = 17, y = -26, label = "(g)", hjust = 1, vjust = -0.8) +
  scale_x_continuous(breaks = c(18, 22, 26)) +#, limits = c(16, 28)) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25.5)) +
  scale_fill_gradientn(
    limits   = residuals_lims,
    colours  = my_palette,
    na.value = NA
  ) +
  theme(legend.position = "none")
SWAFR_mresiduals_plot <- gplot(SWAFR_multivariate_residuals) +
  geom_tile(aes(fill = value)) +
  xlab("Longitude (º)") +
  annotate("text", x = 113, y = -26, label = "(h)", vjust = -0.8) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25.5)) +
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
  here("draft-02/manuscript_ver3/figures/maps.pdf"),
  all_maps,
  width = 7, height = 12
)
ggsave(
  here("draft-02/manuscript_ver3/figures/maps.png"),
  all_maps, dpi = 600,
  width = 7, height = 12
)
