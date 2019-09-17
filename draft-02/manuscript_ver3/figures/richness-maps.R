# Richness maps

GCFR_QDS_richness  <- raster(glue("{data_dir}/GCFR_QDS_richness.tif"))
GCFR_HDS_richness  <- raster(glue("{data_dir}/GCFR_HDS_richness.tif"))
GCFR_DS_richness   <- raster(glue("{data_dir}/GCFR_DS_richness.tif"))
SWAFR_QDS_richness <- raster(glue("{data_dir}/SWAFR_QDS_richness.tif"))
SWAFR_HDS_richness <- raster(glue("{data_dir}/SWAFR_HDS_richness.tif"))
SWAFR_DS_richness  <- raster(glue("{data_dir}/SWAFR_DS_richness.tif"))

ZA_border <- read_rds(here("data/raw-data/ZA-border/GADM_2.8_ZAF_adm0.rds"))
ZA_border_dissolved <- buffer(ZA_border, width = 0.1, dissolve = TRUE)

GCFR_border <- readOGR(here("data/derived-data/borders/GCFR_border"))
GCFR_border_dissolved <- buffer(GCFR_border, width = 0, dissolve = TRUE)

AU_border <- read_rds(here("data/raw-data/ZA-border/GADM_2.8_ZAF_adm0.rds"))

richness_lims <- range(
  c(SWAFR_HDS_richness[], GCFR_HDS_richness[]),
  na.rm = TRUE
)
richness_lims[[2]] <- richness_lims[[2]] + 250

GCFR_richness_plot <- gplot(GCFR_HDS_richness) +
  geom_tile(aes(fill = value)) +
  # FIXME:
  #geom_path(data = GCFR_border_dissolved, aes(long, lat)) +
  labs(
    title = "(a) GCFR",
    x     = "Longitude (º)",
    y     = "Latitude (º)"
  ) +
  scale_x_continuous(breaks = c(18, 22, 26)) +#, limits = c(16, 28)) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25.5)) +
  scale_fill_gradientn(
    limits   = richness_lims,
    colours  = rev(terrain.colors(10)),
    na.value = NA
  ) +
  theme(legend.position = "none")
SWAFR_richness_plot <- gplot(SWAFR_HDS_richness) +
  geom_tile(aes(fill = value)) +
  labs(
    title = "(b) SWAFR",
    x     = "Longitude (º)"
  ) +
  scale_y_continuous(breaks = c(-34, -30, -26), limits = c(-35.5, -25.5)) +
  scale_fill_gradientn(
    name     = bquote(italic("S")["HDS"]),
    limits   = richness_lims,
    colours  = rev(terrain.colors(10)),
    na.value = NA
  ) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank(),
  )
richness_map <- plot_grid(
  GCFR_richness_plot, SWAFR_richness_plot,
  nrow = 1, rel_widths = c(0.75, 1)
)

ggsave(
  here("draft-02/manuscript_ver3/figures/richness-maps.pdf"),
  richness_map,
  width = 7, height = 3
)
ggsave(
  here("draft-02/manuscript_ver3/figures/richness-maps.png"),
  richness_map, dpi = 600,
  width = 7, height = 3
)
