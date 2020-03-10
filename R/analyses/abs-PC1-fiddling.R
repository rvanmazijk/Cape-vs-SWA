# Resample environmental data to EDS -------------------------------------------

enviro_data_EDS <- resample(
  enviro_data, Larsen_grid_EDS_ras,
  method = "bilinear"
)
enviro_data_QDS <- resample(
  enviro_data, Larsen_grid_QDS_ras,
  method = "bilinear"
)
enviro_data_HDS <- resample(
  enviro_data, Larsen_grid_HDS_ras,
  method = "bilinear"
)

# Merge environmental data from rasters into dataframes ------------------------

enviro_data_0.05_df <- enviro_data %>%
  raster2df() %>%
  na.exclude() %>%
  mutate(region = ifelse(lon > 60, "SWAFR", "GCFR")) %>%
  dplyr::select(region, lon, lat, Elevation:pH)
enviro_data_EDS_df <- enviro_data_EDS %>%
  raster2df(Larsen_grid_EDS_data) %>%
  na.exclude() %>%
  mutate(region = ifelse(lon > 60, "SWAFR", "GCFR")) %>%
  dplyr::select(region, lon, lat, Elevation:pH)
enviro_data_QDS_df <- enviro_data_QDS %>%
  raster2df(Larsen_grid_QDS_data) %>%
  na.exclude() %>%
  mutate(region = ifelse(lon > 60, "SWAFR", "GCFR"))
enviro_data_HDS_df <- enviro_data_HDS %>%
  raster2df(Larsen_grid_HDS_data) %>%
  na.exclude() %>%
  mutate(region = ifelse(lon > 60, "SWAFR", "GCFR"))

# Run PCA of absolute variables ------------------------------------------------

PCA_0.05 <- enviro_data_0.05_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

PCA_EDS <- enviro_data_EDS_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

PCA_QDS <- enviro_data_QDS_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

PCA_HDS <- enviro_data_HDS_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

summary(PCA_0.05)
summary(PCA_EDS)
summary(PCA_QDS)
summary(PCA_HDS)

autoplot(PCA_0.05, data = enviro_data_0.05_df, colour = "region",
    loadings = TRUE,
    loadings.colour = "black",
    loadings.label = TRUE,
    loadings.label.colour = "black",
    loadings.label.hjust = -0.25,
    loadings.label.size = 3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_colour_manual(name = "Region", values = c("grey30", "grey80")) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

autoplot(PCA_EDS, data = enviro_data_EDS_df, colour = "region",
    loadings = TRUE,
    loadings.colour = "black",
    loadings.label = TRUE,
    loadings.label.colour = "black",
    loadings.label.hjust = -0.25,
    loadings.label.size = 3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_colour_manual(name = "Region", values = c("grey30", "grey80")) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

autoplot(PCA_QDS, data = enviro_data_QDS_df, colour = "region",
    loadings = TRUE,
    loadings.colour = "black",
    loadings.label = TRUE,
    loadings.label.colour = "black",
    loadings.label.hjust = -0.25,
    loadings.label.size = 3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_colour_manual(name = "Region", values = c("grey30", "grey80")) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

autoplot(PCA_HDS, data = enviro_data_HDS_df, colour = "region",
    loadings = TRUE,
    loadings.colour = "black",
    loadings.label = TRUE,
    loadings.label.colour = "black",
    loadings.label.hjust = -0.25,
    loadings.label.size = 3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_colour_manual(name = "Region", values = c("grey30", "grey80")) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

enviro_data_QDS_df$PC1_abs <- PCA_QDS$x[, 1]
data_QDS <- read_csv(glue("{data_dir}/data-QDS-w-residuals.csv"))
data_QDS %<>%
  full_join(enviro_data_QDS_df, by = c(
    "region", "lat", "lon", "qdgc", "hdgc", "dgc"
  )) %>%
  na.exclude() %>%
  filter(hdgc %in% HDS_w_all_QDS) %>%
  filter(!is.na(region)) %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  group_by(region, hdgc) %>%
  summarise(
    lon          =       mean(lon,          na.rm = TRUE),
    lat          =       mean(lat,          na.rm = TRUE),
    QDS_richness = log10(mean(QDS_richness, na.rm = TRUE)),
    PC1_abs_het  = log10(var(PC1_abs,       na.rm = TRUE))
  ) %>%
  ungroup() %>%
  na.exclude()
ggplot(data_QDS) +
  aes(PC1_abs_het, QDS_richness, colour = region) +
  geom_point()
library(nlme)
m <- gls(
  QDS_richness ~ PC1_abs_het,
  data_QDS,
  corGaus(form = ~ lon + lat | region)
)
summary(m)
visreg::visreg(m)
plot(residuals(m) ~ data_QDS$QDS_richness)
