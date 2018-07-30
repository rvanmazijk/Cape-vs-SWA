# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
map(pre_analysis_import_paths, source)
pacman::p_load(ggfortify)

# QDS --------------------------------------------------------------------------

GCFR_roughness_QDS <- map(GCFR_variables_QDS, focal_sd)
names(GCFR_roughness_QDS) %<>% paste0("_rough")
SWAFR_roughness_QDS <- map(SWAFR_variables_QDS, focal_sd)
names(SWAFR_roughness_QDS) %<>% paste0("_rough")
GCFR <-
  c(richness = mask(GCFR_richness_QDS, GCFR_border_buffered),
    GCFR_variables_QDS,
    GCFR_roughness_QDS) %>%
  map(getValues) %>%
  as_tibble()
SWAFR <-
  c(richness = mask(SWAFR_richness_QDS, SWAFR_border_buffered),
    SWAFR_variables_QDS,
    SWAFR_roughness_QDS) %>%
  map(getValue~ .[]) %>%
  as_tibble()
data <-
  rbind(cbind(region = "GCFR", GCFR),
        cbind(region = "SWAFR", SWAFR)) %>%
  as_tibble() %>%
  na.exclude() %>%
  # Adjust environmental values stored at x10 etc.
  mutate(`Surface T` = `Surface T` - 273.15,
         NDVI        = NDVI / 1e+07,
         NDVI_rough  = NDVI_rough / 1e+07,
         pH          = pH / 10,
         pH_rough    = pH_rough / 10)
data$region %<>% as.character()

pairs(data[, 3:11])
pairs(data[, 12:20])

variables_QDS_pca <- prcomp(data[, 3:11], scale = TRUE)
roughness_QDS_pca <- prcomp(data[, 12:20], scale = TRUE)

summary(roughness_QDS_pca)
summary(variables_QDS_pca)

autoplot(
  roughness_QDS_pca,
  data = data,
  colour = "richness", shape = "region",
  loadings = TRUE, loadings.label = TRUE
) + scale_color_viridis_c()
autoplot(
  variables_QDS_pca,
  data = data,
  colour = "richness", shape = "region",
  loadings = TRUE, loadings.label = TRUE
) + scale_color_viridis_c()

# 3QDS -------------------------------------------------------------------------

GCFR_roughness_3QDS <- map(GCFR_variables_3QDS, focal_sd)
names(GCFR_roughness_3QDS) %<>% paste0("_rough")
SWAFR_roughness_3QDS <- map(SWAFR_variables_3QDS, focal_sd)
names(SWAFR_roughness_3QDS) %<>% paste0("_rough")
GCFR <-
  c(richness = mask(GCFR_richness_3QDS, GCFR_border_buffered),
    GCFR_variables_3QDS,
    GCFR_roughness_3QDS) %>%
  map(~ .[]) %>%
  as_tibble()
SWAFR <-
  c(richness = mask(SWAFR_richness_3QDS, SWAFR_border_buffered),
    SWAFR_variables_3QDS,
    SWAFR_roughness_3QDS) %>%
  map(~ .[]) %>%
  as_tibble()
data <-
  rbind(cbind(region = "GCFR", GCFR),
        cbind(region = "SWAFR", SWAFR)) %>%
  as_tibble() %>%
  na.exclude() %>%
  # Adjust environmental values stored at x10 etc.
  mutate(`Surface T` = `Surface T` - 273.15,
         NDVI        = NDVI / 1e+07,
         NDVI_rough  = NDVI_rough / 1e+07,
         pH          = pH / 10,
         pH_rough    = pH_rough / 10)
data$region %<>% as.character()

pairs(data[, 3:11])
pairs(data[, 12:20])

variables_3QDS_pca <- prcomp(data[, 3:11], scale = TRUE)
roughness_3QDS_pca <- prcomp(data[, 12:20], scale = TRUE)

summary(roughness_3QDS_pca)
summary(variables_3QDS_pca)

autoplot(
  roughness_3QDS_pca,
  data = data,
  colour = "richness", shape = "region",
  loadings = TRUE, loadings.label = TRUE
) + scale_color_viridis_c()
autoplot(
  variables_3QDS_pca,
  data = data,
  colour = "richness", shape = "region",
  loadings = TRUE, loadings.label = TRUE
) + scale_color_viridis_c()
