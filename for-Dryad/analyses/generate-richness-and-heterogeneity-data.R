#####

Larsen_grid_EDS2 <- Larsen_grid_EDS
for (variable in names(enviro_data)) {
  Larsen_grid_EDS2[[variable]] <- extract(
    enviro_data[[variable]],
    Larsen_grid_EDS2,
    fun = mean, na.rm = TRUE
  )
  message(variable, " done")
}
save(Larsen_grid_EDS2, file = "Larsen_grid_EDS2")

#####

# 2020-02-10

GCFR_heterogeneity_QDS1 <- var_names %>%
  str_replace(" ", "_") %>%
  {glue("for-Dryad/raster-layers/GCFR_heterogeneity_{.}_QDS.tif")} %>%
  map(raster) %>%
  stack()

load(file = "Larsen_grid_EDS2")
GCFR_heterogeneity_QDS2 <- Larsen_grid_EDS2@data %>%
  as_tibble() %>%
  filter(region == "GCFR") %>%
  group_by(qdgc) %>%
  dplyr::select(Elevation:pH) %>%
  summarise_if(is.numeric, var) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_if(is.numeric, log10) %>%
  mutate_if(is.numeric, scale)

GCFR_heterogeneity_QDS1 %>%
  as.data.frame() %>%
  map_dfc(log10) %>%
  map_dfc(scale) %>%
  gather(var, val) %>%
  filter(!is.na(val)) %>%
  ggplot() +
    aes(var, val) +
    geom_boxplot() +
    coord_flip()

GCFR_heterogeneity_QDS2 %>%
  gather(var, val, -qdgc) %>%
  ggplot() +
    aes(var, val) +
    geom_boxplot() +
    coord_flip()

#####

heterogeneity_QDS2 <- Larsen_grid_EDS2@data %>%
  as_tibble() %>%
  group_by(region, qdgc) %>%
  dplyr::select(Elevation:pH) %>%
  summarise_if(is.numeric, var) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_if(is.numeric, log10) %>%
  mutate_if(is.numeric, scale)

heterogeneity_PCA <- heterogeneity_QDS2 %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

summary(heterogeneity_PCA)

heterogeneity_QDS2$PC1 <- heterogeneity_PCA$x[, 1]
heterogeneity_QDS2$PC2 <- heterogeneity_PCA$x[, 2]

ggplot(heterogeneity_QDS2) +
  aes(PC1, PC2, colour = region) +
  geom_point()

#####

plot(GCFR_heterogeneity_QDS1$GCFR_heterogeneity_Elevation_QDS)
points(xyFromCell(GCFR_heterogeneity_QDS1, 1:1232), pch = 3)
plot(border = "red", add = TRUE, Larsen_grid_QDS[
  Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS &
  Larsen_grid_QDS$region == "GCFR",
])
