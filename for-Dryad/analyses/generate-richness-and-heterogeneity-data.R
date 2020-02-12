# Heterogeneity and species richness: replicating Larsen et al. 2009 grids
# R. van Mazijk
# CC-BY-4.0 2019

# Import Larsen-grid rasters ---------------------------------------------------

Larsen_grid_EDS_ras <- raster(here(
  "data/derived-data/May-2019/Larsen_grid_EDS_ras.tif"
))
Larsen_grid_QDS_ras <- raster(here(
  "data/derived-data/May-2019/Larsen_grid_QDS_ras.tif"
))
Larsen_grid_HDS_ras <- raster(here(
  "data/derived-data/May-2019/Larsen_grid_HDS_ras.tif"
))

# Collate richness data into grids ---------------------------------------------

make_SpatialPointsDataFrame <- function(df) {
  SpatialPointsDataFrame(
    coords      = df[, c("decimallongitude", "decimallatitude")],
    data        = df[, "species"],
    proj4string = crs(borders_buffered)
  )
}
GCFR_species_occ <- make_SpatialPointsDataFrame(read_csv(here(
  "data/derived-data/flora",
  "GCFR_clean_flora_2017-09-14.csv"
)))
SWAFR_species_occ <- make_SpatialPointsDataFrame(read_csv(here(
  "data/derived-data/flora",
  "SWAFR_clean_flora_2017-09-14.csv"
)))
species_occ <- rbind(GCFR_species_occ, SWAFR_species_occ)

# Add grid codes to species data
species_occ$EDS <- species_occ %over%
  Larsen_grid_EDS %>%
  pull(edgc) %>%
  as.character()
species_occ$QDS <- str_remove(species_occ$EDS, ".$")
species_occ$HDS <- str_remove(species_occ$QDS, ".$")
species_occ$DS  <- str_remove(species_occ$HDS, ".$")

# Export final lists of species
GCFR_species_occ@data %>%
  group_by(species)  %>%
  summarise(n_collections = n()) %>%
  arrange(desc(n_collections)) %>%
  write_csv(here("for-Dryad/GCFR-species.csv"))
SWAFR_species_occ@data %>%
  group_by(species) %>%
  summarise(n_collections = n()) %>%
  arrange(desc(n_collections)) %>%
  write_csv(here("for-Dryad/SWAFR-species.csv"))

# Flag species w/ < 5 collections total in each region
GCFR_bad_species <-
  read_csv(here("for-Dryad/GCFR-species.csv")) %>%
  filter(n_collections < 5) %>%
  pull(species)
SWAFR_bad_species <-
  read_csv(here("for-Dryad/SWAFR-species.csv")) %>%
  filter(n_collections < 5) %>%
  pull(species)
# Filter them out
species_occ_data <- species_occ@data %>%
  filter(!(species %in% c(GCFR_bad_species, SWAFR_bad_species))) %>%
  na.exclude()

# Check species counts now:
species_occ_data %>%
  mutate(region = EDS %>%
    str_extract("E[0-9]{3}") %>%
    str_remove("E") %>%
    as.numeric() %>%
    is_greater_than(60) %>%
    ifelse("SWAFR", "GCFR")
  ) %>%
  group_by(region) %>%
  summarise(n_spp = length(unique(species)))
## # A tibble: 2 x 2
##   region n_spp
##   <chr>  <int>
## 1 GCFR    9489
## 2 SWAFR   6757

# Collate richness measures at each scale
# At QDS-scale:
QDS_richness <- species_occ_data %>%
  filter(QDS %in% QDS_w_all_EDS) %>%
  distinct() %>%  # just in case
  group_by(QDS) %>%
  summarise(
    n_collections = length(species),
    QDS_richness  = length(unique(species))
  )
# At HDS-scale:
mean_QDS_richness <- QDS_richness %>%
  mutate(HDS = str_remove(QDS, ".$")) %>%
  group_by(HDS) %>%
  summarise(mean_QDS_richness = mean(QDS_richness, na.rm = TRUE))
HDS_richness <- species_occ_data %>%
  group_by(HDS) %>%
  filter(HDS %in% HDS_w_all_QDS) %>%
  summarise(
    n_collections   = length(species),
    HDS_richness    = length(unique(species))
  ) %>%
  full_join(mean_QDS_richness)
# At DS-scale:
mean_HDS_richness <- HDS_richness %>%
  mutate(DS = str_remove(HDS, ".$")) %>%
  group_by(DS) %>%
  summarise(mean_HDS_richness = mean(HDS_richness, na.rm = TRUE))
DS_richness <- species_occ_data %>%
  group_by(DS) %>%
  filter(DS %in% DS_w_all_HDS) %>%
  summarise(
    n_collections   = length(species),
    DS_richness     = length(unique(species))
  ) %>%
  full_join(mean_HDS_richness)

# Plot to check
if (FALSE) {
  ggplot(DS_richness) +
    aes(mean_HDS_richness, DS_richness - mean_HDS_richness) +
    geom_point() +
    lims(x = c(0, 3000), y = c(0, 3000))
  ggplot(HDS_richness) +
    aes(mean_QDS_richness, HDS_richness - mean_QDS_richness) +
    geom_point() +
    lims(x = c(0, 3000), y = c(0, 3000))
}

# Import environmental data ----------------------------------------------------

GCFR_file_names  <- glue("{data_dir}/GCFR_{var_names}_masked2.tif")
SWAFR_file_names <- glue("{data_dir}/SWAFR_{var_names}_masked2.tif")

GCFR_variables  <- stack(GCFR_file_names)
SWAFR_variables <- stack(SWAFR_file_names)

enviro_data <- raster::merge(GCFR_variables, SWAFR_variables)
names(enviro_data)  <- str_replace_all(var_names, " ", "_")

# Resample environmental data to EDS -------------------------------------------

enviro_data_EDS <- resample(
  enviro_data, Larsen_grid_EDS_ras
)
enviro_data_QDS <- resample(
  enviro_data, Larsen_grid_QDS_ras
)
enviro_data_HDS <- resample(
  enviro_data, Larsen_grid_HDS_ras
)

# Merge environmental data from rasters into dataframes ------------------------

enviro_data_EDS_df <- raster2df(enviro_data_EDS, Larsen_grid_EDS_data)
enviro_data_QDS_df <- raster2df(enviro_data_QDS, Larsen_grid_QDS_data)
enviro_data_HDS_df <- raster2df(enviro_data_HDS, Larsen_grid_HDS_data)

# Calculate heterogeneity within QDS, HDS, DS ----------------------------------

heterogeneity_QDS_df <- enviro_data_EDS_df %>%
  filter(qdgc %in% QDS_w_all_EDS) %>%
  filter(!is.na(region)) %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  group_by(region, qdgc) %>%
  summarise_at(
    vars(str_replace(var_names, " ", "_")),
    ~var(., na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_at(vars(str_replace(var_names, " ", "_")), log10) %>%
  mutate_at(vars(str_replace(var_names, " ", "_")), scale)

heterogeneity_HDS_df <- enviro_data_QDS_df %>%
  filter(hdgc %in% HDS_w_all_QDS) %>%
  filter(!is.na(region)) %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  group_by(region, hdgc) %>%
  summarise_at(
    vars(str_replace(var_names, " ", "_")),
    ~var(., na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_at(vars(str_replace(var_names, " ", "_")), log10) %>%
  mutate_at(vars(str_replace(var_names, " ", "_")), scale)

heterogeneity_DS_df <- enviro_data_HDS_df %>%
  filter(dgc %in% DS_w_all_HDS) %>%
  filter(!is.na(region)) %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  group_by(region, dgc) %>%
  summarise_at(
    vars(str_replace(var_names, " ", "_")),
    ~var(., na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter_if(is.numeric, ~ (!is.nan(.)) & (!is.na(.))) %>%
  mutate_at(vars(str_replace(var_names, " ", "_")), log10) %>%
  mutate_at(vars(str_replace(var_names, " ", "_")), scale)

# Run PCA of heterogeneity -----------------------------------------------------

heterogeneity_QDS_PCA <- heterogeneity_QDS_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

heterogeneity_HDS_PCA <- heterogeneity_HDS_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

heterogeneity_DS_PCA <- heterogeneity_DS_df %>%
  dplyr::select(Elevation:pH) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  force_positive_PC1()

summary(heterogeneity_QDS_PCA)
summary(heterogeneity_HDS_PCA)
summary(heterogeneity_DS_PCA)

heterogeneity_QDS_df$PC1 <- heterogeneity_QDS_PCA$x[, 1]
heterogeneity_QDS_df$PC2 <- heterogeneity_QDS_PCA$x[, 2]

heterogeneity_HDS_df$PC1 <- heterogeneity_HDS_PCA$x[, 1]
heterogeneity_HDS_df$PC2 <- heterogeneity_HDS_PCA$x[, 2]

heterogeneity_DS_df$PC1 <- heterogeneity_DS_PCA$x[, 1]
heterogeneity_DS_df$PC2 <- heterogeneity_DS_PCA$x[, 2]

# Merge richness data into dataframes ------------------------------------------

# Plot to check
if (FALSE) {
  QDS_richness %>%
    rename(qdgc = QDS) %>%
    full_join(heterogeneity_QDS_df) %>%
    filter_all(~ (!is.nan(.)) & (!is.na(.))) %>%
    ggplot() +
      aes(PC1, QDS_richness, colour = region) +
      geom_point()
  HDS_richness %>%
    rename(hdgc = HDS) %>%
    full_join(heterogeneity_HDS_df) %>%
    filter_all(~ (!is.nan(.)) & (!is.na(.))) %>%
    ggplot() +
      aes(PC1, HDS_richness, colour = region) +
      geom_point()
  DS_richness %>%
    rename(dgc = DS) %>%
    full_join(heterogeneity_DS_df) %>%
    filter_all(~ (!is.nan(.)) & (!is.na(.))) %>%
    ggplot() +
      aes(PC1, DS_richness, colour = region) +
      geom_point()
  # YAY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
}

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
