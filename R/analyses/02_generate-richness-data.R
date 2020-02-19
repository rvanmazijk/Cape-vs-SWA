# Import data ------------------------------------------------------------------

# .... My Larsen-type grid polygons and rasters --------------------------------

Larsen_grid_EDS <- readOGR(
  glue("{data_dir}/Larsen_grid_EDS"),
  layer = "Larsen_grid_EDS"
)
Larsen_grid_QDS <- readOGR(
  glue("{data_dir}/Larsen_grid_QDS"),
  layer = "Larsen_grid_QDS"
)
Larsen_grid_HDS <- readOGR(
  glue("{data_dir}/Larsen_grid_HDS"),
  layer = "Larsen_grid_HDS"
)

Larsen_grid_EDS_ras <- raster(glue(
  "{data_dir}/raster-layers/",
  "Larsen_grid_EDS_ras.tif"
))
Larsen_grid_QDS_ras <- raster(glue(
  "{data_dir}/raster-layers/",
  "Larsen_grid_QDS_ras.tif"
))
Larsen_grid_HDS_ras <- raster(glue(
  "{data_dir}/raster-layers/",
  "Larsen_grid_HDS_ras.tif"
))
Larsen_grid_DS_ras <- raster(glue(
  "{data_dir}/raster-layers/",
  "Larsen_grid_DS_ras.tif"
))

# .... Region polygons ---------------------------------------------------------

GCFR_border_buffered <- readOGR(here(
  "data/derived-data/borders",
  "GCFR_border_buffered/"
))
SWAFR_border_buffered <- readOGR(here(
  "data/derived-data/borders",
  "SWAFR_border_buffered/"
))

# Merge regions' borders
borders_buffered <- rbind(GCFR_border_buffered, SWAFR_border_buffered)

# .... GBIF species occurrence datasets ----------------------------------------

GCFR_species_occ <- make_SpatialPointsDataFrame(read_csv(here(
  "data/derived-data/flora",
  "GCFR_clean_flora_2017-09-14.csv"
)))
SWAFR_species_occ <- make_SpatialPointsDataFrame(read_csv(here(
  "data/derived-data/flora",
  "SWAFR_clean_flora_2017-09-14.csv"
)))

# Merge regions' data
species_occ <- rbind(GCFR_species_occ, SWAFR_species_occ)

# Detemine which DS, HDS & QDS have all 4 of their HDS, QDS & EDS --------------
# (within the regions' borders)

# Query the border polygon and store results in Larsen grid
Larsen_grid_EDS@data <- cbind(
  Larsen_grid_EDS@data,
  Larsen_grid_EDS %over% borders_buffered
)
# For larger cells, just use longitude for region classification,
# because later I filter the cells based on whether their constituent cells
# are in the regions (from EDS above)
Larsen_grid_QDS@data$region <-
  ifelse(Larsen_grid_QDS@data$lon > 60,
    "SWAFR", "GCFR"
  )
Larsen_grid_HDS@data$region <-
  ifelse(Larsen_grid_HDS@data$lon > 60,
    "SWAFR", "GCFR"
  )

# Filter to EDS that are within the regions' borders
Larsen_grid_EDS <- Larsen_grid_EDS[!is.na(Larsen_grid_EDS$region), ]

# Make grids tibbles for easier wrangling
Larsen_grid_EDS_data <- as_tibble(Larsen_grid_EDS@data)
Larsen_grid_QDS_data <- as_tibble(Larsen_grid_QDS@data)
Larsen_grid_HDS_data <- as_tibble(Larsen_grid_HDS@data)

# Pull out QDS-codes of QDS with all 4 EDS (within borders)
QDS_w_all_EDS <- Larsen_grid_EDS_data %>%
  group_by(qdgc, region) %>%
  dplyr::select(edgc) %>%
  distinct() %>%  # just in case
  summarise(n_EDS = n()) %>%
  filter(n_EDS == 4) %>%
  pull(qdgc)

# Pull out HDS-codes of HDS with all 4 HDS (within borders)
HDS_w_all_QDS <- Larsen_grid_QDS_data %>%
  group_by(hdgc, region) %>%
  dplyr::select(qdgc) %>%
  distinct() %>%
  filter(qdgc %in% QDS_w_all_EDS) %>%
  summarise(n_QDS = n()) %>%
  filter(n_QDS == 4) %>%
  pull(hdgc)

# Pull out DS-codes of DS with all 4 DS (within borders)
DS_w_all_HDS <- Larsen_grid_HDS_data %>%
  group_by(dgc, region) %>%
  dplyr::select(hdgc) %>%
  distinct() %>%
  filter(hdgc %in% HDS_w_all_QDS) %>%
  summarise(n_HDS = n()) %>%
  filter(n_HDS == 4) %>%
  pull(dgc)

# Collate species occurrences into richness, turnover data ---------------------
# (by grid-cell codes)

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
  write_csv(glue("{data_dir}/GCFR-species.csv"))
SWAFR_species_occ@data %>%
  group_by(species) %>%
  summarise(n_collections = n()) %>%
  arrange(desc(n_collections)) %>%
  write_csv(glue("{data_dir}/SWAFR-species.csv"))

# Flag species w/ < 5 collections total in each region
GCFR_bad_species <-
  read_csv(glue("{data_dir}/GCFR-species.csv")) %>%
  filter(n_collections < 5) %>%
  pull(species)
SWAFR_bad_species <-
  read_csv(glue("{data_dir}/SWAFR-species.csv")) %>%
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
  full_join(mean_QDS_richness) %>%
  filter(HDS %in% HDS_w_all_QDS)  # (again because `mean_QDS_richness` introduces
                                # some extra cells with less than 4 sub-cells)
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
  full_join(mean_HDS_richness) %>%
  filter(DS %in% DS_w_all_HDS)  # (again because `mean_HDS_richness` introduces
                                # some extra cells with less than 4 sub-cells)

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

# Merge richness dataframes with extra grid-cell data --------------------------

QDS_richness_data <- QDS_richness %>%
  rename(qdgc = QDS) %>%
  full_join(Larsen_grid_QDS_data)
HDS_richness_data <- HDS_richness %>%
  rename(hdgc = HDS) %>%
  full_join(Larsen_grid_HDS_data)
DS_richness_data <- DS_richness %>%
  rename(dgc = DS) %>%
  full_join(Larsen_grid_HDS_data) %>%
  dplyr::select(region, dgc, n_collections, DS_richness, mean_HDS_richness) %>%
  distinct() %>%
  # Make DS-cell midpoints manually
  mutate(
    lon = dgc %>%
      str_extract("E[0-9]{3}") %>%
      str_remove("E") %>%
      as.numeric() %>%
      add(0.5),
    lat = dgc %>%
      str_extract("S[0-9]{2}") %>%
      str_remove("S") %>%
      as.numeric() %>%
      add(0.5) %>%
      multiply_by(-1)
  )

# Save richness dataframes to disc ---------------------------------------------

QDS_richness_data %>%
  dplyr::select(
    region, qdgc, hdgc, dgc, lat, lon,
    n_collections, QDS_richness
  ) %>%
  write_csv(glue("{data_dir}/richness-data-QDS.csv"))

HDS_richness_data %>%
  dplyr::select(
    region, hdgc, dgc, lat, lon,
    n_collections, HDS_richness, mean_QDS_richness
  ) %>%
  write_csv(glue("{data_dir}/richness-data-HDS.csv"))

DS_richness_data %>%
  dplyr::select(
    region, dgc, lat, lon,
    n_collections, DS_richness, mean_HDS_richness
  ) %>%
  write_csv(glue("{data_dir}/richness-data-DS.csv"))

# Rasterise richness dataframes ------------------------------------------------

QDS_richness_ras <- rasterise_data(
  QDS_richness_data, "QDS_richness",
  Larsen_grid_QDS_ras
)
mean_QDS_richness_ras <- rasterise_data(
  HDS_richness_data, "mean_QDS_richness",
  Larsen_grid_HDS_ras
)
HDS_richness_ras <- rasterise_data(
  HDS_richness_data, "HDS_richness",
  Larsen_grid_HDS_ras
)
mean_HDS_richness_ras  <- rasterise_data(
  DS_richness_data,  "mean_HDS_richness",
  Larsen_grid_DS_ras
)
DS_richness_ras  <- rasterise_data(
  DS_richness_data,  "DS_richness",
  Larsen_grid_DS_ras
)

# Plot to check
if (FALSE) {
  plot(QDS_richness_ras)
  plot(mean_QDS_richness_ras)
  plot(HDS_richness_ras)
  plot(mean_HDS_richness_ras)
  plot(DS_richness_ras)
}

# Save richnes rasters to disc -------------------------------------------------

writeRaster(
  QDS_richness_ras,
  glue("{data_dir}/raster-layers/QDS-richness_QDS.tif")
)
writeRaster(
  mean_QDS_richness_ras,
  glue("{data_dir}/raster-layers/mean-QDS-richness_HDS.tif")
)
writeRaster(
  HDS_richness_ras,
  glue("{data_dir}/raster-layers/HDS-richness_HDS.tif")
)
writeRaster(
  mean_HDS_richness_ras,
  glue("{data_dir}/raster-layers/mean-HDS-richness_DS.tif")
)
writeRaster(
  DS_richness_ras,
  glue("{data_dir}/raster-layers/DS-richness_DS.tif")
)
