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
# Save shapefile of this too
species_occ2 <- species_occ[
  !(species_occ$species %in% c(GCFR_bad_species, SWAFR_bad_species)),
]
writeOGR(
  species_occ2,
  here("data/derived-data/Feb-2020/species_occ"),
  layer = "species",
  driver = "ESRI Shapefile"
)

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

# Determine proportion of regions sampled --------------------------------------

ZA_QDS <- readOGR(here("data/raw-data/QDGC/qdgc_zaf"), layer = "qdgc_02_zaf")
AU_QDS <- readOGR(here("data/raw-data/QDGC/qdgc_aus"), layer = "qdgc_02_aus")

ZA_QDS@data <- cbind(
  ZA_QDS@data,
  ZA_QDS %over% GCFR_border_buffered
)
AU_QDS@data <- cbind(
  AU_QDS@data,
  AU_QDS %over% SWAFR_border_buffered
)

GCFR_QDS  <- ZA_QDS[!is.na(ZA_QDS$region), ]
SWAFR_QDS <- AU_QDS[!is.na(AU_QDS$region), ]

par(mfrow = c(1, 2))
#plot(ZA_QDS)
plot(GCFR_QDS)
plot(GCFR_border_buffered, add = TRUE)
#plot(AU_QDS)
plot(SWAFR_QDS)
plot(SWAFR_border_buffered, add = TRUE)
par(op)

n_QDS_GCFR <- GCFR_QDS$qdgc %>%
  unique() %>%
  length()
n_QDS_SWAFR <- SWAFR_QDS$qdgc %>%
  unique() %>%
  length()

n_QDS_sampled <- QDS_richness_data %>%
  filter(qdgc %in% QDS_w_all_EDS) %>%   # same as na.exclude() for this, honestly
  split(.$region) %>%
  map(pull, qdgc) %>%
  map(unique) %>%
  map(length)

message(
  " GCFR: ", n_QDS_sampled$GCFR, "/", n_QDS_GCFR, " => ",
    round(100 * n_QGCF_QDS_sampled$GCFR / n_QDS_GCFR, digits = 2), "% QDS sampled\n",
  "SWAFR: ", n_QDS_sampled$SWAFR, "/", n_QDS_SWAFR, " => ",
    round(100 * n_QGCF_QDS_sampled$SWAFR / n_QDS_SWAFR, digits = 2), "% QDS sampled"
)
##  GCFR: 362/449 => 80.62% QDS sampled
## SWAFR: 624/737 => 84.67% QDS sampled

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
  glue("{data_dir}/raster-layers/QDS-richness_QDS.tif"),
  overwrite = TRUE
)
writeRaster(
  mean_QDS_richness_ras,
  glue("{data_dir}/raster-layers/mean-QDS-richness_HDS.tif"),
  overwrite = TRUE
)
writeRaster(
  HDS_richness_ras,
  glue("{data_dir}/raster-layers/HDS-richness_HDS.tif"),
  overwrite = TRUE
)
writeRaster(
  mean_HDS_richness_ras,
  glue("{data_dir}/raster-layers/mean-HDS-richness_DS.tif"),
  overwrite = TRUE
)
writeRaster(
  DS_richness_ras,
  glue("{data_dir}/raster-layers/DS-richness_DS.tif"),
  overwrite = TRUE
)
