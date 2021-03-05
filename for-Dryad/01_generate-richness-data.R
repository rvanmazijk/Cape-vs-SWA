# Heterogeneity and species richness:
#   Generating vascular plant species richness data
# Ruan van Mazijk, <ruanvmazijk@gmail.com>
# CC-BY-4.0 2021

# Some more data cleaning ======================================================

# Add grid codes to species data
species_occ$EDS <- species_occ %over%
  Larsen_grid_EDS %>%
  pull(edgc) %>%
  as.character()
# Remove final letter of each code to make the code one "level" greater
species_occ$QDS <- str_remove(species_occ$EDS, ".$")
species_occ$HDS <- str_remove(species_occ$QDS, ".$")
species_occ$DS  <- str_remove(species_occ$HDS, ".$")

# Export final lists of species
GCFR_species_occ@data %>%
  group_by(species)  %>%
  summarise(n_collections = n()) %>%
  arrange(desc(n_collections)) %>%
  write_csv("cleaned-species-list_GCFR.csv")
SWAFR_species_occ@data %>%
  group_by(species) %>%
  summarise(n_collections = n()) %>%
  arrange(desc(n_collections)) %>%
  write_csv("cleaned-species-list_SWAFR.csv")

# Flag species w/ < 5 collections total in each region
GCFR_bad_species <-
  read_csv("cleaned-species-list_GCFR.csv") %>%
  filter(n_collections < 5) %>%
  pull(species)
SWAFR_bad_species <-
  read_csv("cleaned-species-list_SWAFR.csv") %>%
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
  species_occ2, "species_occ2",
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
# Result:
## # A tibble: 2 x 2
##   region n_spp
##   <chr>  <int>
## 1 GCFR    9489
## 2 SWAFR   6757

# Collate species occurrences into richness, turnover data =====================
# (by grid-cell codes)

# .... At QDS-scale ------------------------------------------------------------

QDS_richness <- species_occ_data %>%
  filter(QDS %in% QDS_w_all_EDS) %>%
  distinct() %>%  # just in case
  group_by(QDS) %>%
  summarise(
    n_collections = length(species),
    QDS_richness  = length(unique(species))
  )

# .... At HDS-scale ------------------------------------------------------------

mean_QDS_richness <- QDS_richness %>%
  mutate(HDS = str_remove(QDS, ".$")) %>%
  group_by(HDS) %>%
  summarise(mean_QDS_richness = mean(QDS_richness, na.rm = TRUE))

HDS_richness <- species_occ_data %>%
  group_by(HDS) %>%
  filter(HDS %in% HDS_w_all_QDS) %>%
  summarise(
    n_collections = length(species),
    HDS_richness  = length(unique(species))
  ) %>%
  full_join(mean_QDS_richness) %>%
  filter(HDS %in% HDS_w_all_QDS)  # (again because `mean_QDS_richness` introduces
                                  # some extra cells with less than 4 sub-cells)

# .... At DS-scale -------------------------------------------------------------

mean_HDS_richness <- HDS_richness %>%
  mutate(DS = str_remove(HDS, ".$")) %>%
  group_by(DS) %>%
  summarise(mean_HDS_richness = mean(HDS_richness, na.rm = TRUE))

DS_richness <- species_occ_data %>%
  group_by(DS) %>%
  filter(DS %in% DS_w_all_HDS) %>%
  summarise(
    n_collections = length(species),
    DS_richness   = length(unique(species))
  ) %>%
  full_join(mean_HDS_richness) %>%
  filter(DS %in% DS_w_all_HDS)  # (again because `mean_HDS_richness` introduces
                                # some extra cells with less than 4 sub-cells)

# Merge richness dataframes with extra grid-cell data ==========================

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
  # (because don't have dataframe of Larsen cells at DS-scale)
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

# Save richness dataframes to disc =============================================

QDS_richness_data %>%
  dplyr::select(
    region, qdgc, hdgc, dgc, lat, lon,
    n_collections, QDS_richness
  ) %>%
  write_csv("species-richness_QDS.csv")

HDS_richness_data %>%
  dplyr::select(
    region, hdgc, dgc, lat, lon,
    n_collections, HDS_richness, mean_QDS_richness
  ) %>%
  write_csv("species-richness_HDS.csv")

DS_richness_data %>%
  dplyr::select(
    region, dgc, lat, lon,
    n_collections, DS_richness, mean_HDS_richness
  ) %>%
  write_csv("species-richness_DS.csv")

# Rasterise richness dataframes ================================================

QDS_richness_ras      <- rasterise_data(QDS_richness_data, "QDS_richness",      Larsen_grid_QDS_ras)
mean_QDS_richness_ras <- rasterise_data(HDS_richness_data, "mean_QDS_richness", Larsen_grid_HDS_ras)
HDS_richness_ras      <- rasterise_data(HDS_richness_data, "HDS_richness",      Larsen_grid_HDS_ras)
mean_HDS_richness_ras <- rasterise_data(DS_richness_data,  "mean_HDS_richness", Larsen_grid_DS_ras)
DS_richness_ras       <- rasterise_data(DS_richness_data,  "DS_richness",       Larsen_grid_DS_ras)

# Save richness rasters to disc ================================================

writeRaster(QDS_richness_ras,      "species-richness_QDS.tif",  overwrite = TRUE)
writeRaster(mean_QDS_richness_ras, "mean-QDS-richness_HDS.tif", overwrite = TRUE)
writeRaster(HDS_richness_ras,      "species-richness_HDS.tif",  overwrite = TRUE)
writeRaster(mean_HDS_richness_ras, "mean-HDS-richness_DS.tif",  overwrite = TRUE)
writeRaster(DS_richness_ras,       "species-richness_DS.tif",   overwrite = TRUE)
