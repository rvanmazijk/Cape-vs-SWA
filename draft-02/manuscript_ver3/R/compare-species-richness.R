# Import region polygons -------------------------------------------------------

GCFR_border  <- readOGR(here("data/derived-data/borders/GCFR_border"))
SWAFR_border <- readOGR(here("data/derived-data/borders/SWBP_Mike-Cramer"))

GCFR_border_buffered <-
  readOGR(here("data/derived-data/borders/GCFR_border_buffered/"))
SWAFR_border_buffered <-
  readOGR(here("data/derived-data/borders/SWAFR_border_buffered/"))

GCFR_box  <- readOGR(here("data/derived-data/borders/GCFR_box"))
SWAFR_box <- readOGR(here("data/derived-data/borders/SWAFR_box"))

# Import Larsen grid polygons
ZA_EDS <- readOGR(here("data/raw-data/QDGC/qdgc_zaf"), layer = "qdgc_03_zaf")
AU_EDS <- readOGR(here("data/raw-data/QDGC/qdgc_aus"), layer = "qdgc_03_aus")
ZA_QDS <- readOGR(here("data/raw-data/QDGC/qdgc_zaf"), layer = "qdgc_02_zaf")
AU_QDS <- readOGR(here("data/raw-data/QDGC/qdgc_aus"), layer = "qdgc_02_aus")
ZA_HDS <- readOGR(here("data/raw-data/QDGC/qdgc_zaf"), layer = "qdgc_01_zaf")
AU_HDS <- readOGR(here("data/raw-data/QDGC/qdgc_aus"), layer = "qdgc_01_aus")

# Crop to regions
#GCFR_EDS  <- crop(ZA_EDS, GCFR_box)
#SWAFR_EDS <- crop(AU_EDS, SWAFR_box)

# Get DS, HDS, QDS codes from EDS codes
Larsen_grid <- rbind(GCFR_EDS, SWAFR_EDS)
Larsen_grid$edgc <- Larsen_grid$qdgc
Larsen_grid$qdgc <- str_remove(Larsen_grid$edgc, ".$")
Larsen_grid$hdgc <- str_remove(Larsen_grid$qdgc, ".$")
Larsen_grid$dgc  <- str_remove(Larsen_grid$hdgc, ".$")

# ... --------------------------------------------------------------------------

# .... QDS-scale ---------------------------------------------------------------

# Test
ZA_EDS@data %<>% cbind(ZA_EDS %over% GCFR_border_buffered)
ZA_QDS@data %<>% cbind(ZA_QDS %over% GCFR_border_buffered)

ZA_EDS@data$region %<>% {!is.na(.)}
ZA_QDS@data$region %<>% {!is.na(.)}

GCFR_EDS <- ZA_EDS[ZA_EDS$region, ]
GCFR_QDS <- ZA_QDS[ZA_QDS$region, ]

GCFR_QDS_EDS <- intersect(GCFR_QDS, GCFR_EDS)

GCFR_QDS_w_all_EDS <- GCFR_QDS_EDS@data %>%
  group_by(qdgc.1) %>%
  summarise(n_EDS = n()) %>%
  filter(n_EDS == 4) %>%
  pull(qdgc.1) %>%
  as.character()

# Check
plot(GCFR_EDS, lwd = 2)
plot(
  ZA_QDS[ZA_QDS$qdgc %in% GCFR_QDS_w_all_EDS, ],
  border = "green", add = TRUE
)
# Works!

# Do rest
AU_EDS@data %<>% cbind(AU_EDS %over% SWAFR_border_buffered)
AU_QDS@data %<>% cbind(AU_QDS %over% SWAFR_border_buffered)

AU_EDS@data$region %<>% {!is.na(.)}
AU_QDS@data$region %<>% {!is.na(.)}

SWAFR_EDS <- AU_EDS[AU_EDS$region, ]
SWAFR_QDS <- AU_QDS[AU_QDS$region, ]

SWAFR_QDS_EDS <- intersect(SWAFR_QDS, SWAFR_EDS)

SWAFR_QDS_w_all_EDS <- SWAFR_QDS_EDS@data %>%
  group_by(qdgc.1) %>%
  summarise(n_EDS = n()) %>%
  filter(n_EDS == 4) %>%
  pull(qdgc.1) %>%
  as.character()

# .... HDS-scale ---------------------------------------------------------------

ZA_HDS@data %<>% cbind(ZA_HDS %over% GCFR_border_buffered)

ZA_HDS@data$region %<>% {!is.na(.)}

GCFR_HDS <- ZA_HDS[ZA_HDS$region, ]

GCFR_HDS_QDS <- intersect(GCFR_HDS, GCFR_QDS)

GCFR_HDS_w_all_QDS <- GCFR_HDS_QDS@data %>%
  group_by(qdgc.1) %>%
  summarise(n_QDS = n()) %>%
  filter(n_QDS == 4) %>%
  pull(qdgc.1) %>%
  as.character()

# Check
plot(GCFR_QDS, lwd = 2)
plot(
  ZA_HDS[ZA_HDS$qdgc %in% GCFR_HDS_w_all_QDS, ],
  border = "green", add = TRUE
)
# Works!

# Do rest
AU_EDS@data %<>% cbind(AU_EDS %over% SWAFR_border_buffered)
AU_QDS@data %<>% cbind(AU_QDS %over% SWAFR_border_buffered)

AU_EDS@data$region %<>% {!is.na(.)}
AU_QDS@data$region %<>% {!is.na(.)}

SWAFR_EDS <- AU_EDS[AU_EDS$region, ]
SWAFR_QDS <- AU_QDS[AU_QDS$region, ]

SWAFR_QDS_EDS <- intersect(SWAFR_QDS, SWAFR_EDS)

SWAFR_QDS_w_all_EDS <- SWAFR_QDS_EDS@data %>%
  group_by(qdgc.1) %>%
  summarise(n_EDS = n()) %>%
  filter(n_EDS == 4) %>%
  pull(qdgc.1) %>%
  as.character()




## Collate heterogeneity data into grids ----------------------------------------
#
## Include lon/lat when converting from Raster* to data.frame
#raster2df <- function(r) {
#  lon_lat <- xyFromCell(r, 1:ncell(r))
#  colnames(lon_lat) <- c("lon", "lat")
#  df <- as.data.frame(log10(r))
#  df <- cbind(lon_lat, df)
#  df
#}
#heterogeneity_w_coords <- map2(GCFR_heterogeneity, SWAFR_heterogeneity,
#  ~ na.exclude(rbind(
#    cbind(region = "GCFR",  raster2df(.x)),
#    cbind(region = "SWAFR", raster2df(.y))
#  ))
#)
#heterogeneity_w_coords %<>%
#  map(mutate_at, vars(str_replace_all(var_names, " ", "_")), scale) %>%
#  map(as_tibble)
#heterogeneity <- map2(heterogeneity, heterogeneity_w_coords,
#  full_join
#)
#
#heterogeneity$QDS$QDS <- heterogeneity %$%
#  QDS %$%
#  SpatialPoints(
#    coords      = data.frame(x = lon, y = lat),
#    proj4string = crs(Larsen_grid)
#  ) %over%
#  Larsen_grid %>%
#  pull(qdgc)
#
#heterogeneity$HDS$HDS <- heterogeneity %$%
#  HDS %$%
#  SpatialPoints(
#    coords      = data.frame(x = lon, y = lat),
#    proj4string = crs(Larsen_grid)
#  ) %over%
#  Larsen_grid %>%
#  pull(hdgc)
#
#heterogeneity$DS$DS <- heterogeneity %$%
#  DS %$%
#  SpatialPoints(
#    coords      = data.frame(x = lon, y = lat),
#    proj4string = crs(Larsen_grid)
#  ) %over%
#  Larsen_grid %>%
#  pull(dgc)

# Collate richness data into grids ---------------------------------------------

make_SpatialPointsDataFrame <- function(x) {
  SpatialPointsDataFrame(
    coords      = x[, c("decimallongitude", "decimallatitude")],
    data        = x[, "species"],
    proj4string = crs(Larsen_grid)
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
  Larsen_grid %>%
  pull(edgc)
species_occ@data$EDS %<>% as.character()
species_occ$QDS <- str_remove(species_occ$EDS, ".$")
species_occ$HDS <- str_remove(species_occ$QDS, ".$")
species_occ$DS  <- str_remove(species_occ$HDS, ".$")

# Export final lists of species
GCFR_species_occ@data %>%
  group_by(species)  %>%
  summarise(n_collections = n()) %>%
  arrange(desc(n_collections)) %>%
  write_csv(here("draft-02/manuscript_ver3/GCFR-species.csv"))
SWAFR_species_occ@data %>%
  group_by(species) %>%
  summarise(n_collections = n()) %>%
  arrange(desc(n_collections)) %>%
  write_csv(here("draft-02/manuscript_ver3/SWAFR-species.csv"))

# Flag species w/ < 5 collections total in each region
GCFR_bad_species <-
  read_csv(here("draft-02/manuscript_ver3/GCFR-species.csv")) %>%
  filter(n_collections < 5) %>%
  pull(species)
SWAFR_bad_species <-
  read_csv(here("draft-02/manuscript_ver3/SWAFR-species.csv")) %>%
  filter(n_collections < 5) %>%
  pull(species)
# Filter them out
species_occ_data <- species_occ@data %>%
  filter(!(species %in% c(GCFR_bad_species, SWAFR_bad_species)))

species_occ_data %<>% na.exclude()

# Collate richness measures at each scale
# At QDS-scale:
QDS_richness <- species_occ_data %>%
  full_join(tibble(EDS = Larsen_grid$edgc)) %>%
  mutate(EDS_in_region = EDS %in% Larsen_grid$edgc[Larsen_grid$in_region]) %>%
  group_by(QDS) %>%
  summarise(
    n_EDS_in_region = length(unique(EDS[EDS_in_region])),
    n_collections   = length(species),
    QDS_richness    = length(unique(species))
  ) %>%
  filter(n_EDS_in_region == 4)
# At HDS-scale:
mean_QDS_richness <- QDS_richness %>%
  mutate(HDS = str_remove(QDS, ".$")) %>%
  group_by(HDS) %>%
  summarise(mean_QDS_richness = mean(QDS_richness, na.rm = TRUE))
HDS_richness <- species_occ_data %>%
  full_join(tibble(QDS = Larsen_grid$qdgc[Larsen_grid$in_region])) %>%
  mutate(QDS_in_region = QDS %in% Larsen_grid$qdgc[Larsen_grid$in_region]) %>%
  group_by(HDS) %>%
  summarise(
    n_QDS_in_region = length(unique(QDS[QDS_in_region])),
    n_collections   = length(species),
    HDS_richness    = length(unique(species))
  ) %>%
  full_join(mean_QDS_richness) %>%
  filter(n_QDS_in_region == 4)
# At DS-scale:
mean_HDS_richness <- HDS_richness %>%
  mutate(DS = str_remove(HDS, ".$")) %>%
  group_by(DS) %>%
  summarise(mean_HDS_richness = mean(HDS_richness, na.rm = TRUE))
DS_richness <- species_occ_data %>%
  full_join(tibble(HDS = Larsen_grid$hdgc[Larsen_grid$in_region])) %>%
  mutate(HDS_in_region = HDS %in% Larsen_grid$hdgc[Larsen_grid$in_region]) %>%
  group_by(DS) %>%
  summarise(
    n_HDS_in_region = length(unique(HDS[HDS_in_region])),
    n_collections   = length(species),
    DS_richness     = length(unique(species))
  ) %>%
  full_join(mean_HDS_richness) %>%
  filter(n_HDS_in_region == 4)

# Collate gridded heterogeneity & richness data --------------------------------

data <- heterogeneity[-1]  # can't use 0.10x0.10 for richness data
data$QDS %<>%
  full_join(QDS_richness) %>%
  na.exclude() #%>%
  #filter(n_EDS == 4)
data$HDS %<>%
  full_join(HDS_richness) %>%
  na.exclude() %>%
  #filter(n_QDS == 4) %>%
  mutate(
    QDS_turnover      = HDS_richness - mean_QDS_richness,
    QDS_turnover_prop = QDS_turnover / HDS_richness
  )
data$DS %<>%
  full_join(DS_richness) %>%
  na.exclude() %>%
  #filter(n_HDS == 4) %>%
  mutate(
    HDS_turnover      = DS_richness - mean_HDS_richness,
    HDS_turnover_prop = HDS_turnover/DS_richness,
  )

# Test for differences in richness and turnover --------------------------------

# TODO: functionalise thise
richness_test_results <- data %$% rbind(
  QDS %$% tibble(
    metric = "QDS_richness",
    P_U = tidy(wilcox.test(QDS_richness ~ region))$p.value,
    CLES_value = CLES(
      QDS_richness[region == "SWAFR"],
      QDS_richness[region == "GCFR"]
    )
  ),
  HDS %$% tibble(
    metric = "HDS_richness",
    P_U = tidy(wilcox.test(HDS_richness ~ region))$p.value,
    CLES_value = CLES(
      HDS_richness[region == "SWAFR"],
      HDS_richness[region == "GCFR"]
    )
  ),
  DS %$% tibble(
    metric = "DS_richness",
    P_U = tidy(wilcox.test(DS_richness ~ region))$p.value,
    CLES_value = CLES(
      DS_richness[region == "SWAFR"],
      DS_richness[region == "GCFR"]
    )
  ),
  HDS %$% tibble(
    metric = "QDS_turnover_prop",
    P_U = tidy(wilcox.test(QDS_turnover_prop ~ region))$p.value,
    CLES_value = CLES(
      QDS_turnover_prop[region == "SWAFR"],
      QDS_turnover_prop[region == "GCFR"]
    )
  ),
  DS %$% tibble(
    metric = "HDS_turnover_prop",
    P_U = tidy(wilcox.test(HDS_turnover_prop ~ region))$p.value,
    CLES_value = CLES(
      HDS_turnover_prop[region == "SWAFR"],
      HDS_turnover_prop[region == "GCFR"]
    )
  )
)
# Print table
richness_test_results
