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
GCFR_EDS  <- crop(ZA_EDS, GCFR_box)
SWAFR_EDS <- crop(AU_EDS, SWAFR_box)

# Get DS, HDS, QDS codes from EDS codes
Larsen_grid <- rbind(GCFR_EDS, SWAFR_EDS)
Larsen_grid$edgc <- Larsen_grid$qdgc
Larsen_grid$qdgc <- str_remove(Larsen_grid$edgc, ".$")
Larsen_grid$hdgc <- str_remove(Larsen_grid$qdgc, ".$")
Larsen_grid$dgc  <- str_remove(Larsen_grid$hdgc, ".$")

# Detemine which DS, HDS & QDS have all 4 of their HDS, QDS & EDS --------------

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

AU_HDS@data %<>% cbind(AU_HDS %over% SWAFR_border_buffered)

AU_HDS@data$region %<>% {!is.na(.)}

SWAFR_HDS <- AU_HDS[AU_HDS$region, ]

SWAFR_HDS_QDS <- intersect(SWAFR_HDS, SWAFR_QDS)

SWAFR_HDS_w_all_QDS <- SWAFR_HDS_QDS@data %>%
  group_by(qdgc.1) %>%
  summarise(n_QDS = n()) %>%
  filter(n_QDS == 4) %>%
  pull(qdgc.1) %>%
  as.character()

# .... DS-scale ----------------------------------------------------------------

GCFR_HDS$dgc <- str_remove(GCFR_HDS$qdgc, ".$")

GCFR_DS_w_all_HDS <- GCFR_HDS@data %>%
  group_by(dgc) %>%
  summarise(n_HDS = n()) %>%
  filter(n_HDS == 4) %>%
  pull(dgc) %>%
  as.character()

# Check
plot(GCFR_HDS, lwd = 2)
plot(
  GCFR_HDS[GCFR_HDS$dgc %in% GCFR_DS_w_all_HDS, ],
  border = "green", add = TRUE
)
# Works!

SWAFR_HDS$dgc <- str_remove(SWAFR_HDS$qdgc, ".$")

SWAFR_DS_w_all_HDS <- SWAFR_HDS@data %>%
  group_by(dgc) %>%
  summarise(n_HDS = n()) %>%
  filter(n_HDS == 4) %>%
  pull(dgc) %>%
  as.character()

# Collate heterogeneity data into grids ----------------------------------------

# Include lon/lat when converting from Raster* to data.frame
raster2df <- function(r) {
  lon_lat <- xyFromCell(r, 1:ncell(r))
  colnames(lon_lat) <- c("lon", "lat")
  df <- as.data.frame(log10(r))
  df <- cbind(lon_lat, df)
  df
}
heterogeneity_w_coords <- map2(GCFR_heterogeneity, SWAFR_heterogeneity,
  ~ na.exclude(rbind(
    cbind(region = "GCFR",  raster2df(.x)),
    cbind(region = "SWAFR", raster2df(.y))
  ))
)
heterogeneity_w_coords %<>%
  map(mutate_at, vars(str_replace_all(var_names, " ", "_")), scale) %>%
  map(as_tibble)
heterogeneity <- map2(heterogeneity, heterogeneity_w_coords,
  full_join
)

heterogeneity$QDS$QDS <- heterogeneity %$%
  QDS %$%
  SpatialPoints(
    coords      = data.frame(x = lon, y = lat),
    proj4string = crs(Larsen_grid)
  ) %over%
  Larsen_grid %>%
  pull(qdgc)
heterogeneity$QDS %<>%
  filter(QDS %in% c(GCFR_QDS_w_all_EDS, SWAFR_QDS_w_all_EDS))

heterogeneity$HDS$HDS <- heterogeneity %$%
  HDS %$%
  SpatialPoints(
    coords      = data.frame(x = lon, y = lat),
    proj4string = crs(Larsen_grid)
  ) %over%
  Larsen_grid %>%
  pull(hdgc)
heterogeneity$HDS %<>%
  filter(HDS %in% c(GCFR_HDS_w_all_QDS, SWAFR_HDS_w_all_QDS))

heterogeneity$DS$DS <- heterogeneity %$%
  DS %$%
  SpatialPoints(
    coords      = data.frame(x = lon, y = lat),
    proj4string = crs(Larsen_grid)
  ) %over%
  Larsen_grid %>%
  pull(dgc)
heterogeneity$DS %<>%
  filter(DS %in% c(GCFR_DS_w_all_HDS, SWAFR_DS_w_all_HDS))

# Check
map(heterogeneity[2:4],
  ~ ggplot(.x, aes(lon, lat)) +
    geom_point() +
    facet_grid(~region, scales = "free_x")
)
# Works!

# Collate richness data into grids ---------------------------------------------

make_SpatialPointsDataFrame <- function(df) {
  SpatialPointsDataFrame(
    coords      = df[, c("decimallongitude", "decimallatitude")],
    data        = df[, "species"],
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
  filter(!(species %in% c(GCFR_bad_species, SWAFR_bad_species)))

species_occ_data %<>% na.exclude()

# Collate richness measures at each scale
# At QDS-scale:
QDS_richness <- species_occ_data %>%
  full_join(tibble(EDS = Larsen_grid$edgc)) %>%
  mutate(EDS_in_region = EDS %in% Larsen_grid$edgc[Larsen_grid$in_region]) %>%
  group_by(QDS) %>%
  filter(QDS %in% c(GCFR_QDS_w_all_EDS, SWAFR_QDS_w_all_EDS)) %>%
  summarise(
    n_EDS_in_region = length(unique(EDS[EDS_in_region])),
    n_collections   = length(species),
    QDS_richness    = length(unique(species))
  )
# At HDS-scale:
mean_QDS_richness <- QDS_richness %>%
  mutate(HDS = str_remove(QDS, ".$")) %>%
  group_by(HDS) %>%
  summarise(mean_QDS_richness = mean(QDS_richness, na.rm = TRUE))
HDS_richness <- species_occ_data %>%
  full_join(tibble(QDS = Larsen_grid$qdgc[Larsen_grid$in_region])) %>%
  mutate(QDS_in_region = QDS %in% Larsen_grid$qdgc[Larsen_grid$in_region]) %>%
  group_by(HDS) %>%
  filter(HDS %in% c(GCFR_HDS_w_all_QDS, SWAFR_HDS_w_all_QDS)) %>%
  summarise(
    n_QDS_in_region = length(unique(QDS[QDS_in_region])),
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
  full_join(tibble(HDS = Larsen_grid$hdgc[Larsen_grid$in_region])) %>%
  mutate(HDS_in_region = HDS %in% Larsen_grid$hdgc[Larsen_grid$in_region]) %>%
  group_by(DS) %>%
  filter(DS %in% c(GCFR_DS_w_all_HDS, SWAFR_DS_w_all_HDS)) %>%
  summarise(
    n_HDS_in_region = length(unique(HDS[HDS_in_region])),
    n_collections   = length(species),
    DS_richness     = length(unique(species))
  ) %>%
  full_join(mean_HDS_richness)

# Collate gridded heterogeneity & richness data --------------------------------

data <- heterogeneity[-1]  # b/c can't use 0.10 x 0.10 for richness data
data$QDS %<>%
  full_join(QDS_richness) %>%
  na.exclude()
data$HDS %<>%
  full_join(HDS_richness) %>%
  na.exclude() %>%
  mutate(
    QDS_turnover      = HDS_richness - mean_QDS_richness,
    QDS_turnover_prop = QDS_turnover / HDS_richness
  )
data$DS %<>%
  full_join(DS_richness) %>%
  na.exclude() %>%
  mutate(
    HDS_turnover      = DS_richness - mean_HDS_richness,
    HDS_turnover_prop = HDS_turnover/DS_richness,
  )

# Check
map(data, visdat::vis_dat)
map(data,
  ~ ggplot(.x, aes(lon, lat)) +
    geom_point() +
    facet_grid(~region, scales = "free_x")
)
# Good!

# Save to disc
iwalk(data, ~write_csv(.x, glue("{data_dir}/data-{.y}.csv")))

# Rasterise richness & PC1 data ------------------------------------------------

# .... QDS-scale ---------------------------------------------------------------

# GCFR:
GCFR_QDS_richness   <- GCFR_QDS_PC1   <- GCFR_heterogeneity$QDS$Elevation
GCFR_QDS_richness[] <- GCFR_QDS_PC1[] <- NA
names(GCFR_QDS_richness) <- "QDS_richness"
names(GCFR_QDS_PC1)      <- "QDS_PC1"
cell_nos <- cellFromXY(
  GCFR_QDS_richness,
  data %$%
    QDS %>%
    filter(region == "GCFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
GCFR_QDS_richness[cell_nos] <- data %$%
  QDS %>%
  filter(region == "GCFR") %>%
  pull(QDS_richness)
GCFR_QDS_PC1[cell_nos] <- data %$%
  QDS %>%
  filter(region == "GCFR") %>%
  pull(PC1)
# Check
plot(GCFR_QDS_richness)
plot(GCFR_border, add = TRUE)
plot(GCFR_QDS_PC1)
plot(GCFR_border, add = TRUE)
plot(GCFR_QDS_richness[] ~ GCFR_QDS_PC1[])
# Works!
writeRaster(
  GCFR_QDS_richness,
  glue("{data_dir}/GCFR_QDS_richness.tif"),
  overwrite = TRUE
)
writeRaster(
  GCFR_QDS_PC1,
  glue("{data_dir}/GCFR_QDS_PC1.tif"),
  overwrite = TRUE
)

# SWAFR:
SWAFR_QDS_richness   <- SWAFR_QDS_PC1   <- SWAFR_heterogeneity$QDS$Elevation
SWAFR_QDS_richness[] <- SWAFR_QDS_PC1[] <- NA
names(SWAFR_QDS_richness) <- "QDS_richness"
names(SWAFR_QDS_PC1)      <- "QDS_PC1"
cell_nos <- cellFromXY(
  SWAFR_QDS_richness,
  data %$%
    QDS %>%
    filter(region == "SWAFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
SWAFR_QDS_richness[cell_nos] <- data %$%
  QDS %>%
  filter(region == "SWAFR") %>%
  pull(QDS_richness)
SWAFR_QDS_PC1[cell_nos] <- data %$%
  QDS %>%
  filter(region == "SWAFR") %>%
  pull(PC1)
plot(SWAFR_QDS_richness)
plot(SWAFR_border, add = TRUE)
plot(SWAFR_QDS_PC1)
plot(SWAFR_border, add = TRUE)
plot(SWAFR_QDS_richness[] ~ SWAFR_QDS_PC1[])
writeRaster(
  SWAFR_QDS_richness,
  glue("{data_dir}/SWAFR_QDS_richness.tif"),
  overwrite = TRUE
)
writeRaster(
  SWAFR_QDS_richness,
  glue("{data_dir}/SWAFR_QDS_PC1.tif"),
  overwrite = TRUE
)

# .... HDS-scale ---------------------------------------------------------------

# GCFR:
GCFR_HDS_richness   <- GCFR_HDS_PC1   <- GCFR_heterogeneity$HDS$Elevation
GCFR_HDS_richness[] <- GCFR_HDS_PC1[] <- NA
names(GCFR_HDS_richness) <- "HDS_richness"
names(GCFR_HDS_PC1)      <- "HDS_PC1"
cell_nos <- cellFromXY(
  GCFR_HDS_richness,
  data %$%
    HDS %>%
    filter(region == "GCFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
GCFR_HDS_richness[cell_nos] <- data %$%
  HDS %>%
  filter(region == "GCFR") %>%
  pull(HDS_richness)
GCFR_HDS_PC1[cell_nos] <- data %$%
  HDS %>%
  filter(region == "GCFR") %>%
  pull(PC1)
plot(GCFR_HDS_richness)
plot(GCFR_border, add = TRUE)
plot(GCFR_HDS_PC1)
plot(GCFR_border, add = TRUE)
plot(GCFR_HDS_richness[] ~ GCFR_HDS_PC1[])
writeRaster(
  GCFR_HDS_richness,
  glue("{data_dir}/GCFR_HDS_richness.tif"),
  overwrite = TRUE
)
writeRaster(
  GCFR_HDS_PC1,
  glue("{data_dir}/GCFR_HDS_PC1.tif"),
  overwrite = TRUE
)

# SWAFR:
SWAFR_HDS_richness   <- SWAFR_HDS_PC1   <- SWAFR_heterogeneity$HDS$Elevation
SWAFR_HDS_richness[] <- SWAFR_HDS_PC1[] <- NA
names(SWAFR_HDS_richness) <- "HDS_richness"
names(SWAFR_HDS_PC1)      <- "HDS_PC1"
cell_nos <- cellFromXY(
  SWAFR_HDS_richness,
  data %$%
    HDS %>%
    filter(region == "SWAFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
SWAFR_HDS_richness[cell_nos] <- data %$%
  HDS %>%
  filter(region == "SWAFR") %>%
  pull(HDS_richness)
SWAFR_HDS_PC1[cell_nos] <- data %$%
  HDS %>%
  filter(region == "SWAFR") %>%
  pull(PC1)
plot(SWAFR_HDS_richness)
plot(SWAFR_border, add = TRUE)
plot(SWAFR_HDS_PC1)
plot(SWAFR_border, add = TRUE)
plot(SWAFR_HDS_richness[] ~ SWAFR_HDS_PC1[])
writeRaster(
  SWAFR_HDS_richness,
  glue("{data_dir}/SWAFR_HDS_richness.tif"),
  overwrite = TRUE
)
writeRaster(
  SWAFR_HDS_PC1,
  glue("{data_dir}/SWAFR_HDS_PC1.tif"),
  overwrite = TRUE
)

# .... DS-scale ----------------------------------------------------------------

# GCFR:
GCFR_DS_richness   <- GCFR_DS_PC1   <- GCFR_heterogeneity$DS$Elevation
GCFR_DS_richness[] <- GCFR_DS_PC1[] <- NA
names(GCFR_DS_richness) <- "DS_richness"
names(GCFR_DS_PC1)      <- "DS_PC1"
cell_nos <- cellFromXY(
  GCFR_DS_richness,
  data %$%
    DS %>%
    filter(region == "GCFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
GCFR_DS_richness[cell_nos] <- data %$%
  DS %>%
  filter(region == "GCFR") %>%
  pull(DS_richness)
GCFR_DS_PC1[cell_nos] <- data %$%
  DS %>%
  filter(region == "GCFR") %>%
  pull(PC1)
plot(GCFR_DS_richness)
plot(GCFR_border, add = TRUE)
plot(GCFR_DS_PC1)
plot(GCFR_border, add = TRUE)
plot(GCFR_DS_richness[] ~ GCFR_DS_PC1[])
writeRaster(
  GCFR_DS_richness,
  glue("{data_dir}/GCFR_DS_richness.tif"),
  overwrite = TRUE
)
writeRaster(
  GCFR_DS_PC1,
  glue("{data_dir}/GCFR_DS_PC1.tif"),
  overwrite = TRUE
)

# SWAFR:
SWAFR_DS_richness   <- SWAFR_DS_PC1   <- SWAFR_heterogeneity$DS$Elevation
SWAFR_DS_richness[] <- SWAFR_DS_PC1[] <- NA
names(SWAFR_DS_richness) <- "DS_richness"
names(SWAFR_DS_PC1)      <- "DS_PC1"
cell_nos <- cellFromXY(
  SWAFR_DS_richness,
  data %$%
    DS %>%
    filter(region == "SWAFR") %>%
    dplyr::select(lon, lat) %>%
    as.matrix()
)
SWAFR_DS_richness[cell_nos] <- data %$%
  DS %>%
  filter(region == "SWAFR") %>%
  pull(DS_richness)
SWAFR_DS_PC1[cell_nos] <- data %$%
  DS %>%
  filter(region == "SWAFR") %>%
  pull(PC1)
plot(SWAFR_DS_richness)
plot(SWAFR_border, add = TRUE)
plot(SWAFR_DS_PC1)
plot(SWAFR_border, add = TRUE)
plot(SWAFR_DS_richness[] ~ SWAFR_DS_PC1[])
writeRaster(
  SWAFR_DS_richness,
  glue("{data_dir}/SWAFR_DS_richness.tif"),
  overwrite = TRUE
)
writeRaster(
  SWAFR_DS_PC1,
  glue("{data_dir}/SWAFR_DS_PC1.tif"),
  overwrite = TRUE
)
