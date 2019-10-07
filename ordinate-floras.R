library(here)
library(tidyverse)
library(magrittr)
library(raster)
library(rgdal)
library(vegan)

data <- read_csv(here(
  "data/derived-data/May-2019",
  "data-QDS.csv"
))

GCFR_species <- read_csv(here(
  "data/derived-data/May-2019",
  "GCFR-species.csv"
))
SWAFR_species <- read_csv(here(
  "data/derived-data/May-2019",
  "SWAFR-species.csv"
))

GCFR_box  <- readOGR(here("data/derived-data/borders/GCFR_box"))
SWAFR_box <- readOGR(here("data/derived-data/borders/SWAFR_box"))

ZA_EDS <- readOGR(here("data/raw-data/QDGC/qdgc_zaf"), layer = "qdgc_03_zaf")
AU_EDS <- readOGR(here("data/raw-data/QDGC/qdgc_aus"), layer = "qdgc_03_aus")

GCFR_EDS  <- crop(ZA_EDS, GCFR_box)
SWAFR_EDS <- crop(AU_EDS, SWAFR_box)

Larsen_grid <- rbind(GCFR_EDS, SWAFR_EDS)

Larsen_grid$edgc <- Larsen_grid$qdgc
Larsen_grid$qdgc <- str_remove(Larsen_grid$edgc, ".$")
Larsen_grid$hdgc <- str_remove(Larsen_grid$qdgc, ".$")
Larsen_grid$dgc  <- str_remove(Larsen_grid$hdgc, ".$")

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

species_occ$EDS <- species_occ %over%
  Larsen_grid %>%
  pull(edgc)
species_occ@data$EDS %<>% as.character()
species_occ$QDS <- str_remove(species_occ$EDS, ".$")
species_occ$HDS <- str_remove(species_occ$QDS, ".$")
species_occ$DS  <- str_remove(species_occ$HDS, ".$")

species_occ2 <- species_occ@data %>%
  mutate(
    lon = DS %>%
      str_extract("E\\d\\d\\d") %>%
      str_remove("E") %>%
      as.numeric(),
    region = ifelse(lon >= 112, "SWAFR", "GCFR")
  ) %>%
  split(.$region) %>%
  map(dplyr::select, -EDS, -lon, -region) %>%
  map(distinct)

GCFR_species  %<>% filter(n_collections >= 5)
SWAFR_species %<>% filter(n_collections >= 5)

cells <- sort(unique(species_occ2$GCFR$QDS))
cells <- cells[cells %in% data$QDS]
species <- sort(unique(species_occ2$GCFR$species))
species <- species[species %in% GCFR_species$species]
n_cells <- length(cells)
n_species <- length(species)
GCFR_matrix <- matrix(nrow = n_cells, ncol = n_species)
rownames(GCFR_matrix) <- cells
colnames(GCFR_matrix) <- species
for (i in 1:nrow(GCFR_matrix)) {
  GCFR_matrix[i, ] <- species %in% species_occ2$GCFR$species[
    species_occ2$GCFR$QDS == cells[[i]]
  ]
}

cells <- sort(unique(species_occ2$SWAFR$QDS))
cells <- cells[cells %in% data$QDS]
species <- sort(unique(species_occ2$SWAFR$species))
species <- species[species %in% SWAFR_species$species]
n_cells <- length(cells)
n_species <- length(species)
SWAFR_matrix <- matrix(nrow = n_cells, ncol = n_species)
rownames(SWAFR_matrix) <- cells
colnames(SWAFR_matrix) <- species
for (i in 1:nrow(SWAFR_matrix)) {
  SWAFR_matrix[i, ] <- species %in% species_occ2$SWAFR$species[
    species_occ2$SWAFR$QDS == cells[[i]]
  ]
}

#cells <- sort(unique(c(
#  species_occ2$GCFR$QDS,
#  species_occ2$SWAFR$QDS
#)))
#cells <- cells[cells %in% data$QDS]
#species <- sort(unique(c(
#  species_occ2$GCFR$species,
#  species_occ2$SWAFR$species
#)))
#species <- species[species %in% c(
#  GCFR_species$species,
#  SWAFR_species$species
#)]
#n_cells <- length(cells)
#n_species <- length(species)
#BOTH_matrix <- matrix(nrow = n_cells, ncol = n_species)
#rownames(BOTH_matrix) <- cells
#colnames(BOTH_matrix) <- species
#for (i in 1:nrow(BOTH_matrix)) {
#  BOTH_matrix[i, ] <- species %in% species_occ@data$species[
#    species_occ@data$QDS == cells[[i]]
#  ]
#}

GCFR_jaccard <- vegdist(GCFR_matrix, method = "jaccard")
#GCFR_braycur <- vegdist(GCFR_matrix, method = "bray")
GCFR_pcoa <- ape::pcoa(GCFR_jaccard)
#GCFR_pcoa2 <- ape::pcoa(GCFR_braycur)
plot(GCFR_pcoa$vectors)
#plot(GCFR_pcoa2$vectors)

SWAFR_jaccard <- vegdist(SWAFR_matrix, method = "jaccard")
SWAFR_pcoa <- ape::pcoa(SWAFR_jaccard)
plot(SWAFR_pcoa$vectors)

#BOTH_jaccard <- vegdist(BOTH_matrix, method = "jaccard")
#BOTH_pcoa <- ape::pcoa(BOTH_jaccard)
#plot(BOTH_pcoa$vectors)

pcoa_axes <- as_tibble(rbind(
  cbind(
    region = "GCFR",
    QDS = rownames(GCFR_pcoa$vectors),
    GCFR_pcoa$vectors[, 1:2]
  ),
  cbind(
    region = "SWAFR",
    QDS = rownames(SWAFR_pcoa$vectors),
    SWAFR_pcoa$vectors[, 1:2]
  )
))
pcoa_axes[, 3:4] %<>% map(as.numeric)

#BOTH_pcoa_axes <- as_tibble(cbind(
#  QDS = rownames(BOTH_pcoa$vectors),
#  BOTH_pcoa$vectors[, 1:2]
#))
#BOTH_pcoa_axes %<>% mutate(
#  lon = QDS %>%
#    str_extract("E\\d\\d\\d") %>%
#    str_remove("E") %>%
#    as.numeric(),
#  region = ifelse(lon >= 112, "SWAFR", "GCFR")
#)
#BOTH_pcoa_axes[, 2:3] %<>% map(as.numeric)
#BOTH_pcoa_axes %<>% transmute(
#  region = region, QDS = QDS,
#  BOTH_Axis.1 = Axis.1,
#  BOTH_Axis.2 = Axis.2
#)

data %<>%
  full_join(pcoa_axes) %>%
  #full_join(BOTH_pcoa_axes)
  mutate(
    vegtype = case_when(
      (Axis.1 > 0) & (Axis.2 > 0) ~ 1,
      (Axis.1 > 0) & (Axis.2 < 0) ~ 2,
      (Axis.1 < 0) & (Axis.2 > 0) ~ 3,
      (Axis.1 < 0) & (Axis.2 < 0) ~ 4,
    ),
    vegunique = sqrt((0 - Axis.1)^2 + (0 - Axis.2)^2)
  )

GCFR_pcoa_kmeans <- kmeans(GCFR_pcoa$vectors, 4)
GCFR_pcoa_kmeans <- tibble(
  QDS      = names(GCFR_pcoa_kmeans$cluster),
  kcluster = GCFR_pcoa_kmeans$cluster
)
SWAFR_pcoa_kmeans <- kmeans(SWAFR_pcoa$vectors, 4)
SWAFR_pcoa_kmeans <- tibble(
  QDS      = names(SWAFR_pcoa_kmeans$cluster),
  kcluster = SWAFR_pcoa_kmeans$cluster
)
data %<>% full_join(rbind(GCFR_pcoa_kmeans, SWAFR_pcoa_kmeans))

ggplot(data, aes(lon, lat, colour = Axis.1)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free_x")

ggplot(data, aes(lon, lat, colour = Axis.2)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free_x")

ggplot(data, aes(lon, lat, colour = vegtype)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free_x")

ggplot(data, aes(lon, lat, colour = kcluster)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free_x")

ggplot(data, aes(lon, lat, colour = vegunique)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free_x")

ggplot(data, aes(Axis.1, Axis.2, colour = kcluster)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free_x")

ggplot(data, aes(Axis.1, Axis.2, colour = PC1)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_grid(~region)

ggplot(data, aes(Axis.1, Axis.2, colour = QDS_richness)) +
  geom_point() +
  scale_colour_viridis_c(trans = "log10") +
  facet_grid(~region)

ggplot(data, aes(Axis.1, Axis.2, colour = vegunique)) +
  geom_point() +
  scale_colour_viridis_c(trans = "log10") +
  facet_grid(~region)

data %>%
  group_by(region, vegtype) %>%
  summarize(mean_QDS_richness = mean(QDS_richness))

ggplot(data, aes(QDS_richness, vegunique)) +
  geom_point() +
  scale_x_log10() +
  facet_grid(~region)

ggplot(data, aes(PC1, vegunique, colour = QDS_richness)) +
  geom_point() +
  scale_colour_viridis_c(trans = "log10") +
  facet_grid(~region)

ggplot(data, aes(PC1, QDS_richness, colour = vegtype)) +
  geom_point() +
  scale_y_log10() +
  scale_colour_viridis_c() +
  facet_grid(~region)

ggplot(data, aes(PC1, QDS_richness, colour = vegunique)) +
  geom_point() +
  scale_y_log10() +
  scale_colour_viridis_c() +
  facet_grid(~region)
