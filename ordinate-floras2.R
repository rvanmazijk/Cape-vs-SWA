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

GCFR_species  %<>% mutate(genus = str_extract(species, "^[a-zA-Z]+"))
SWAFR_species %<>% mutate(genus = str_extract(species, "^[a-zA-Z]+"))

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
    data        = df[, "genus"],
    proj4string = crs(Larsen_grid)
  )
}
GCFR_genus_occ <- make_SpatialPointsDataFrame(read_csv(here(
  "data/derived-data/flora",
  "GCFR_clean_flora_2017-09-14.csv"
)))
SWAFR_genus_occ <- make_SpatialPointsDataFrame(read_csv(here(
  "data/derived-data/flora",
  "SWAFR_clean_flora_2017-09-14.csv"
)))
genus_occ <- rbind(GCFR_genus_occ, SWAFR_genus_occ)

genus_occ$EDS <- genus_occ %over%
  Larsen_grid %>%
  pull(edgc)
genus_occ@data$EDS %<>% as.character()
genus_occ$QDS <- str_remove(genus_occ$EDS, ".$")
genus_occ$HDS <- str_remove(genus_occ$QDS, ".$")
genus_occ$DS  <- str_remove(genus_occ$HDS, ".$")

genus_occ2 <- genus_occ@data %>%
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

GCFR_species %<>%
  group_by(genus) %>%
  summarise(n_collections = sum(n_collections)) %>%
  filter(n_collections >= 5)
SWAFR_species %<>%
  group_by(genus) %>%
  summarise(n_collections = sum(n_collections)) %>%
  filter(n_collections >= 5)

cells <- sort(unique(genus_occ2$GCFR$QDS))
cells <- cells[cells %in% data$QDS]
genera <- sort(unique(genus_occ2$GCFR$genus))
genera <- genera[genera %in% GCFR_species$genus]
n_cells <- length(cells)
n_genera <- length(genera)
GCFR_matrix <- matrix(nrow = n_cells, ncol = n_genera)
rownames(GCFR_matrix) <- cells
colnames(GCFR_matrix) <- genera
for (i in 1:nrow(GCFR_matrix)) {
  GCFR_matrix[i, ] <- genera %in% genus_occ2$GCFR$genus[
    genus_occ2$GCFR$QDS == cells[[i]]
  ]
}

cells <- sort(unique(genus_occ2$SWAFR$QDS))
cells <- cells[cells %in% data$QDS]
genera <- sort(unique(genus_occ2$SWAFR$genus))
genera <- genera[genera %in% SWAFR_species$genus]
n_cells <- length(cells)
n_genera <- length(genera)
SWAFR_matrix <- matrix(nrow = n_cells, ncol = n_genera)
rownames(SWAFR_matrix) <- cells
colnames(SWAFR_matrix) <- genera
for (i in 1:nrow(SWAFR_matrix)) {
  SWAFR_matrix[i, ] <- genera %in% genus_occ2$SWAFR$genus[
    genus_occ2$SWAFR$QDS == cells[[i]]
  ]
}

cells <- sort(unique(c(
  genus_occ2$GCFR$QDS,
  genus_occ2$SWAFR$QDS
)))
cells <- cells[cells %in% data$QDS]
genera <- sort(unique(c(
  genus_occ2$GCFR$genus,
  genus_occ2$SWAFR$genus
)))
genera <- genera[genera %in% c(
  GCFR_species$genus,
  SWAFR_species$genus
)]
n_cells <- length(cells)
n_genera <- length(genera)
BOTH_matrix <- matrix(nrow = n_cells, ncol = n_genera)
rownames(BOTH_matrix) <- cells
colnames(BOTH_matrix) <- genera
for (i in 1:nrow(BOTH_matrix)) {
  BOTH_matrix[i, ] <- genera %in% genus_occ@data$genus[
    genus_occ@data$QDS == cells[[i]]
  ]
}

GCFR_jaccard <- vegdist(GCFR_matrix, method = "jaccard")
#GCFR_braycur <- vegdist(GCFR_matrix, method = "bray")
GCFR_pcoa <- ape::pcoa(GCFR_jaccard)
#GCFR_pcoa2 <- ape::pcoa(GCFR_braycur)
plot(GCFR_pcoa$vectors)
#plot(GCFR_pcoa2$vectors)

SWAFR_jaccard <- vegdist(SWAFR_matrix, method = "jaccard")
SWAFR_pcoa <- ape::pcoa(SWAFR_jaccard)
plot(SWAFR_pcoa$vectors)

BOTH_jaccard <- vegdist(BOTH_matrix, method = "jaccard")
BOTH_pcoa <- ape::pcoa(BOTH_jaccard)
plot(BOTH_pcoa$vectors)

GCFR_jaccard %>%
  as.matrix() %>%
  as.data.frame() %>%
  cbind(QDS1 = rownames(.)) %>%
  gather(QDS2, jaccard, -QDS1) %>%
  ggplot() +
  aes(
    reorder(QDS1, desc(jaccard)),
    reorder(QDS2, desc(jaccard)),
    fill = jaccard
  ) +
  geom_tile() +
  scale_fill_viridis_c(direction = -1) +
  theme(axis.ticks = element_blank(), axis.text = element_blank())

SWAFR_jaccard %>%
  as.matrix() %>%
  as.data.frame() %>%
  cbind(QDS1 = rownames(.)) %>%
  gather(QDS2, jaccard, -QDS1) %>%
  ggplot() +
  aes(
    reorder(QDS1, desc(jaccard)),
    reorder(QDS2, desc(jaccard)),
    fill = jaccard
  ) +
  geom_tile() +
  scale_fill_viridis_c(direction = -1) +
  theme(axis.ticks = element_blank(), axis.text = element_blank())

rbind(
  cbind(region = "GCFR",  jaccard = as.vector(GCFR_jaccard)),
  cbind(region = "SWAFR", jaccard = as.vector(SWAFR_jaccard))
) %>%
  as.data.frame() %>%
  mutate(jaccard = as.numeric(as.character(jaccard))) %>%
  ggplot(aes(jaccard, fill = region)) +
  geom_histogram(position = "dodge")

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

BOTH_pcoa_axes <- as_tibble(cbind(
  QDS = rownames(BOTH_pcoa$vectors),
  BOTH_pcoa$vectors[, 1:2]
))
BOTH_pcoa_axes %<>% mutate(
  lon = QDS %>%
    str_extract("E\\d\\d\\d") %>%
    str_remove("E") %>%
    as.numeric(),
  region = ifelse(lon >= 112, "SWAFR", "GCFR")
)
BOTH_pcoa_axes[, 2:3] %<>% map(as.numeric)
BOTH_pcoa_axes %<>% transmute(
  region = region, QDS = QDS,
  BOTH_Axis.1 = Axis.1,
  BOTH_Axis.2 = Axis.2
)

data %<>%
  full_join(pcoa_axes) %>%
  full_join(BOTH_pcoa_axes) %>%
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

#ggplot(data, aes(lon, lat, colour =  BOTH_Axis.1)) +
#  geom_point() +
#  scale_colour_viridis_c() +
#  facet_grid(~region, scales = "free_x")

ggplot(data, aes(lon, lat, colour = Axis.2)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_grid(~region, scales = "free_x")

#ggplot(data, aes(lon, lat, colour =  BOTH_Axis.2)) +
#  geom_point() +
#  scale_colour_viridis_c() +
#  facet_grid(~region, scales = "free_x")

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

ggplot(data, aes(lon, lat, colour = Axis.1 > 0)) +
  geom_point() +
  facet_grid(~region, scales = "free_x")

ggplot(data, aes(Axis.1, Axis.2, colour = vegtype)) +
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

ggplot(data, aes(BOTH_Axis.1, BOTH_Axis.2, colour = QDS_richness)) +
  geom_point() +
  scale_colour_viridis_c(trans = "log10")

ggplot(data[data$region == "GCFR", ], aes(Axis.1, Axis.2, colour = lon)) +
  geom_point() +
  scale_colour_viridis_c()
ggplot(data[data$region == "GCFR", ], aes(Axis.1, Axis.2, colour = lat)) +
  geom_point() +
  scale_colour_viridis_c()

ggplot(data[data$region == "SWAFR", ], aes(Axis.1, Axis.2, colour = lon)) +
  geom_point() +
  scale_colour_viridis_c()
ggplot(data[data$region == "SWAFR", ], aes(Axis.1, Axis.2, colour = lat)) +
  geom_point() +
  scale_colour_viridis_c()

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

ggplot(data, aes(PC1, QDS_richness, colour = Axis.1 > 0)) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_y_log10() +
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
