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

species_occ_data %<>%
  mutate(genus = str_extract(species, "[^ ]+"))

# Make community matrix --------------------------------------------------------
# (QDS-scale)

cells            <- sort(unique(species_occ_data$QDS))
genera           <- sort(unique(species_occ_data$genus))
community_matrix <- matrix(nrow = length(cells), ncol = length(genera))
rownames(community_matrix) <- cells
colnames(community_matrix) <- genera
for (i in 1:nrow(community_matrix)) {
  community_matrix[i, ] <- genera %in% species_occ_data$genus[
    species_occ_data$QDS == cells[[i]]
  ]
}

# Split community matrix into each regions'
region_index <- community_matrix %>%
  rownames() %>%
  str_extract("E[0-9]{3}") %>%
  str_remove("E") %>%
  as.numeric() %>%
  is_greater_than(60) %>%
  ifelse("SWAFR", "GCFR")

GCFR_matrix  <- community_matrix[region_index == "GCFR", ]
SWAFR_matrix <- community_matrix[region_index == "SWAFR", ]

both_jaccard  <- vegan::vegdist(community_matrix, method = "jaccard")
GCFR_jaccard  <- vegan::vegdist(GCFR_matrix,      method = "jaccard")
SWAFR_jaccard <- vegan::vegdist(SWAFR_matrix,     method = "jaccard")

both_pcoa  <- ape::pcoa(both_jaccard)
GCFR_pcoa  <- ape::pcoa(GCFR_jaccard)
SWAFR_pcoa <- ape::pcoa(SWAFR_jaccard)

plot(both_pcoa$vectors[, 1:2], col = as.factor(region_index))
plot(GCFR_pcoa$vectors[, 1:2])
plot(SWAFR_pcoa$vectors[, 1:2])
