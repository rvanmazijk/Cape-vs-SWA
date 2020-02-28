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

# Make community matrix --------------------------------------------------------
# (QDS-scale)

cells            <- sort(unique(species_occ_data$QDS))
species          <- sort(unique(species_occ_data$species))
community_matrix <- matrix(nrow = length(cells), ncol = length(species))
rownames(community_matrix) <- cells
colnames(community_matrix) <- species
for (i in 1:nrow(community_matrix)) {
  community_matrix[i, ] <- species %in% species_occ_data$species[
    species_occ_data$QDS == cells[[i]]
  ]
}

# Tidy matrices into range sizes -----------------------------------------------

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

GCFR_range_sizes <- GCFR_matrix %>%
  apply(2, sum) %>%
  sort(decreasing = TRUE) %>%
  {tibble(region = "GCFR", species = names(.), n_QDS = .)}
SWAFR_range_sizes <- SWAFR_matrix %>%
  apply(2, sum) %>%
  sort(decreasing = TRUE) %>%
  {tibble(region = "SWAFR", species = names(.), n_QDS = .)}
range_sizes <- full_join(GCFR_range_sizes, SWAFR_range_sizes)

# Plot -------------------------------------------------------------------------

range_size_plot <- ggplot(range_sizes) +
  aes(n_QDS, fill = region, group = region) +
  geom_histogram(
    # Scale to frequencies/proportions of cells for each region separately
    aes(y = 2*(..density..)/sum(..density..)),
    bins = 20,
    position = "dodge",
    colour = "black"
  ) +
  labs(x = "Range size (no. QDS)", y = "Prop. species") +
  scale_x_log10() +
  scale_fill_manual(name = "Region", values = c("black", "white")) +
  theme(legend.position = c(0.8, 0.8))

# Save to disc
ggsave(
  here("figures/compare-species-range-sizes.pdf"),
  range_size_plot,
  width = 4, height = 4
)
ggsave(
  here("figures/compare-species-range-sizes.png"),
  range_size_plot, dpi = 600,
  width = 4, height = 4
)

# Test -------------------------------------------------------------------------

range_sizes %$% {
  print(wilcox.test(
    n_QDS[region == "SWAFR"], n_QDS[region == "GCFR"]
  ))
  message("CLES = ", CLES(
    n_QDS[region == "SWAFR"], n_QDS[region == "GCFR"]
  ))
}

