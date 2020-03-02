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

range_size_plot <- ggplot(range_sizes[range_sizes$n_QDS > 0, ]) +
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
  print(t.test(
    log10(n_QDS[(region == "SWAFR") & (n_QDS > 0)]),
    log10(n_QDS[(region == "GCFR")  & (n_QDS > 0)])
  ))
  print(wilcox.test(
    n_QDS[(region == "SWAFR") & (n_QDS > 0)],
    n_QDS[(region == "GCFR")  & (n_QDS > 0)]
  ))
  message("CLES = ", CLES(
    n_QDS[(region == "SWAFR") & (n_QDS > 0)],
    n_QDS[(region == "GCFR")  & (n_QDS > 0)]
  ))
}

# Assess whether SWAFR species are being planted in Perth ----------------------

# List Perth's QDS-cells manually
Perth <- tribble(
  ~lon,    ~lat,    ~qdgc,
  115.875, -31.875, "E115S31DD",
  115.875, -32.125, "E115S32BB",
  116.125, -31.875, "E116S31CC",
  116.125, -32.125, "E116S32AA"
)
# Check
plot(SWAFR_border_buffered)
points(Perth[, 1:2])

# Simplify
Perth <- Perth$qdgc

species_in_Perth <- SWAFR_matrix[rownames(SWAFR_matrix) %in% Perth, ] %>%
  apply(2, sum) %>%
  sort(decreasing = TRUE) %>%
  {tibble(species = names(.), n_QDS = .)} %>%
  filter(n_QDS > 0) %>%
  pull(species)

# How many species have occurrences in Perth?
length(species_in_Perth)
## [1] 2944

# What proportion of species in the SWAFR have an occurrence in Perth?
length(species_in_Perth) / ncol(SWAFR_matrix)
## [1] 0.1838966

# .... Plot distribution of SWAFR Perth vs non-Perth species' range sizes ------

SWAFR_range_sizes %>%
  mutate(in_Perth = species %in% species_in_Perth) %>%
  ggplot() +
    aes(n_QDS, fill = in_Perth) +
    geom_histogram(position = "dodge") +
    scale_x_log10()

# .... Plot some maps of Perth-occurring species -------------------------------
# To see if Perth is within those species' "main range" or not

SWAFR_species_occ$EDS <- SWAFR_species_occ %over%
  Larsen_grid_EDS %>%
  pull(edgc) %>%
  as.character()
SWAFR_species_occ$QDS <- str_remove(SWAFR_species_occ$EDS, ".$")

SWAFR_Perth_species_occ <- SWAFR_species_occ[
  SWAFR_species_occ$species %in% species_in_Perth,
]
SWAFR_Perth_species_occ$in_region <- SWAFR_Perth_species_occ %over%
  SWAFR_border_buffered %>%
  {!is.na(.)}
SWAFR_Perth_species_occ <- SWAFR_Perth_species_occ[
  c(SWAFR_Perth_species_occ$in_region),
]

set.seed(1234)
random_Perth_species <- sample(unique(SWAFR_Perth_species_occ$species), 300)

pdf(
  here("figures/plot-300-Perth-species-ranges.pdf"),
  width = 100, height = 100
)
par(mfrow = c(10, 10))
for (i in 1:length(random_Perth_species)) {
  plot(SWAFR_border_buffered)
  points(
    SWAFR_Perth_species_occ[
      SWAFR_Perth_species_occ$species == random_Perth_species[[i]],
    ],
    cex = 3, pch = 20,
  )
  # Plot Perth
  points(115.8605, -31.9505, cex = 4, pch = 23, bg = rgb(1, 1, 1, 0.5))
  title(random_Perth_species[[i]], cex.main = 5)
}
par(op)
dev.off()

# Plot only species' ranges that I think look like have Perth as an outlier
pdf(
  here("figures/plot-flagged-Perth-species-ranges.pdf"),
  width = 50, height = 40
)
par(mfrow = c(5, 6))
flagged_species <- random_Perth_species[c(
  # Numbers of panels (in 3x 10x10 plots above):
    9,  18,  35,  67,  72,  73,  74,  84,
  103, 104, 122, 127, 129, 137, 162, 167, 186, 194,
  215, 224, 231, 241, 257, 271, 276, 286, 294, 298
)]
flagged_species %<>% sort()
flagged_species %>%
  {tibble(species = .)} %>%
  write_csv(here("results/Perth-flagged-SWAFR-species.csv"))
for (i in 1:length(flagged_species)) {
  plot(SWAFR_border_buffered)
  points(
    SWAFR_Perth_species_occ[
      SWAFR_Perth_species_occ$species == flagged_species[[i]],
    ],
    cex = 5, pch = 20,
  )
  # Plot Perth
  points(115.8605, -31.9505, cex = 7, pch = 23, bg = NA)
  title(flagged_species[[i]], cex.main = 5)
}
par(op)
dev.off()

# .... Try an ordination? ------------------------------------------------------
# (Fiddling)

SWAFR_jaccard <- vegan::vegdist(SWAFR_matrix, method = "jaccard")
SWAFR_pcoa <- ape::pcoa(SWAFR_jaccard)
SWAFR_pcoa_axes <-
  cbind(
    region = "SWAFR",
    QDS = rownames(SWAFR_pcoa$vectors),
    SWAFR_pcoa$vectors[, 1:2]
  ) %>%
  as_tibble() %>%
  mutate(
    Axis.1 = as.numeric(Axis.1),
    Axis.2 = as.numeric(Axis.2),
    lon = map_dbl(QDS, ~Larsen_grid_QDS$lon[Larsen_grid_QDS$qdgc == .]),
    lat = map_dbl(QDS, ~Larsen_grid_QDS$lat[Larsen_grid_QDS$qdgc == .]),
    vegtype = as.factor(case_when(
      (Axis.1 > 0) & (Axis.2 > 0) ~ 1,
      (Axis.1 > 0) & (Axis.2 < 0) ~ 2,
      (Axis.1 < 0) & (Axis.2 > 0) ~ 3,
      (Axis.1 < 0) & (Axis.2 < 0) ~ 4,
    )),
    vegunique = sqrt((0 - Axis.1)^2 + (0 - Axis.2)^2)  # Euclidean dist from 0,0
  )

ggplot(SWAFR_pcoa_axes) +
  aes(Axis.1, Axis.2, colour = QDS %in% Perth) +
  geom_point()

ggplot(SWAFR_pcoa_axes) +
  aes(Axis.1, Axis.2, colour = lat) +
  geom_point() +
  scale_colour_viridis_c()

ggplot(SWAFR_pcoa_axes) +
  aes(vegtype, vegunique) +
  geom_boxplot() +
  scale_colour_viridis_c()

ggplot(SWAFR_pcoa_axes) +
  aes(lon, lat, fill = vegtype) +
  geom_tile() +
  scale_fill_viridis_d()

ggplot(SWAFR_pcoa_axes) +
  aes(lon, lat, fill = vegunique) +
  geom_tile() +
  scale_fill_viridis_c()

ggplot(SWAFR_pcoa_axes) +
  aes(Axis.1, Axis.2, colour = vegunique) +
  geom_point() +
  scale_colour_viridis_c()

richness_data_QDS <- read_csv(glue("{data_dir}/richness-data-QDS.csv"))
SWAFR_pcoa_richness <- richness_data_QDS %>%
  rename(QDS = qdgc) %>%
  dplyr::select(QDS, QDS_richness) %>%
  right_join(SWAFR_pcoa_axes)

ggplot(SWAFR_pcoa_richness) +
  aes(Axis.1, Axis.2, colour = log10(QDS_richness)) +
  geom_point() +
  scale_colour_viridis_c(na.value = NA)

ggplot(SWAFR_pcoa_richness) +
  aes(QDS_richness, vegunique, colour = QDS %in% Perth) +
  geom_point() +
  scale_colour_viridis_d()

