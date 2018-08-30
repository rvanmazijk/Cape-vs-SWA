# Initialise empty column for QDS richness
GCFR_species@data$QDS_richness <- NULL
for (i in seq_along(levels(factor(GCFR_species$qdgc)))) {
  # Current QDS geocode name
  current_QDS <- levels(factor(GCFR_species$qdgc))[[i]]
  # Filter to only those QDS w/i the current HDS
  spp_in_qdgc <- GCFR_species@data %>%
    filter(qdgc == current_QDS) %>%
    select(species) %>%
    as_vector()
  # Store QDS richness in data
  GCFR_species$QDS_richness[GCFR_species$qdgc == current_QDS] <-
    length(spp_in_qdgc)
}
