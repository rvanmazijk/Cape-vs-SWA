# Get all QDS geocode names
all_QDS <- levels(factor(GCFR_species$qdgc))
# Initialise empty column for QDS richness
GCFR_species@data$QDS_richness <- NULL
for (QDS in all_QDS) {
  # Filter to only those QDS w/i the current HDS
  spp_in_qdgc <- GCFR_species@data %>%
    filter(qdgc == current_QDS) %>%
    select(species) %>%
    as_vector()
  # Store QDS richness in data
  GCFR_species$QDS_richness[GCFR_species$qdgc == QDS] <- length(spp_in_qdgc)
}
