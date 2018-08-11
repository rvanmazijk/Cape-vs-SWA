# Assessing problems in species occurence dataset
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("data/01_import-region-polygons.R"))
source(here::here("data/02_import-floral-data.R"))

# Compile data and calculate turnover and richness -----------------------------
# If not already done

# .... GCFR --------------------------------------------------------------------

GCFR_spp_path <- here::here(
  "outputs/species-turnover-and-richness/GCFR_spp_2018-08-10"
)

if (!file.exists(GCFR_spp_path)) {

  GCFR_spp <- trimmed_GCFR_clean_flora_spdf_species
  GCFR_spp@data <- tibble(
    family = trimmed_GCFR_clean_flora_spdf_family@data$family,
    genus = trimmed_GCFR_clean_flora_spdf_genus@data$genus,
    species = trimmed_GCFR_clean_flora_spdf_species@data$species
  )

  # Get the QDS and HDS geocodes
  GCFR_spp@data$qdgc <- over(GCFR_spp, GCFR_QDS[, "qdgc"])[[1]]
  GCFR_spp@data$hdgc <- map_chr(
    .x = GCFR_spp@data$qdgc,
    .f = ~ substr(.x, 1, 8)  # Drop the last letter
  )

  # Init empty columns for data to come
  GCFR_spp@data$HDS_richness <- NA
  GCFR_spp@data$mean_QDS_richness <- NA
  GCFR_spp@data$mean_QDS_turnover <- NA

  pb <- txtProgressBar(0, length(levels(factor(GCFR_spp$hdgc))))
  for (i in seq_along(levels(factor(GCFR_spp$hdgc)))) {
    # Current HDS geocode name
    current_HDS <- levels(factor(GCFR_spp$hdgc))[[i]]
    # Filter to only those QDS w/i the current HDS
    spp_in_hdgc_by_qdgc <- GCFR_spp@data %>%
      filter(hdgc == current_HDS) %>%
      select(species, qdgc)
    # Construct a community matrix containing the 4 QDS
    community_matrix <- matrix(
      nrow = length(unique(spp_in_hdgc_by_qdgc$qdgc)),
      ncol = length(unique(spp_in_hdgc_by_qdgc$species)),
      dimnames = list(
        unique(spp_in_hdgc_by_qdgc$qdgc),
        unique(spp_in_hdgc_by_qdgc$species)
      )
    )
    for (j in seq(nrow(community_matrix))) {
      for (k in seq(ncol(community_matrix))) {
        community_matrix[j, k] <-
          spp_in_hdgc_by_qdgc %>%
          filter(
            qdgc == rownames(community_matrix)[[j]] &
            species == colnames(community_matrix)[[k]]
          ) %>%
          magrittr::extract2("species") %>%
          unique() %>%
          is_empty() %>%
          not()
      }
    }
    # Output the summary data to the right rows in the spatial points data frame
    GCFR_spp$HDS_richness[GCFR_spp$hdgc == current_HDS] <-
      community_matrix %>%
      colnames() %>%
      length()
    GCFR_spp$mean_QDS_turnover[GCFR_spp$hdgc == current_HDS] <-
      community_matrix %>%
      vegdist(method = "jaccard") %>%
      mean(na.rm = TRUE)
    GCFR_spp$mean_QDS_richness[GCFR_spp$hdgc == current_HDS] <-
      community_matrix %>%
      t() %>%
      as.data.frame() %>%
      summarise_if(is.logical, function(x) length(x[x])) %>%
      t() %>%
      mean(na.rm = TRUE)
    setTxtProgressBar(pb, i)
  }
  close(pb)

  # Save
  for (layer in names(GCFR_spp)) {
    writeOGR(
      GCFR_spp,
      dsn = here::here(
        "outputs/species-turnover-and-richness/GCFR_spp_2018-08-10"
      ),
      layer = layer,
      driver = "ESRI Shapefile"
    )
  }

}

GCFR_spp <- readOGR(GCFR_spp_path)

# .... SWAFR -------------------------------------------------------------------

SWAFR_spp_path <- here::here(
  "outputs/species-turnover-and-richness/SWAFR_spp_2018-08-10"
)

if (!file.exists(SWAFR_spp_path)) {

  SWAFR_spp <- trimmed_SWAFR_clean_flora_spdf_species
  SWAFR_spp@data <- tibble(
    family = trimmed_SWAFR_clean_flora_spdf_family@data$family,
    genus = trimmed_SWAFR_clean_flora_spdf_genus@data$genus,
    species = trimmed_SWAFR_clean_flora_spdf_species@data$species
  )

  # Get the QDS and HDS geocodes
  SWAFR_spp@data$qdgc <- over(SWAFR_spp, SWAFR_QDS[, "qdgc"])[[1]]
  SWAFR_spp@data$hdgc <- map_chr(
    .x = SWAFR_spp@data$qdgc,
    .f = ~ substr(.x, 1, 8)  # Drop the last letter
  )

  # Init empty columns for data to come
  SWAFR_spp@data$HDS_richness <- NA
  SWAFR_spp@data$mean_QDS_richness <- NA
  SWAFR_spp@data$mean_QDS_turnover <- NA

  pb <- txtProgressBar(0, length(levels(factor(SWAFR_spp$hdgc))))
  for (i in seq_along(levels(factor(SWAFR_spp$hdgc)))) {
    # Current HDS geocode name
    current_HDS <- levels(factor(SWAFR_spp$hdgc))[[i]]
    # Filter to only those QDS w/i the current HDS
    spp_in_hdgc_by_qdgc <- SWAFR_spp@data %>%
      filter(hdgc == current_HDS) %>%
      select(species, qdgc)
    # Construct a community matrix containing the 4 QDS
    community_matrix <- matrix(
      nrow = length(unique(spp_in_hdgc_by_qdgc$qdgc)),
      ncol = length(unique(spp_in_hdgc_by_qdgc$species)),
      dimnames = list(
        unique(spp_in_hdgc_by_qdgc$qdgc),
        unique(spp_in_hdgc_by_qdgc$species)
      )
    )
    for (j in seq(nrow(community_matrix))) {
      for (k in seq(ncol(community_matrix))) {
        community_matrix[j, k] <-
          spp_in_hdgc_by_qdgc %>%
          filter(
            qdgc == rownames(community_matrix)[[j]] &
            species == colnames(community_matrix)[[k]]
          ) %>%
          magrittr::extract2("species") %>%
          unique() %>%
          is_empty() %>%
          not()
      }
    }
    # Output the summary data to the right rows in the spatial points data frame
    SWAFR_spp$HDS_richness[SWAFR_spp$hdgc == current_HDS] <-
      community_matrix %>%
      colnames() %>%
      length()
    SWAFR_spp$mean_QDS_turnover[SWAFR_spp$hdgc == current_HDS] <-
      community_matrix %>%
      vegdist(method = "jaccard") %>%
      mean(na.rm = TRUE)
    SWAFR_spp$mean_QDS_richness[SWAFR_spp$hdgc == current_HDS] <-
      community_matrix %>%
      t() %>%
      as.data.frame() %>%
      summarise_if(is.logical, function(x) length(x[x])) %>%
      t() %>%
      mean(na.rm = TRUE)
    setTxtProgressBar(pb, i)
  }
  close(pb)

  # Save
  for (layer in names(GCFR_spp)) {
    writeOGR(
      SWAFR_spp,
      dsn = here::here(
        "outputs/species-turnover-and-richness/SWAFR_spp_2018-08-10"
      ),
      layer = layer,
      driver = "ESRI Shapefile"
    )
  }

}

SWAFR_spp <- readOGR(SWAFR_spp_path)

# Models -----------------------------------------------------------------------

GCFR_spp_data <- GCFR_spp@data %>%
  select(hdgc, HDS_richness, mean_QDS_richness, mean_QDS_turnover) %>%
  distinct()
SWAFR_spp_data <- SWAFR_spp@data %>%
  select(hdgc, HDS_richness, mean_QDS_richness, mean_QDS_turnover) %>%
  distinct()
richness_turnover_data <- as_tibble(rbind(
  cbind(region = "Cape", GCFR_spp_data),
  cbind(region = "SWA", SWAFR_spp_data)
))


)

GCFR_model <- lm(
  HDS_richness ~ mean_QDS_richness + mean_QDS_turnover,
  data = filter(richness_turnover_data, region == "Cape")
)
GCFR_model_interaction <- lm(
  HDS_richness ~ mean_QDS_richness * mean_QDS_turnover,
  data = filter(richness_turnover_data, region == "Cape")
)
AIC(GCFR_model, GCFR_model_interaction)

SWAFR_model <- lm(
  HDS_richness ~ mean_QDS_richness + mean_QDS_turnover,
  data = filter(richness_turnover_data, region == "SWA")
)
SWAFR_model_interaction <- lm(
  HDS_richness ~ mean_QDS_richness * mean_QDS_turnover,
  data = filter(richness_turnover_data, region == "SWA")
)
AIC(SWAFR_model, SWAFR_model_interaction)

combined_model <- lm(
  HDS_richness ~ mean_QDS_richness + mean_QDS_turnover,
  data = richness_turnover_data
)
combined_model_interaction <- lm(
  HDS_richness ~ mean_QDS_richness * mean_QDS_turnover,
  data = richness_turnover_data
)
AIC(combined_model, combined_model_interaction)
