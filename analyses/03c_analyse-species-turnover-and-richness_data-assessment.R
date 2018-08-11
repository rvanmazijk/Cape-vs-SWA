# Assessing problems in species occurence dataset
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("data/01_import-region-polygons.R"))
source(here::here("data/02_import-floral-data.R"))


# GCFR -------------------------------------------------------------------------

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
  GCFR_spp$HDS_richness[GCFR_spp$hdgc == current_HDS] <- community_matrix %>%
    colnames() %>%
    length()
  GCFR_spp$mean_QDS_turnover[GCFR_spp$hdgc == current_HDS] <- community_matrix %>%
    vegdist(method = "jaccard") %>%
    mean(na.rm = TRUE)
  GCFR_spp$mean_QDS_richness[GCFR_spp$hdgc == current_HDS] <- community_matrix %>%
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

# SWAFR ------------------------------------------------------------------------

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
  SWAFR_spp$HDS_richness[SWAFR_spp$hdgc == current_HDS] <- community_matrix %>%
    colnames() %>%
    length()
  SWAFR_spp$mean_QDS_turnover[SWAFR_spp$hdgc == current_HDS] <- community_matrix %>%
    vegdist(method = "jaccard") %>%
    mean(na.rm = TRUE)
  SWAFR_spp$mean_QDS_richness[SWAFR_spp$hdgc == current_HDS] <- community_matrix %>%
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

expect_HDS_richness <- function(richness, turnover, n) {
  richness * ((n - 1) + turnover)
}
# E.g.
# 2 site case: complete turnover
expect_HDS_richness(richness = 10, turnover = 1.0, n = 2)
# 2 site case: incomplete turnover
expect_HDS_richness(richness = 10, turnover = 0.5, n = 2)
# 4 site case: incomplete turnover
expect_HDS_richness(richness = 10, turnover = 1.0, n = 4)
# 4 site case: complete turnover
expect_HDS_richness(richness = 10, turnover = 0.5, n = 4)

richness_turnover_data %<>% mutate(expect_HDS_richness = expect_HDS_richness(
  mean_QDS_richness,
  mean_QDS_turnover,
  n = 4  # TODO: generate n_QDS in loops above, for use here
))

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

# Plots ------------------------------------------------------------------------

richness_turnover_data %>%
  ggplot(aes(
    HDS_richness,
    expect_HDS_richness,
    col = region
  )) +
    geom_point() +
    geom_smooth(method = lm) +
    geom_abline(intercept = 0, slope = 1, col = "grey50", linetype = "dashed") +
    labs(
      x = "Obs. HDS richness",
      y = "Exp. HDS richness"
    ) +
    scale_colour_manual(name = "Region", values = my_palette)

richness_turnover_data %>%
  ggplot(aes(
    mean_QDS_richness,
    HDS_richness,
    col = region
  )) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(
      x = "Mean QDS richness",
      y = "HDS richness"
    ) +
    scale_colour_manual(name = "Region", values = my_palette)

richness_turnover_data %>%
  ggplot(aes(
    mean_QDS_turnover,
    HDS_richness,
    col = region
  )) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(
      x = "Mean QDS richness",
      y = "HDS richness"
    ) +
    scale_colour_manual(name = "Region", values = my_palette)

richness_turnover_data %>%
  ggplot(aes(
    mean_QDS_turnover * mean_QDS_richness * 3,
    HDS_richness,
    col = region
  )) +
    geom_point() +
    geom_smooth(method = lm) +
    geom_abline(intercept = 0, slope = 1, col = "grey50", linetype = "dashed") +
    labs(
      x = "Mean QDS richness * mean QDS turnover",
      y = "HDS richness"
    ) +
    scale_colour_manual(name = "Region", values = my_palette)

richness_turnover_data %>%
  ggplot(aes(
    mean_QDS_turnover,
    mean_QDS_turnover * mean_QDS_richness * 3,
    col = region
  )) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(
      x = "Mean QDS turnover",
      y = "HDS richness / mean QDS richness"
    ) +
    scale_colour_manual(name = "Region", values = my_palette)
