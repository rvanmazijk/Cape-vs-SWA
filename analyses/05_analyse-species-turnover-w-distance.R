# Analyse floral species turnover with geographic distance
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("analyses/01_import-region-polygons.R"))
source(here::here("analyses/03_import-floral-data.R"))

# Trim floral occurrences outside of regions -----------------------------------

# .... GCFR --------------------------------------------------------------------

# Make SpatialPointsDataFrames for species, genus, and family occurences
GCFR_clean_flora_spdf_species <- make_SpatialPointsDataFrame(
    GCFR_clean_flora,
    feature_columns = "species"
)
GCFR_clean_flora_spdf_genus <- make_SpatialPointsDataFrame(
    GCFR_clean_flora,
    feature_columns = "genus"
)
GCFR_clean_flora_spdf_family <- make_SpatialPointsDataFrame(
    GCFR_clean_flora,
    feature_columns = "family"
)

# Query their presences in the GCFR border
GCFR_point_query_species <-
    GCFR_clean_flora_spdf_species %over% GCFR_border
GCFR_point_query_genus <-
    GCFR_clean_flora_spdf_genus %over% GCFR_border
GCFR_point_query_family <-
    GCFR_clean_flora_spdf_family %over% GCFR_border
stopifnot(
    length(GCFR_clean_flora_spdf_species) == length(!is.na(GCFR_point_query_species))
)
stopifnot(
    length(GCFR_clean_flora_spdf_genus) == length(!is.na(GCFR_point_query_genus))
)
stopifnot(
    length(GCFR_clean_flora_spdf_family) == length(!is.na(GCFR_point_query_family))
)

# Trim!
trimmed_GCFR_clean_flora_spdf_species <-
    GCFR_clean_flora_spdf_species[!is.na(GCFR_point_query_species)[, 1], ]
trimmed_GCFR_clean_flora_spdf_genus <-
    GCFR_clean_flora_spdf_genus[!is.na(GCFR_point_query_genus)[, 1], ]
trimmed_GCFR_clean_flora_spdf_family <-
    GCFR_clean_flora_spdf_family[!is.na(GCFR_point_query_family)[, 1], ]

# .... SWAFR -------------------------------------------------------------------

# Make SpatialPointsDataFrames for species, genus, and family occurences
SWAFR_clean_flora_spdf_species <- make_SpatialPointsDataFrame(
    SWAFR_clean_flora,
    feature_columns = "species"
)
SWAFR_clean_flora_spdf_genus <- make_SpatialPointsDataFrame(
    SWAFR_clean_flora,
    feature_columns = "genus"
)
SWAFR_clean_flora_spdf_family <- make_SpatialPointsDataFrame(
    SWAFR_clean_flora,
    feature_columns = "family"
)

# Query their presences in the SWAFR border
SWAFR_point_query_species <-
    SWAFR_clean_flora_spdf_species %over% SWAFR_border
SWAFR_point_query_genus <-
    SWAFR_clean_flora_spdf_genus %over% SWAFR_border
SWAFR_point_query_family <-
    SWAFR_clean_flora_spdf_family %over% SWAFR_border
stopifnot(
    length(SWAFR_clean_flora_spdf_species) == length(!is.na(SWAFR_point_query_species))
)
stopifnot(
    length(SWAFR_clean_flora_spdf_genus) == length(!is.na(SWAFR_point_query_genus))
)
stopifnot(
    length(SWAFR_clean_flora_spdf_family) == length(!is.na(SWAFR_point_query_family))
)
trimmed_SWAFR_clean_flora_spdf_species <-
    SWAFR_clean_flora_spdf_species[!is.na(SWAFR_point_query_species)[, 1], ]
trimmed_SWAFR_clean_flora_spdf_genus <-
    SWAFR_clean_flora_spdf_genus[!is.na(SWAFR_point_query_genus)[, 1], ]
trimmed_SWAFR_clean_flora_spdf_family <-
    SWAFR_clean_flora_spdf_family[!is.na(SWAFR_point_query_family)[, 1], ]

# Get pixel IDs for points -----------------------------------------------------

trimmed_GCFR_clean_flora_spdf_species$cell_nos <- cellFromXY(
    GCFR_richness_QDS,
    trimmed_GCFR_clean_flora_spdf_species
)
trimmed_GCFR_clean_flora_spdf_genus$cell_nos <- cellFromXY(
    GCFR_richness_QDS,
    trimmed_GCFR_clean_flora_spdf_genus
)
trimmed_GCFR_clean_flora_spdf_family$cell_nos <- cellFromXY(
    GCFR_richness_QDS,
    trimmed_GCFR_clean_flora_spdf_family
)

trimmed_SWAFR_clean_flora_spdf_species$cell_nos <- cellFromXY(
    SWAFR_richness_QDS,
    trimmed_SWAFR_clean_flora_spdf_species
)
trimmed_SWAFR_clean_flora_spdf_genus$cell_nos <- cellFromXY(
    SWAFR_richness_QDS,
    trimmed_SWAFR_clean_flora_spdf_genus
)
trimmed_SWAFR_clean_flora_spdf_family$cell_nos <- cellFromXY(
    SWAFR_richness_QDS,
    trimmed_SWAFR_clean_flora_spdf_family
)

# Calculate all-pixel-pairwise Jaccard distances -------------------------------
# And geographical distances,
# for species, genus, and family turnover

# .... GCFR --------------------------------------------------------------------

species_turnover_geodist_GCFR <- calc_all_pw_jaccard(
    communities_by_cell = communities_by_cell_GCFR_QDS_species,
    richness_QDS = GCFR_richness_QDS,
    feature_column = "species",
    cell_nos = levels(as.factor(trimmed_GCFR_clean_flora_spdf_species$cell_nos))
)

genus_turnover_geodist_GCFR <- calc_all_pw_jaccard(
    communities_by_cell = communities_by_cell_GCFR_QDS_genus,
    richness_QDS = GCFR_richness_QDS,
    feature_column = "genus",
    cell_nos = levels(as.factor(trimmed_GCFR_clean_flora_spdf_genus$cell_nos))
)

family_species_turnover_geodist_GCFR <- calc_all_pw_jaccard(
    communities_by_cell = communities_by_cell_GCFR_QDS_family,
    richness_QDS = GCFR_richness_QDS,
    feature_column = "family",
    cell_nos = levels(as.factor(trimmed_GCFR_clean_flora_spdf_family$cell_nos))
)

# .... SWAFR -------------------------------------------------------------------

species_turnover_geodist_SWAFR <- calc_all_pw_jaccard(
    communities_by_cell = communities_by_cell_SWAFR_QDS_species,
    richness_QDS = SWAFR_richness_QDS,
    feature_column = "species",
    cell_nos = levels(as.factor(trimmed_SWAFR_clean_flora_spdf_species$cell_nos))
)

genus_turnover_geodist_SWAFR <- calc_all_pw_jaccard(
    communities_by_cell = communities_by_cell_SWAFR_QDS_genus,
    richness_QDS = SWAFR_richness_QDS,
    feature_column = "genus",
    cell_nos = levels(as.factor(trimmed_SWAFR_clean_flora_spdf_genus$cell_nos))
)

family_species_turnover_geodist_SWAFR <- calc_all_pw_jaccard(
    communities_by_cell = communities_by_cell_SWAFR_QDS_family,
    richness_QDS = SWAFR_richness_QDS,
    feature_column = "family",
    cell_nos = levels(as.factor(trimmed_SWAFR_clean_flora_spdf_family$cell_nos))
)

# Combine both regions data-frames ---------------------------------------------

# .... Species -----------------------------------------------------------------

species_turnover_geodist <- as_tibble(rbind(
    cbind(region = "GCFR", species_turnover_geodist_GCFR),
    cbind(region = "SWAFR", species_turnover_geodist_SWAFR)
))
species_turnover_geodist$region %<>% factor(levels = c("SWAFR", "GCFR"))
species_turnover_geodist %<>% filter(geodist > 0)

# Save to disc
write_csv(
    species_turnover_geodist,
    here::here("analyses/05_outputs/species_turnover_geodist.csv")
)

# .... Genus -------------------------------------------------------------------

genus_turnover_geodist <- as_tibble(rbind(
    cbind(region = "GCFR", genus_turnover_geodist_GCFR),
    cbind(region = "SWAFR", genus_turnover_geodist_SWAFR)
))
genus_turnover_geodist$region %<>% factor(levels = c("SWAFR", "GCFR"))
genus_turnover_geodist %<>% filter(geodist > 0)

# Save to disc
write_csv(
    genus_turnover_geodist,
    here::here("analyses/05_outputs/genus_turnover_geodist.csv")
)

# .... Family ------------------------------------------------------------------

family_turnover_geodist <- as_tibble(rbind(
    cbind(region = "GCFR", family_turnover_geodist_GCFR),
    cbind(region = "SWAFR", family_turnover_geodist_SWAFR)
))
family_turnover_geodist$region %<>% factor(levels = c("SWAFR", "GCFR"))
family_turnover_geodist %<>% filter(geodist > 0)

# Save to disc
write_csv(
    family_turnover_geodist,
    here::here("analyses/05_outputs/family_turnover_geodist.csv")
)

# Model ------------------------------------------------------------------------

# .... Species -----------------------------------------------------------------

species_turnover_geodist_m <- species_turnover_geodist %>%
    filter(geodist > 0) %>%
    rq(turnover ~ log(geodist) * region, 0.05, .)

# Save to disc
write_rds(
    species_turnover_geodist_m,
    here::here("analyses/05_outputs/species_turnover_geodist_m.RDS")
)

# .... Genus -------------------------------------------------------------------

genus_turnover_geodist_m <- genus_turnover_geodist %>%
    filter(geodist > 0) %>%
    rq(turnover ~ log(geodist) * region, 0.05, .)

# Save to disc
write_rds(
    genus_turnover_geodist_m,
    here::here("analyses/05_outputs/genus_turnover_geodist_m.RDS")
)

# .... Family ------------------------------------------------------------------

family_turnover_geodist_m <- family_turnover_geodist %>%
    filter(geodist > 0) %>%
    rq(turnover ~ log(geodist) * region, 0.05, .)

# Save to disc
write_rds(
    family_turnover_geodist_m,
    here::here("analyses/05_outputs/family_turnover_geodist_m.RDS")
)
