# Analyse floral species turnover with geographic distance
# Cape vs SWA publication
# Ruan van Mazijk

# Trim floral occurrences outside of regions -----------------------------------

make_SpatialPointsDataFrame <- function(df,
                                        lat_column = "decimallatitude",
                                        lon_column = "decimallongitude",
                                        feature_columns = c("family",
                                                            "genus",
                                                            "species"),
                                        CRS = std_CRS) {
    return(SpatialPointsDataFrame(
        coords = df[, c(lon_column, lat_column)],
        data = df[, c(feature_columns)],
        proj4string = CRS(CRS)
    ))
}

# GCFR -------------------------------------------------------------------------

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

# SWAFR ------------------------------------------------------------------------

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

# GCFR: species ----------------------------------------------------------------

cell_nos <- levels(as.factor(trimmed_GCFR_clean_flora_spdf_species$cell_nos))

communities_by_cell_GCFR <- vector("list", length = length(cell_nos))
names(communities_by_cell_GCFR) <- paste0("cell_", cell_nos)
for (i in seq_along(cell_nos)) {
    communities_by_cell_GCFR[[i]] <-
        trimmed_GCFR_clean_flora_spdf_species$species[trimmed_GCFR_clean_flora_spdf_species$cell_nos == cell_nos[[i]]]
    #print(i)
}

jaccard_distance <- function(a, b) {
    (length(dplyr::union(a, b)) - length(dplyr::intersect(a, b))) /
    length(dplyr::union(a, b))
}
turnovers_betw_cells_GCFR <-
    matrix(nrow = length(cell_nos), ncol = length(cell_nos))
rownames(turnovers_betw_cells_GCFR) <- paste0("cell_", cell_nos)
colnames(turnovers_betw_cells_GCFR) <- paste0("cell_", cell_nos)
for (i in seq_along(cell_nos)) {
    for (j in seq_along(cell_nos)) {
        turnovers_betw_cells_GCFR[i, j] <- jaccard_distance(
            communities_by_cell_GCFR[[i]],
            communities_by_cell_GCFR[[j]]
        )
    }
    #print(glue("
    #    All distances against cell no. {cell_nos[[i]]} complete.
    #"))
}

geodists_betw_cells_GCFR <- pointDistance(
    p1 = xyFromCell(GCFR_richness_QDS, cell = as.numeric(cell_nos)),
    p2 = xyFromCell(GCFR_richness_QDS, cell = as.numeric(cell_nos)),
    lonlat = TRUE,
    allpairs = TRUE
)

species_turnovers_betw_cells_GCFR_df <- as_tibble(turnovers_betw_cells_GCFR)
species_turnovers_betw_cells_GCFR_df$cell_no_a <- colnames(turnovers_betw_cells_GCFR)
species_turnovers_betw_cells_GCFR_df %<>% gather(cell_no_b, turnover,
                                                 -cell_no_a)

geodists_betw_cells_GCFR_df <- as_tibble(geodists_betw_cells_GCFR)
colnames(geodists_betw_cells_GCFR_df) <- colnames(turnovers_betw_cells_GCFR)
geodists_betw_cells_GCFR_df$cell_no_a <- colnames(turnovers_betw_cells_GCFR)
geodists_betw_cells_GCFR_df %<>% gather(cell_no_b, geodist,
                                        -cell_no_a)

species_turnover_geodist_betw_cells_GCFR_df <- full_join(
    species_turnovers_betw_cells_GCFR_df,
    geodists_betw_cells_GCFR_df
)

# GCFR: genus ------------------------------------------------------------------

cell_nos <- levels(as.factor(trimmed_GCFR_clean_flora_spdf_genus$cell_nos))

communities_by_cell_GCFR <- vector("list", length = length(cell_nos))
names(communities_by_cell_GCFR) <- paste0("cell_", cell_nos)
for (i in seq_along(cell_nos)) {
    communities_by_cell_GCFR[[i]] <-
        trimmed_GCFR_clean_flora_spdf_genus$genus[trimmed_GCFR_clean_flora_spdf_genus$cell_nos == cell_nos[[i]]]
    #print(i)
}

jaccard_distance <- function(a, b) {
    (length(dplyr::union(a, b)) - length(dplyr::intersect(a, b))) /
    length(dplyr::union(a, b))
}
turnovers_betw_cells_GCFR <-
    matrix(nrow = length(cell_nos), ncol = length(cell_nos))
rownames(turnovers_betw_cells_GCFR) <- paste0("cell_", cell_nos)
colnames(turnovers_betw_cells_GCFR) <- paste0("cell_", cell_nos)
for (i in seq_along(cell_nos)) {
    for (j in seq_along(cell_nos)) {
        turnovers_betw_cells_GCFR[i, j] <- jaccard_distance(
            communities_by_cell_GCFR[[i]],
            communities_by_cell_GCFR[[j]]
        )
    }
    #print(glue("
    #    All distances against cell no. {cell_nos[[i]]} complete.
    #"))
}

geodists_betw_cells_GCFR <- pointDistance(
    p1 = xyFromCell(GCFR_richness_QDS, cell = as.numeric(cell_nos)),
    p2 = xyFromCell(GCFR_richness_QDS, cell = as.numeric(cell_nos)),
    lonlat = TRUE,
    allpairs = TRUE
)

genus_turnovers_betw_cells_GCFR_df <- as_tibble(turnovers_betw_cells_GCFR)
genus_turnovers_betw_cells_GCFR_df$cell_no_a <- colnames(turnovers_betw_cells_GCFR)
genus_turnovers_betw_cells_GCFR_df %<>% gather(cell_no_b, turnover,
                                               -cell_no_a)

geodists_betw_cells_GCFR_df <- as_tibble(geodists_betw_cells_GCFR)
colnames(geodists_betw_cells_GCFR_df) <- colnames(turnovers_betw_cells_GCFR)
geodists_betw_cells_GCFR_df$cell_no_a <- colnames(turnovers_betw_cells_GCFR)
geodists_betw_cells_GCFR_df %<>% gather(cell_no_b, geodist,
                                        -cell_no_a)

genus_turnover_geodist_betw_cells_GCFR_df <- full_join(
    genus_turnovers_betw_cells_GCFR_df,
    geodists_betw_cells_GCFR_df
)

# GCFR: family -----------------------------------------------------------------

cell_nos <- levels(as.factor(trimmed_GCFR_clean_flora_spdf_family$cell_nos))

communities_by_cell_GCFR <- vector("list", length = length(cell_nos))
names(communities_by_cell_GCFR) <- paste0("cell_", cell_nos)
for (i in seq_along(cell_nos)) {
    communities_by_cell_GCFR[[i]] <-
        trimmed_GCFR_clean_flora_spdf_family$family[trimmed_GCFR_clean_flora_spdf_family$cell_nos == cell_nos[[i]]]
    #print(i)
}

jaccard_distance <- function(a, b) {
    (length(dplyr::union(a, b)) - length(dplyr::intersect(a, b))) /
    length(dplyr::union(a, b))
}
turnovers_betw_cells_GCFR <-
    matrix(nrow = length(cell_nos), ncol = length(cell_nos))
rownames(turnovers_betw_cells_GCFR) <- paste0("cell_", cell_nos)
colnames(turnovers_betw_cells_GCFR) <- paste0("cell_", cell_nos)
for (i in seq_along(cell_nos)) {
    for (j in seq_along(cell_nos)) {
        turnovers_betw_cells_GCFR[i, j] <- jaccard_distance(
            communities_by_cell_GCFR[[i]],
            communities_by_cell_GCFR[[j]]
        )
    }
    #print(glue("
    #    All distances against cell no. {cell_nos[[i]]} complete.
    #"))
}

geodists_betw_cells_GCFR <- pointDistance(
    p1 = xyFromCell(GCFR_richness_QDS, cell = as.numeric(cell_nos)),
    p2 = xyFromCell(GCFR_richness_QDS, cell = as.numeric(cell_nos)),
    lonlat = TRUE,
    allpairs = TRUE
)

family_turnovers_betw_cells_GCFR_df <- as_tibble(turnovers_betw_cells_GCFR)
family_turnovers_betw_cells_GCFR_df$cell_no_a <- colnames(turnovers_betw_cells_GCFR)
family_turnovers_betw_cells_GCFR_df %<>% gather(cell_no_b, turnover,
                                                -cell_no_a)

geodists_betw_cells_GCFR_df <- as_tibble(geodists_betw_cells_GCFR)
colnames(geodists_betw_cells_GCFR_df) <- colnames(turnovers_betw_cells_GCFR)
geodists_betw_cells_GCFR_df$cell_no_a <- colnames(turnovers_betw_cells_GCFR)
geodists_betw_cells_GCFR_df %<>% gather(cell_no_b, geodist,
                                        -cell_no_a)

family_turnover_geodist_betw_cells_GCFR_df <- full_join(
    family_turnovers_betw_cells_GCFR_df,
    geodists_betw_cells_GCFR_df
)

# SWAFR: species ---------------------------------------------------------------

cell_nos <- levels(as.factor(trimmed_SWAFR_clean_flora_spdf_species$cell_nos))

communities_by_cell_SWAFR <- vector("list", length = length(cell_nos))
names(communities_by_cell_SWAFR) <- paste0("cell_", cell_nos)
for (i in seq_along(cell_nos)) {
    communities_by_cell_SWAFR[[i]] <-
        trimmed_SWAFR_clean_flora_spdf_species$species[trimmed_SWAFR_clean_flora_spdf_species$cell_nos == cell_nos[[i]]]
    #print(i)
}

jaccard_distance <- function(a, b) {
    (length(dplyr::union(a, b)) - length(dplyr::intersect(a, b))) /
    length(dplyr::union(a, b))
}
turnovers_betw_cells_SWAFR <-
    matrix(nrow = length(cell_nos), ncol = length(cell_nos))
rownames(turnovers_betw_cells_SWAFR) <- paste0("cell_", cell_nos)
colnames(turnovers_betw_cells_SWAFR) <- paste0("cell_", cell_nos)
for (i in seq_along(cell_nos)) {
    for (j in seq_along(cell_nos)) {
        turnovers_betw_cells_SWAFR[i, j] <- jaccard_distance(
            communities_by_cell_SWAFR[[i]],
            communities_by_cell_SWAFR[[j]]
        )
    }
    #print(glue("
    #    All distances against cell no. {cell_nos[[i]]} complete.
    #"))
}

geodists_betw_cells_SWAFR <- pointDistance(
    p1 = xyFromCell(SWAFR_richness_QDS, cell = as.numeric(cell_nos)),
    p2 = xyFromCell(SWAFR_richness_QDS, cell = as.numeric(cell_nos)),
    lonlat = TRUE,
    allpairs = TRUE
)

species_turnovers_betw_cells_SWAFR_df <- as_tibble(turnovers_betw_cells_SWAFR)
species_turnovers_betw_cells_SWAFR_df$cell_no_a <- colnames(turnovers_betw_cells_SWAFR)
species_turnovers_betw_cells_SWAFR_df %<>% gather(cell_no_b, turnover,
                                                  -cell_no_a)

geodists_betw_cells_SWAFR_df <- as_tibble(geodists_betw_cells_SWAFR)
colnames(geodists_betw_cells_SWAFR_df) <- colnames(turnovers_betw_cells_SWAFR)
geodists_betw_cells_SWAFR_df$cell_no_a <- colnames(turnovers_betw_cells_SWAFR)
geodists_betw_cells_SWAFR_df %<>% gather(cell_no_b, geodist,
                                         -cell_no_a)

species_turnover_geodist_betw_cells_SWAFR_df <- full_join(
    species_turnovers_betw_cells_SWAFR_df,
    geodists_betw_cells_SWAFR_df
)

# SWAFR: genus -----------------------------------------------------------------

cell_nos <- levels(as.factor(trimmed_SWAFR_clean_flora_spdf_genus$cell_nos))

communities_by_cell_SWAFR <- vector("list", length = length(cell_nos))
names(communities_by_cell_SWAFR) <- paste0("cell_", cell_nos)
for (i in seq_along(cell_nos)) {
    communities_by_cell_SWAFR[[i]] <-
        trimmed_SWAFR_clean_flora_spdf_genus$genus[trimmed_SWAFR_clean_flora_spdf_genus$cell_nos == cell_nos[[i]]]
    #print(i)
}

jaccard_distance <- function(a, b) {
    (length(dplyr::union(a, b)) - length(dplyr::intersect(a, b))) /
    length(dplyr::union(a, b))
}
turnovers_betw_cells_SWAFR <-
    matrix(nrow = length(cell_nos), ncol = length(cell_nos))
rownames(turnovers_betw_cells_SWAFR) <- paste0("cell_", cell_nos)
colnames(turnovers_betw_cells_SWAFR) <- paste0("cell_", cell_nos)
for (i in seq_along(cell_nos)) {
    for (j in seq_along(cell_nos)) {
        turnovers_betw_cells_SWAFR[i, j] <- jaccard_distance(
            communities_by_cell_SWAFR[[i]],
            communities_by_cell_SWAFR[[j]]
        )
    }
    #print(glue("
    #    All distances against cell no. {cell_nos[[i]]} complete.
    #"))
}

geodists_betw_cells_SWAFR <- pointDistance(
    p1 = xyFromCell(SWAFR_richness_QDS, cell = as.numeric(cell_nos)),
    p2 = xyFromCell(SWAFR_richness_QDS, cell = as.numeric(cell_nos)),
    lonlat = TRUE,
    allpairs = TRUE
)

genus_turnovers_betw_cells_SWAFR_df <- as_tibble(turnovers_betw_cells_SWAFR)
genus_turnovers_betw_cells_SWAFR_df$cell_no_a <- colnames(turnovers_betw_cells_SWAFR)
genus_turnovers_betw_cells_SWAFR_df %<>% gather(cell_no_b, turnover,
                                                -cell_no_a)

geodists_betw_cells_SWAFR_df <- as_tibble(geodists_betw_cells_SWAFR)
colnames(geodists_betw_cells_SWAFR_df) <- colnames(turnovers_betw_cells_SWAFR)
geodists_betw_cells_SWAFR_df$cell_no_a <- colnames(turnovers_betw_cells_SWAFR)
geodists_betw_cells_SWAFR_df %<>% gather(cell_no_b, geodist,
                                         -cell_no_a)

genus_turnover_geodist_betw_cells_SWAFR_df <- full_join(
    genus_turnovers_betw_cells_SWAFR_df,
    geodists_betw_cells_SWAFR_df
)

# SWAFR: family ----------------------------------------------------------------

cell_nos <- levels(as.factor(trimmed_SWAFR_clean_flora_spdf_family$cell_nos))

communities_by_cell_SWAFR <- vector("list", length = length(cell_nos))
names(communities_by_cell_SWAFR) <- paste0("cell_", cell_nos)
for (i in seq_along(cell_nos)) {
    communities_by_cell_SWAFR[[i]] <-
 trimmed_SWAFR_clean_flora_spdf_family$family[trimmed_SWAFR_clean_flora_spdf_family$cell_nos == cell_nos[[i]]]
    #print(i)
}

jaccard_distance <- function(a, b) {
    (length(dplyr::union(a, b)) - length(dplyr::intersect(a, b))) /
    length(dplyr::union(a, b))
}
turnovers_betw_cells_SWAFR <-
    matrix(nrow = length(cell_nos), ncol = length(cell_nos))
rownames(turnovers_betw_cells_SWAFR) <- paste0("cell_", cell_nos)
colnames(turnovers_betw_cells_SWAFR) <- paste0("cell_", cell_nos)
for (i in seq_along(cell_nos)) {
    for (j in seq_along(cell_nos)) {
        turnovers_betw_cells_SWAFR[i, j] <- jaccard_distance(
            communities_by_cell_SWAFR[[i]],
            communities_by_cell_SWAFR[[j]]
        )
    }
    #print(glue("
    #    All distances against cell no. {cell_nos[[i]]} complete.
    #"))
}

geodists_betw_cells_SWAFR <- pointDistance(
    p1 = xyFromCell(SWAFR_richness_QDS, cell = as.numeric(cell_nos)),
    p2 = xyFromCell(SWAFR_richness_QDS, cell = as.numeric(cell_nos)),
    lonlat = TRUE,
    allpairs = TRUE
)

family_turnovers_betw_cells_SWAFR_df <- as_tibble(turnovers_betw_cells_SWAFR)
family_turnovers_betw_cells_SWAFR_df$cell_no_a <- colnames(turnovers_betw_cells_SWAFR)
family_turnovers_betw_cells_SWAFR_df %<>% gather(cell_no_b, turnover,
                                                 -cell_no_a)

geodists_betw_cells_SWAFR_df <- as_tibble(geodists_betw_cells_SWAFR)
colnames(geodists_betw_cells_SWAFR_df) <- colnames(turnovers_betw_cells_SWAFR)
geodists_betw_cells_SWAFR_df$cell_no_a <- colnames(turnovers_betw_cells_SWAFR)
geodists_betw_cells_SWAFR_df %<>% gather(cell_no_b, geodist,
                                         -cell_no_a)

family_turnover_geodist_betw_cells_SWAFR_df <- full_join(
    family_turnovers_betw_cells_SWAFR_df,
    geodists_betw_cells_SWAFR_df
)

# Combine both regions dfs: species --------------------------------------------

species_turnover_geodist_betw_cells_df <- as_tibble(rbind(
    cbind(region = "GCFR", species_turnover_geodist_betw_cells_GCFR_df),
    cbind(region = "SWAFR", species_turnover_geodist_betw_cells_SWAFR_df)
))
species_turnover_geodist_betw_cells_df$region %<>% factor(levels = c("SWAFR", "GCFR"))
species_turnover_geodist_betw_cells_df %<>% filter(geodist > 0)

# Save to disc
write_csv(
    species_turnover_geodist_betw_cells_df,
    here::here("analyses/05_outputs/species_turnover_geodist_betw_cells_df.csv")
)

# Combine both regions dfs: genus ----------------------------------------------

genus_turnover_geodist_betw_cells_df <- as_tibble(rbind(
    cbind(region = "GCFR", genus_turnover_geodist_betw_cells_GCFR_df),
    cbind(region = "SWAFR", genus_turnover_geodist_betw_cells_SWAFR_df)
))
genus_turnover_geodist_betw_cells_df$region %<>% factor(levels = c("SWAFR", "GCFR"))
genus_turnover_geodist_betw_cells_df %<>% filter(geodist > 0)

# Save to disc
write_csv(
    genus_turnover_geodist_betw_cells_df,
    here::here("analyses/05_outputs/genus_turnover_geodist_betw_cells_df.csv")
)

# Combine both regions dfs: family ---------------------------------------------

family_turnover_geodist_betw_cells_df <- as_tibble(rbind(
    cbind(region = "GCFR", family_turnover_geodist_betw_cells_GCFR_df),
    cbind(region = "SWAFR", family_turnover_geodist_betw_cells_SWAFR_df)
))
family_turnover_geodist_betw_cells_df$region %<>% factor(levels = c("SWAFR", "GCFR"))
family_turnover_geodist_betw_cells_df %<>% filter(geodist > 0)

# Save to disc
write_csv(
    family_turnover_geodist_betw_cells_df,
    here::here("analyses/05_outputs/family_turnover_geodist_betw_cells_df.csv")
)

# Model: species ---------------------------------------------------------------

species_turnover_geodist_m <- species_turnover_geodist_betw_cells_df %>%
    filter(geodist > 0) %>%
    rq(turnover ~ log(geodist) * region, 0.05, .)

# Save to disc
write_rds(
    species_turnover_geodist_m,
    here::here("analyses/05_outputs/species_turnover_geodist_m.RDS")
)

# Model: genus -----------------------------------------------------------------

genus_turnover_geodist_m <- genus_turnover_geodist_betw_cells_df %>%
    filter(geodist > 0) %>%
    rq(turnover ~ log(geodist) * region, 0.05, .)

# Save to disc
write_rds(
    genus_turnover_geodist_m,
    here::here("analyses/05_outputs/genus_turnover_geodist_m.RDS")
)

# Model: family ----------------------------------------------------------------

family_turnover_geodist_m <- family_turnover_geodist_betw_cells_df %>%
    filter(geodist > 0) %>%
    rq(turnover ~ log(geodist) * region, 0.05, .)

# Save to disc
write_rds(
    family_turnover_geodist_m,
    here::here("analyses/05_outputs/family_turnover_geodist_m.RDS")
)
