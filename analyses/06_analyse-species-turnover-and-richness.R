# Analyse floral species turnover with richness
# (Richness (gamma) ~ mean QDS richness (alpha) * mean QDS turnover (beta))
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("analyses/01_import-region-polygons.R"))
source(here::here("analyses/02_import-floral-data.R"))

# HDS richness ~ mean QDS richness * mean QDS turnover -------------------------

# Compile data:
# GCFR
GCFR_gamma_beta_alpha_HDS_species <- compile_gamma_beta_alpha(
    cells,
    communities_by_cell_QDS$GCFR$species,
    region = "GCFR",
    focal_scale = "HDS"
)
GCFR_gamma_beta_alpha_HDS_genus <- compile_gamma_beta_alpha(
    cells,
    communities_by_cell_QDS$GCFR$genus,
    region = "GCFR",
    focal_scale = "HDS"
)
GCFR_gamma_beta_alpha_HDS_family <- compile_gamma_beta_alpha(
    cells,
    communities_by_cell_QDS$GCFR$family,
    region = "GCFR",
    focal_scale = "HDS"
)

# SWAFR
SWAFR_gamma_beta_alpha_HDS_species <- compile_gamma_beta_alpha(
    cells,
    communities_by_cell_QDS$SWAFR$species,
    region = "SWAFR",
    focal_scale = "HDS"
)
SWAFR_gamma_beta_alpha_HDS_genus <- compile_gamma_beta_alpha(
    cells,
    communities_by_cell_QDS$SWAFR$genus,
    region = "SWAFR",
    focal_scale = "HDS"
)
SWAFR_gamma_beta_alpha_HDS_family <- compile_gamma_beta_alpha(
    cells,
    communities_by_cell_QDS$SWAFR$family,
    region = "SWAFR",
    focal_scale = "HDS"
)

# Merge both regions
gamma_beta_alpha_HDS_species <- rbind(
    cbind(
        region = "GCFR",
        HDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells$HDS_cell_no[cells$region == "GCFR"]))
        ),
        GCFR_gamma_beta_alpha_HDS_species
    ),
    cbind(
        region = "SWAFR",
        HDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells$HDS_cell_no[cells$region == "SWAFR"]))
        ),
        SWAFR_gamma_beta_alpha_HDS_species
    )
)
gamma_beta_alpha_HDS_genus <- rbind(
    cbind(
        region = "GCFR",
        HDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells$HDS_cell_no[cells$region == "GCFR"]))
        ),
        GCFR_gamma_beta_alpha_HDS_genus
    ),
    cbind(
        region = "SWAFR",
        HDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells$HDS_cell_no[cells$region == "SWAFR"]))
        ),
        SWAFR_gamma_beta_alpha_HDS_genus
    )
)
gamma_beta_alpha_HDS_family <- rbind(
    cbind(
        region = "GCFR",
        HDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells$HDS_cell_no[cells$region == "GCFR"]))
        ),
        GCFR_gamma_beta_alpha_HDS_family
    ),
    cbind(
        region = "SWAFR",
        HDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells$HDS_cell_no[cells$region == "SWAFR"]))
        ),
        SWAFR_gamma_beta_alpha_HDS_family
    )
)

gamma_beta_alpha_HDS <- rbind(
    cbind(gamma_beta_alpha_HDS_species, rank = "species"),
    cbind(gamma_beta_alpha_HDS_genus,   rank = "genus"),
    cbind(gamma_beta_alpha_HDS_family,  rank = "family")
)

# Save to disc
write_csv(
    gamma_beta_alpha_HDS,
    here::here("analyses/06_outputs/gamma_beta_alpha_HDS.csv")
)

# Tidy up
rm(
    GCFR_gamma_beta_alpha_HDS_species,
    GCFR_gamma_beta_alpha_HDS_genus,
    GCFR_gamma_beta_alpha_HDS_family,

    SWAFR_gamma_beta_alpha_HDS_species,
    SWAFR_gamma_beta_alpha_HDS_genus,
    SWAFR_gamma_beta_alpha_HDS_family,

    gamma_beta_alpha_HDS_species,
    gamma_beta_alpha_HDS_genus,
    gamma_beta_alpha_HDS_family
)

# 3DS richness ~ mean QDS richness * mean QDS turnover -------------------------

# Compile data:
# GCFR
GCFR_gamma_beta_alpha_3QDS_species <- compile_gamma_beta_alpha(
    cells,
    communities_by_cell_QDS$GCFR$species,
    region = "GCFR",
    focal_scale = "threeQDS"
)
GCFR_gamma_beta_alpha_3QDS_genus <- compile_gamma_beta_alpha(
    cells,
    communities_by_cell_QDS$GCFR$genus,
    region = "GCFR",
    focal_scale = "threeQDS"
)
GCFR_gamma_beta_alpha_3QDS_family <- compile_gamma_beta_alpha(
    cells,
    communities_by_cell_QDS$GCFR$family,
    region = "GCFR",
    focal_scale = "threeQDS"
)

# SWAFR
SWAFR_gamma_beta_alpha_3QDS_species <- compile_gamma_beta_alpha(
    cells,
    communities_by_cell_QDS$SWAFR$species,
    region = "SWAFR",
    focal_scale = "threeQDS"
)
SWAFR_gamma_beta_alpha_3QDS_genus <- compile_gamma_beta_alpha(
    cells,
    communities_by_cell_QDS$SWAFR$genus,
    region = "SWAFR",
    focal_scale = "threeQDS"
)
SWAFR_gamma_beta_alpha_3QDS_family <- compile_gamma_beta_alpha(
    cells,
    communities_by_cell_QDS$SWAFR$family,
    region = "SWAFR",
    focal_scale = "threeQDS"
)

# Merge both regions
gamma_beta_alpha_3QDS_species <- rbind(
    cbind(
        region = "GCFR",
        threeQDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells$threeQDS_cell_no[cells$region == "GCFR"]))
        ),
        GCFR_gamma_beta_alpha_3QDS_species
    ),
    cbind(
        region = "SWAFR",
        threeQDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells$threeQDS_cell_no[cells$region == "SWAFR"]))
        ),
        SWAFR_gamma_beta_alpha_3QDS_species
    )
)
gamma_beta_alpha_3QDS_genus <- rbind(
    cbind(
        region = "GCFR",
        threeQDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells$threeQDS_cell_no[cells$region == "GCFR"]))
        ),
        GCFR_gamma_beta_alpha_3QDS_genus
    ),
    cbind(
        region = "SWAFR",
        threeQDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells$threeQDS_cell_no[cells$region == "SWAFR"]))
        ),
        SWAFR_gamma_beta_alpha_3QDS_genus
    )
)
gamma_beta_alpha_3QDS_family <- rbind(
    cbind(
        region = "GCFR",
        threeQDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells$threeQDS_cell_no[cells$region == "GCFR"]))
        ),
        GCFR_gamma_beta_alpha_3QDS_family
    ),
    cbind(
        region = "SWAFR",
        threeQDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells$threeQDS_cell_no[cells$region == "SWAFR"]))
        ),
        SWAFR_gamma_beta_alpha_3QDS_family
    )
)

gamma_beta_alpha_3QDS <- rbind(
    cbind(gamma_beta_alpha_3QDS_species, rank = "species"),
    cbind(gamma_beta_alpha_3QDS_genus,   rank = "genus"),
    cbind(gamma_beta_alpha_3QDS_family,  rank = "family")
)

# Save to disc
write_csv(
    gamma_beta_alpha_HDS,
    here::here("analyses/06_outputs/gamma_beta_alpha_3QDS.csv")
)

# Tidy up
rm(
    GCFR_gamma_beta_alpha_3QDS_species,
    GCFR_gamma_beta_alpha_3QDS_genus,
    GCFR_gamma_beta_alpha_3QDS_family,

    SWAFR_gamma_beta_alpha_3QDS_species,
    SWAFR_gamma_beta_alpha_3QDS_genus,
    SWAFR_gamma_beta_alpha_3QDS_family,

    gamma_beta_alpha_3QDS_species,
    gamma_beta_alpha_3QDS_genus,
    gamma_beta_alpha_3QDS_family
)

#####################
if (FALSE) {

communities_by_cell_SWAFR_QDS <- communities_by_cell_SWAFR
communities_by_cell_GCFR_QDS <- communities_by_cell_GCFR

cells_df_GCFR <- filter(cells, region == "GCFR")
DS_cell_nos <- levels(as.factor(cells_df_GCFR$DS_cell_no))
GCFR_gamma_beta_alpha_3QDS <- foreach(DS_cell_no = DS_cell_nos) %do% {
    QDS_cell_nos <- get_constituent_QDS_cells(cells_df_GCFR, DS_cell_no)
    QDS_cells <- communities_by_cell_GCFR_QDS[
        names(communities_by_cell_GCFR_QDS) %in%
        paste0("cell_", QDS_cell_nos$QDS_cell_no)
    ]
    turnover_betw_QDS_cells <-
        matrix(nrow = length(QDS_cells), ncol = length(QDS_cells))
    for (i in seq_along(QDS_cells)) {
        for (j in seq_along(QDS_cells)) {
            turnover_betw_QDS_cells[i, j] <- jaccard_distance(
                QDS_cells[[i]],
                QDS_cells[[j]]
            )
        }
    }
    return(list(
        DS_richness      = length(unique(unlist(QDS_cells))),
        avg_QDS_richness = mean(unlist(map(QDS_cells, length)), na.rm = TRUE),
        avg_QDS_turnover = mean(as.dist(turnover_betw_QDS_cells), na.rm = TRUE)
    ))
}
names(GCFR_gamma_beta_alpha_3QDS) <-
    paste0("cell_", levels(as.factor(cells_df_GCFR$DS_cell_no)))

cells_df_SWAFR <- filter(cells, region == "SWAFR")
DS_cell_nos <- levels(as.factor(cells_df_SWAFR$DS_cell_no))
SWAFR_gamma_beta_alpha_3QDS <- foreach(DS_cell_no = DS_cell_nos) %do% {
    QDS_cell_nos <- get_constituent_QDS_cells(cells_df_SWAFR, DS_cell_no)
    QDS_cells <- communities_by_cell_SWAFR_QDS[
        names(communities_by_cell_SWAFR_QDS) %in%
        paste0("cell_", QDS_cell_nos$QDS_cell_no)
    ]
    turnover_betw_QDS_cells <-
        matrix(nrow = length(QDS_cells), ncol = length(QDS_cells))
    for (i in seq_along(QDS_cells)) {
        for (j in seq_along(QDS_cells)) {
            turnover_betw_QDS_cells[i, j] <- jaccard_distance(
                QDS_cells[[i]],
                QDS_cells[[j]]
            )
        }
    }
    return(list(
        DS_richness      = length(unique(unlist(QDS_cells))),
        avg_QDS_richness = mean(unlist(map(QDS_cells, length)), na.rm = TRUE),
        avg_QDS_turnover = mean(as.dist(turnover_betw_QDS_cells), na.rm = TRUE)
    ))
}
names(SWAFR_gamma_beta_alpha_3QDS) <- paste0(
    "cell_",
    levels(as.factor(cells_df_SWAFR$DS_cell_no))
)

GCFR_gamma_beta_alpha_3QDS2 <- as.data.frame(GCFR_gamma_beta_alpha_3QDS[[1]])
foreach(cell = GCFR_gamma_beta_alpha_3QDS[-1]) %do% {
    GCFR_gamma_beta_alpha_3QDS2 %<>% rbind(as.data.frame(cell))
}
GCFR_gamma_beta_alpha_3QDS <- GCFR_gamma_beta_alpha_3QDS2

SWAFR_gamma_beta_alpha_3QDS2 <- as.data.frame(SWAFR_gamma_beta_alpha_3QDS[[1]])
foreach(cell = SWAFR_gamma_beta_alpha_3QDS[-1]) %do% {
    SWAFR_gamma_beta_alpha_3QDS2 %<>% rbind(as.data.frame(cell))
}
SWAFR_gamma_beta_alpha_3QDS <- SWAFR_gamma_beta_alpha_3QDS2

gamma_beta_alpha_3QDS <- rbind(
    cbind(
        region = "GCFR",
        DS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells_df_GCFR$DS_cell_no))
        ),
        GCFR_gamma_beta_alpha_3QDS
    ),
    cbind(
        region = "SWAFR",
        DS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells_df_SWAFR$DS_cell_no))
        ),
        SWAFR_gamma_beta_alpha_3QDS
    )
)

# Save to disc
write_csv(
    gamma_beta_alpha_3QDS,
    here::here("analyses/09_outputs/gamma_beta_alpha_3QDS.csv")
)

}

#######################

# Model gamma-beta-alpha -------------------------------------------------------

# HDS
turnover_richness_HDS_m <- step(lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover +
        region * log(avg_QDS_richness + 1) +
        region * avg_QDS_turnover,
    data = gamma_beta_alpha_HDS
))
# Region term retained!
# More evidence for value of region term:
turnover_richness_HDS_m_no_region <- lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover,
    data = gamma_beta_alpha_HDS
)
HDS_AIC_table <-
    AIC(turnover_richness_HDS_m, turnover_richness_HDS_m_no_region)

# 3QDS
turnover_richness_3QDS_m <- step(lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover +
        region * log(avg_QDS_richness + 1) +
        region * avg_QDS_turnover,
    data = gamma_beta_alpha_3QDS
))
# Region term retained!
# More evidence for value of region term:
turnover_richness_3QDS_m_no_region <- lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover,
    data = gamma_beta_alpha_3QDS
)
threeQDS_AIC_table <-
    AIC(turnover_richness_3QDS_m, turnover_richness_3QDS_m_no_region)

# Save to disc
write_rds(
    turnover_richness_HDS_m,
    here::here("analyses/06_outputs/turnover_richness_HDS_m.RDS")
)
write_rds(
    turnover_richness_3QDS_m,
    here::here("analyses/06_outputs/turnover_richness_3QDS_m.RDS")
)
write_csv(
    HDS_AIC_table,
    here::here("analyses/06_outputs/HDS_AIC_table.csv")
)
write_csv(
    threeQDS_AIC_table,
    here::here("analyses/06_outputs/threeQDS_AIC_table.csv")
)

# TODO: tidy up ls() w/ rm()
