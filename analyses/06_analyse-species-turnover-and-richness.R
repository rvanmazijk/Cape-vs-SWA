# Analyse floral species turnover with richness
# (HDS richness ~ QDS richness * QDS turnover)
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("analyses/01_import-region-polygons.R"))
source(here::here("analyses/03_import-floral-data.R"))

# Pseudo-code-plan -------------------------------------------------------------

# >     GCFR_QDS_by_HDS <- foreach(HDS = GCFR_HDS_cell_nos) %do% {
# >         QDS_cells <- get_constituent_QDS_cells(HDS)
# >         turnover_betw_QDS_cells <-
# >             matrix(nrow = length(QDS_cells), ncol = length(QDS_cells))
# >         for (i in seq_along(QDS_cells)) {
# >             for (j in seq_along(QDS_cells)) {
# >                 turnover_betw_QDS_cells[i, j] <-
# >                     jaccard_distance(QDS_cells[[i]], QDS_cells[[j]])
# >             }
# >         }
# >         return(list(
# >             HDS_richness = length(unique(HDS$species)),
# >             avg_QDS_richness = mean(length(unique(QDS_cells$species))),
# >             avg_QDS_turnover = mean(triangulate(turnover_betw_QDS_cells))
# >         ))
# >     }

# Implementation of above for HDS & QDS ----------------------------------------

communities_by_cell_SWAFR_QDS <- compile_communities_by_cell(
    trimmed_SWAFR_clean_flora_spdf_species,
    "species"
)
species_turnover_geodist_SWAFR <- calc_all_pw_jaccard(
    communities_by_cell = communities_by_cell_SWAFR_QDS,
    richness_QDS = SWAFR_richness_QDS,
    feature_column = "species",
    cell_nos = levels(as.factor(trimmed_SWAFR_clean_flora_spdf_species$cell_nos))
)


communities_by_cell_GCFR_QDS <- compile_communities_by_cell(
    trimmed_GCFR_clean_flora_spdf_species,
    "species"
)

cells_df_GCFR <- filter(cells, region == "GCFR")
HDS_cell_nos <- levels(as.factor(cells_df_GCFR$HDS_cell_no))
GCFR_gamma_beta_alpha <- foreach(HDS_cell_no = HDS_cell_nos) %do% {
    QDS_cell_nos <- get_constituent_QDS_cells(cells_df_GCFR, HDS_cell_no)
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
        HDS_richness     = length(unique(unlist(QDS_cells))),
        avg_QDS_richness = mean(unlist(map(QDS_cells, length)), na.rm = TRUE),
        avg_QDS_turnover = mean(as.dist(turnover_betw_QDS_cells), na.rm = TRUE)
    ))
}
names(GCFR_gamma_beta_alpha) <-
    paste0("cell_", levels(as.factor(cells_df_GCFR$HDS_cell_no)))

cells_df_SWAFR <- filter(cells, region == "SWAFR")
HDS_cell_nos <- levels(as.factor(cells_df_SWAFR$HDS_cell_no))
SWAFR_gamma_beta_alpha <- foreach(HDS_cell_no = HDS_cell_nos) %do% {
    QDS_cell_nos <- get_constituent_QDS_cells(cells_df_SWAFR, HDS_cell_no)
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
        HDS_richness     = length(unique(unlist(QDS_cells))),
        avg_QDS_richness = mean(unlist(map(QDS_cells, length)), na.rm = TRUE),
        avg_QDS_turnover = mean(as.dist(turnover_betw_QDS_cells), na.rm = TRUE)
    ))
}
names(SWAFR_gamma_beta_alpha) <- paste0(
    "cell_",
    levels(as.factor(cells_df_SWAFR$HDS_cell_no))
)

GCFR_gamma_beta_alpha2 <- as.data.frame(GCFR_gamma_beta_alpha[[1]])
foreach(cell = GCFR_gamma_beta_alpha[-1]) %do% {
    GCFR_gamma_beta_alpha2 %<>% rbind(as.data.frame(cell))
}
GCFR_gamma_beta_alpha <- GCFR_gamma_beta_alpha2

SWAFR_gamma_beta_alpha2 <- as.data.frame(SWAFR_gamma_beta_alpha[[1]])
foreach(cell = SWAFR_gamma_beta_alpha[-1]) %do% {
    SWAFR_gamma_beta_alpha2 %<>% rbind(as.data.frame(cell))
}
SWAFR_gamma_beta_alpha <- SWAFR_gamma_beta_alpha2

gamma_beta_alpha <- rbind(
    cbind(
        region = "GCFR",
        HDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells_df_GCFR$HDS_cell_no))
        ),
        GCFR_gamma_beta_alpha
    ),
    cbind(
        region = "SWAFR",
        HDS_cell_no = paste0(
            "cell_",
            levels(as.factor(cells_df_SWAFR$HDS_cell_no))
        ),
        SWAFR_gamma_beta_alpha
    )
)

# Save to disc
write_csv(
    gamma_beta_alpha,
    here::here("analyses/09_outputs/gamma_beta_alpha.csv")
)

# Implementation of above for 3QDS & QDS ---------------------------------------

get_constituent_QDS_cells <- function(cells_df, query_3QDS_cell_no) {
    return(filter(
        cells_df,
        DS_cell_no == query_3QDS_cell_no
    ))
}

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

# Model gamma-beta-alpha -------------------------------------------------------

# HDS
turnover_richness_HDS_m <- step(lm(
    HDS_richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover +
        region * log(avg_QDS_richness + 1) +
        region * avg_QDS_turnover,
    data = gamma_beta_alpha
))
# Region term retained!
# More evidence for value of region term:
turnover_richness_HDS_m_no_region <- lm(
    HDS_richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover,
    data = gamma_beta_alpha
)
HDS_AIC_table <-
    AIC(turnover_richness_HDS_m, turnover_richness_HDS_m_no_region)

# 3QDS
turnover_richness_3QDS_m <- step(lm(
    DS_richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover +
        region * log(avg_QDS_richness + 1) +
        region * avg_QDS_turnover,
    data = gamma_beta_alpha_3QDS
))
# Region term retained!
# More evidence for value of region term:
turnover_richness_3QDS_m_no_region <- lm(
    DS_richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover,
    data = gamma_beta_alpha_3QDS
)
threeQDS_AIC_table <-
    AIC(turnover_richness_3QDS_m, turnover_richness_3QDS_m_no_region)

# Save to disc
write_rds(
    turnover_richness_HDS_m,
    here::here("analyses/09_outputs/turnover_richness_HDS_m.RDS")
)
write_rds(
    turnover_richness_3QDS_m,
    here::here("analyses/09_outputs/turnover_richness_3QDS_m.RDS")
)
write_csv(
    HDS_AIC_table,
    here::here("analyses/09_outputs/HDS_AIC_table.csv")
)
write_csv(
    threeQDS_AIC_table,
    here::here("analyses/09_outputs/threeQDS_AIC_table.csv")
)
