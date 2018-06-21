# Analyse floral species turnover with richness
# (Richness (gamma) ~ mean QDS richness (alpha) * mean QDS turnover (beta))
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("analyses/01_import-region-polygons.R"))
source(here::here("analyses/02_import-floral-data.R"))

# HDS richness ~ mean QDS richness * mean QDS turnover -------------------------

# .... Compile data ------------------------------------------------------------

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

# .... Merge both regions ------------------------------------------------------

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

# .... Merge across ranks, and save --------------------------------------------

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

# 3QDS richness ~ mean QDS richness * mean QDS turnover ------------------------

# .... Compile data ------------------------------------------------------------

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

# .... Merge both regions ------------------------------------------------------

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
    gamma_beta_alpha_3QDS,
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

# Model gamma-beta-alpha -------------------------------------------------------

# .... HDS ---------------------------------------------------------------------

species_turnover_richness_HDS_m <- step(lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover +
        region * log(avg_QDS_richness + 1) +
        region * avg_QDS_turnover,
    data = gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "species", ]
))
genus_turnover_richness_HDS_m <- step(lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover +
        region * log(avg_QDS_richness + 1) +
        region * avg_QDS_turnover,
    data = gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "genus", ]
))
family_turnover_richness_HDS_m <- step(lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover +
        region * log(avg_QDS_richness + 1) +
        region * avg_QDS_turnover,
    data = gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "family", ]
))
# Region terms retained!
# More evidence for value of region term:
species_turnover_richness_HDS_m_no_region <- lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover,
    data = gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "species", ]
)
genus_turnover_richness_HDS_m_no_region <- lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover,
    data = gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "genus", ]
)
family_turnover_richness_HDS_m_no_region <- lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover,
    data = gamma_beta_alpha_HDS[gamma_beta_alpha_HDS$rank == "family", ]
)
species_HDS_AIC_table <-
    AIC(species_turnover_richness_HDS_m, species_turnover_richness_HDS_m_no_region)
genus_HDS_AIC_table <-
    AIC(genus_turnover_richness_HDS_m, genus_turnover_richness_HDS_m_no_region)
family_HDS_AIC_table <-
    AIC(family_turnover_richness_HDS_m, family_turnover_richness_HDS_m_no_region)

# .... 3QDS --------------------------------------------------------------------

species_turnover_richness_3QDS_m <- step(lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover +
        region * log(avg_QDS_richness + 1) +
        region * avg_QDS_turnover,
    data = gamma_beta_alpha_3QDS[gamma_beta_alpha_3QDS$rank == "species", ]
))
genus_turnover_richness_3QDS_m <- step(lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover +
        region * log(avg_QDS_richness + 1) +
        region * avg_QDS_turnover,
    data = gamma_beta_alpha_3QDS[gamma_beta_alpha_3QDS$rank == "genus", ]
))
family_turnover_richness_3QDS_m <- step(lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover +
        region * log(avg_QDS_richness + 1) +
        region * avg_QDS_turnover,
    data = gamma_beta_alpha_3QDS[gamma_beta_alpha_3QDS$rank == "family", ]
))
# Region terms retained!
# More evidence for value of region term:
species_turnover_richness_3QDS_m_no_region <- lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover,
    data = gamma_beta_alpha_3QDS[gamma_beta_alpha_3QDS$rank == "species", ]
)
genus_turnover_richness_3QDS_m_no_region <- lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover,
    data = gamma_beta_alpha_3QDS[gamma_beta_alpha_3QDS$rank == "genus", ]
)
family_turnover_richness_3QDS_m_no_region <- lm(
    richness ~
        log(avg_QDS_richness + 1) + avg_QDS_turnover,
    data = gamma_beta_alpha_3QDS[gamma_beta_alpha_3QDS$rank == "family", ]
)
species_3QDS_AIC_table <-
    AIC(species_turnover_richness_3QDS_m, species_turnover_richness_3QDS_m_no_region)
genus_3QDS_AIC_table <-
    AIC(genus_turnover_richness_3QDS_m, genus_turnover_richness_3QDS_m_no_region)
family_3QDS_AIC_table <-
    AIC(family_turnover_richness_3QDS_m, family_turnover_richness_3QDS_m_no_region)

# .... Save to disc ------------------------------------------------------------

write_rds(
    species_turnover_richness_HDS_m,
    here::here("analyses/06_outputs/species_turnover_richness_HDS_m.RDS")
)
write_rds(
    genus_turnover_richness_HDS_m,
    here::here("analyses/06_outputs/genus_turnover_richness_HDS_m.RDS")
)
write_rds(
    family_turnover_richness_HDS_m,
    here::here("analyses/06_outputs/family_turnover_richness_HDS_m.RDS")
)
write_rds(
    species_turnover_richness_3QDS_m,
    here::here("analyses/06_outputs/species_turnover_richness_3QDS_m.RDS")
)
write_rds(
    genus_turnover_richness_3QDS_m,
    here::here("analyses/06_outputs/genus_turnover_richness_3QDS_m.RDS")
)
write_rds(
    family_turnover_richness_3QDS_m,
    here::here("analyses/06_outputs/family_turnover_richness_3QDS_m.RDS")
)
write_csv(
    species_HDS_AIC_table,
    here::here("analyses/06_outputs/species_HDS_AIC_table.csv")
)
write_csv(
    genus_HDS_AIC_table,
    here::here("analyses/06_outputs/genus_HDS_AIC_table.csv")
)
write_csv(
    family_HDS_AIC_table,
    here::here("analyses/06_outputs/family_HDS_AIC_table.csv")
)
write_csv(
    species_3QDS_AIC_table,
    here::here("analyses/06_outputs/species_3QDS_AIC_table.csv")
)
write_csv(
    genus_3QDS_AIC_table,
    here::here("analyses/06_outputs/genus_3QDS_AIC_table.csv")
)
write_csv(
    family_3QDS_AIC_table,
    here::here("analyses/06_outputs/family_3QDS_AIC_table.csv")
)

# Tidy up
rm(
    species_turnover_richness_HDS_m,
    genus_turnover_richness_HDS_m,
    family_turnover_richness_HDS_m,
    species_turnover_richness_3QDS_m,
    genus_turnover_richness_3QDS_m,
    family_turnover_richness_3QDS_m,
    species_HDS_AIC_table,
    genus_HDS_AIC_table,
    family_HDS_AIC_table,
    species_3QDS_AIC_table,
    genus_3QDS_AIC_table,
    family_3QDS_AIC_table
)
