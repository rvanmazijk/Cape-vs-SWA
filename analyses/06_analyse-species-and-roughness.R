# Relate floral species turnover, and richness, with environmental roughness
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
source(here::here("analyses/02_import-region-polygons.R"))
source(here::here("analyses/04_import-environmental-data.R"))
source(here::here("analyses/06_import-floral-data.R"))

set.seed(1234)

# Enviro data setup ------------------------------------------------------------

var_names <- c(
    "Elevation",
    "MAP",
    "PDQ",
    "Surface T",
    "NDVI",
    "CEC",
    "Clay",
    "Soil C",
    "pH"
)
GCFR_variables <- list(
    GCFR_elev,
    GCFR_MAP,
    GCFR_PDQ,
    GCFR_MLST,
    GCFR_NDVI,
    GCFR_soils$GCFR_CECSOL_M_250m_std_CRS_0.05,
    GCFR_soils$GCFR_CLYPPT_M_250m_std_CRS_0.05,
    GCFR_soils$GCFR_OCDENS_M_250m_std_CRS_0.05,
    GCFR_soils$GCFR_PHIKCL_M_250m_std_CRS_0.05
)
SWAFR_variables <- list(
    SWAFR_elev,
    SWAFR_MAP,
    SWAFR_PDQ,
    SWAFR_MLST,
    SWAFR_NDVI,
    SWAFR_soils$SWAFR_CECSOL_M_250m_std_CRS_0.05,
    SWAFR_soils$SWAFR_CLYPPT_M_250m_std_CRS_0.05,
    SWAFR_soils$SWAFR_OCDENS_M_250m_std_CRS_0.05,
    SWAFR_soils$SWAFR_PHIKCL_M_250m_std_CRS_0.05
)
GCFR_variables %<>%
    map(crop, GCFR_variables[[4]]) %>%
    map(mask, GCFR_border)
SWAFR_variables %<>%
    map(crop, SWAFR_variables[[4]]) %>%
    map(mask, SWAFR_border)
names(GCFR_variables) <- var_names
names(SWAFR_variables) <- var_names

GCFR_variables_QDS <- GCFR_variables %>%
    map(resample, GCFR_richness_QDS, method = "bilinear") %>%
    map(mask, GCFR_border)
GCFR_variables_HDS <- GCFR_variables %>%
    map(resample, GCFR_richness_HDS, method = "bilinear") %>%
    map(mask, GCFR_border)
GCFR_variables_3QDS <- GCFR_variables %>%
    map(resample, GCFR_richness_3QDS, method = "bilinear") %>%
    map(mask, GCFR_border)

SWAFR_variables_QDS <- SWAFR_variables %>%
    map(resample, SWAFR_richness_QDS, method = "bilinear") %>%
    map(mask, SWAFR_border)
SWAFR_variables_HDS <- SWAFR_variables %>%
    map(resample, SWAFR_richness_HDS, method = "bilinear") %>%
    map(mask, SWAFR_border)
SWAFR_variables_3QDS <- SWAFR_variables %>%
    map(resample, SWAFR_richness_3QDS, method = "bilinear") %>%
    map(mask, SWAFR_border)

# Gridded data setup -----------------------------------------------------------
# Get pixel IDs for QDS & HDS & DS

GCFR_3QDS_cell_nos <- unlist(cellFromPolygon(GCFR_richness_3QDS, GCFR_border))
GCFR_HDS_cell_nos <- unlist(cellFromPolygon(GCFR_richness_HDS, GCFR_border))
GCFR_QDS_cell_nos <- unlist(cellFromPolygon(GCFR_richness_QDS, GCFR_border))
GCFR_QDS_cell_xys <- xyFromCell(GCFR_richness_QDS, GCFR_QDS_cell_nos)
GCFR_3QDS_cell_nos_from_QDS_xys <- cellFromXY(GCFR_richness_3QDS, GCFR_QDS_cell_xys)
GCFR_HDS_cell_nos_from_QDS_xys <- cellFromXY(GCFR_richness_HDS, GCFR_QDS_cell_xys)

SWAFR_3QDS_cell_nos <- unlist(cellFromPolygon(SWAFR_richness_3QDS, SWAFR_border))
SWAFR_HDS_cell_nos <- unlist(cellFromPolygon(SWAFR_richness_HDS, SWAFR_border))
SWAFR_QDS_cell_nos <- unlist(cellFromPolygon(SWAFR_richness_QDS, SWAFR_border))
SWAFR_QDS_cell_xys <- xyFromCell(SWAFR_richness_QDS, SWAFR_QDS_cell_nos)
SWAFR_3QDS_cell_nos_from_QDS_xys <- cellFromXY(SWAFR_richness_3QDS, SWAFR_QDS_cell_xys)
SWAFR_HDS_cell_nos_from_QDS_xys <- cellFromXY(SWAFR_richness_HDS, SWAFR_QDS_cell_xys)

GCFR_3QDS_HDS_QDS_cells_df <- tibble(
    region      = "GCFR",
    QDS_cell_no = GCFR_QDS_cell_nos,
    QDS_cell_x  = GCFR_QDS_cell_xys[, 1],
    QDS_cell_y  = GCFR_QDS_cell_xys[, 2],
    HDS_cell_no = GCFR_HDS_cell_nos_from_QDS_xys,
    DS_cell_no  = GCFR_3QDS_cell_nos_from_QDS_xys
)
SWAFR_3QDS_HDS_QDS_cells_df <- tibble(
    region      = "SWAFR",
    QDS_cell_no = SWAFR_QDS_cell_nos,
    QDS_cell_x  = SWAFR_QDS_cell_xys[, 1],
    QDS_cell_y  = SWAFR_QDS_cell_xys[, 2],
    HDS_cell_no = SWAFR_HDS_cell_nos_from_QDS_xys,
    DS_cell_no  = SWAFR_3QDS_cell_nos_from_QDS_xys
)
threeQDS_HDS_QDS_cells_df <-
    rbind(GCFR_3QDS_HDS_QDS_cells_df, SWAFR_3QDS_HDS_QDS_cells_df)

# ... --------------------------------------------------------------------------

# GCFR
#foreach(HDS_cell = GCFR_3QDS_HDS_QDS_cells_df$HDS_cell_no) %do% {
foreach(HDS_cell = 210:220) %do% {

    four_QDS_cell_nos <- GCFR_3QDS_HDS_QDS_cells_df %>%
        filter(HDS_cell_no == HDS_cell) %>%
        dplyr::select(QDS_cell_no) %>%
        as.vector()

    four_QDS_values <- foreach(QDS_cell_no = four_QDS_cell_nos) %do% {
        extract(GCFR_variables_QDS$Elevation, QDS_cell_no[[1]])
    }

    diffs <- vector(length = 6)
    diffs[1] <- (four_QDS_values[1] - four_QDS_values[2]) ^ 2
    diffs[2] <- (four_QDS_values[1] - four_QDS_values[3]) ^ 2
    diffs[3] <- (four_QDS_values[1] - four_QDS_values[4]) ^ 2
    diffs[4] <- (four_QDS_values[2] - four_QDS_values[3]) ^ 2
    diffs[5] <- (four_QDS_values[2] - four_QDS_values[4]) ^ 2
    diffs[6] <- (four_QDS_values[3] - four_QDS_values[4]) ^ 2

    print(mean(diffs))

}
# FIXME

# Species richness data setup --------------------------------------------------

source(here::here("analyses/09_analyse-species-turnover-and-richness.R"))

