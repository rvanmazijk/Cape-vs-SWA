# Relate floral species turnover, and richness, with environmental roughness
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))
source(here::here("analyses/01_import-region-polygons.R"))
source(here::here("analyses/02_import-environmental-data.R"))
source(here::here("analyses/03_import-floral-data.R"))

set.seed(1234)

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

