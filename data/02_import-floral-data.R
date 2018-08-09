# Import floral occurrence data
# Cape vs SWA publication
# Ruan van Mazijk

flora_dir <- here::here("data/derived-data/flora")

# Raw flora --------------------------------------------------------------------

GCFR_clean_flora <-
  read_csv(glue("{flora_dir}/GCFR_clean_flora_2017-09-14.csv"))
SWAFR_clean_flora <-
  read_csv(glue("{flora_dir}/SWAFR_clean_flora_2017-09-14.csv"))

# Trimmed-to-regions SpatialPointsDataFrames of species occ. data --------------

trimmed_GCFR_clean_flora_spdf_species <-
  read_rds(glue("{flora_dir}/trimmed_GCFR_clean_flora_spdf_species"))
trimmed_GCFR_clean_flora_spdf_genus <-
  read_rds(glue("{flora_dir}/trimmed_GCFR_clean_flora_spdf_genus"))
trimmed_GCFR_clean_flora_spdf_family <-
  read_rds(glue("{flora_dir}/trimmed_GCFR_clean_flora_spdf_family"))
trimmed_SWAFR_clean_flora_spdf_species <-
  read_rds(glue("{flora_dir}/trimmed_SWAFR_clean_flora_spdf_species"))
trimmed_SWAFR_clean_flora_spdf_genus <-
  read_rds(glue("{flora_dir}/trimmed_SWAFR_clean_flora_spdf_genus"))
trimmed_SWAFR_clean_flora_spdf_family <-
  read_rds(glue("{flora_dir}/trimmed_SWAFR_clean_flora_spdf_family"))

# Lists of communities in QDS cells --------------------------------------------
# At the species, genus, and family level

communities_by_cell_GCFR_QDS_species <-
  read_rds(glue("{flora_dir}/communities_by_cell_GCFR_QDS_species.RDS"))
communities_by_cell_GCFR_QDS_genus <-
  read_rds(glue("{flora_dir}/communities_by_cell_GCFR_QDS_genus.RDS"))
communities_by_cell_GCFR_QDS_family <-
  read_rds(glue("{flora_dir}/communities_by_cell_GCFR_QDS_family.RDS"))
communities_by_cell_SWAFR_QDS_species <-
  read_rds(glue("{flora_dir}/communities_by_cell_SWAFR_QDS_species.RDS"))
communities_by_cell_SWAFR_QDS_genus <-
  read_rds(glue("{flora_dir}/communities_by_cell_SWAFR_QDS_genus.RDS"))
communities_by_cell_SWAFR_QDS_family <-
  read_rds(glue("{flora_dir}/communities_by_cell_SWAFR_QDS_family.RDS"))

# Compile into 1 object (a list of lists of lists)
communities_by_cell_QDS <- list(
  GCFR = list(
    species = communities_by_cell_GCFR_QDS_species,
    genus = communities_by_cell_GCFR_QDS_genus,
    family = communities_by_cell_GCFR_QDS_family
  ),
  SWAFR = list(
    species = communities_by_cell_SWAFR_QDS_species,
    genus = communities_by_cell_SWAFR_QDS_genus,
    family = communities_by_cell_SWAFR_QDS_family
  )
)
# Tidy up
rm(
  communities_by_cell_GCFR_QDS_species,
  communities_by_cell_GCFR_QDS_genus,
  communities_by_cell_GCFR_QDS_family,
  communities_by_cell_SWAFR_QDS_species,
  communities_by_cell_SWAFR_QDS_genus,
  communities_by_cell_SWAFR_QDS_family
)

# Richness rasters -------------------------------------------------------------

GCFR_richness_QDS <-
  raster(glue("{flora_dir}/GCFR_richness_QDS_2017-09-16.tif"))
stopifnot(proj4string(GCFR_richness_QDS) == std_CRS)
GCFR_richness_HDS <-
  raster(glue("{flora_dir}/GCFR_richness_HDS_2017-09-16.tif"))
GCFR_richness_3QDS <-
  raster(glue("{flora_dir}/GCFR_richness_3QDS_2017-09-16.tif"))

SWAFR_richness_QDS <-
  raster(glue("{flora_dir}/SWAFR_richness_QDS_2017-09-16.tif"))
stopifnot(proj4string(SWAFR_richness_QDS) == std_CRS)
SWAFR_richness_HDS <-
  raster(glue("{flora_dir}/SWAFR_richness_HDS_2017-09-16.tif"))
SWAFR_richness_3QDS <-
  raster(glue("{flora_dir}/SWAFR_richness_3QDS_2017-09-16.tif"))

# Get pixel IDs for QDS & HDS & DS rasters -------------------------------------

GCFR_3QDS_cell_nos <- unlist(cellFromPolygon(GCFR_richness_3QDS, GCFR_border))
GCFR_HDS_cell_nos <- unlist(cellFromPolygon(GCFR_richness_HDS, GCFR_border))
GCFR_QDS_cell_nos <- unlist(cellFromPolygon(GCFR_richness_QDS, GCFR_border))
GCFR_QDS_cell_xys <-
  xyFromCell(GCFR_richness_QDS, GCFR_QDS_cell_nos)
GCFR_3QDS_cell_nos_from_QDS_xys <-
  cellFromXY(GCFR_richness_3QDS, GCFR_QDS_cell_xys)
GCFR_HDS_cell_nos_from_QDS_xys <-
  cellFromXY(GCFR_richness_HDS, GCFR_QDS_cell_xys)

SWAFR_3QDS_cell_nos <- unlist(cellFromPolygon(SWAFR_richness_3QDS, SWAFR_border))
SWAFR_HDS_cell_nos <- unlist(cellFromPolygon(SWAFR_richness_HDS, SWAFR_border))
SWAFR_QDS_cell_nos <- unlist(cellFromPolygon(SWAFR_richness_QDS, SWAFR_border))
SWAFR_QDS_cell_xys <-
  xyFromCell(SWAFR_richness_QDS, SWAFR_QDS_cell_nos)
SWAFR_3QDS_cell_nos_from_QDS_xys <-
  cellFromXY(SWAFR_richness_3QDS, SWAFR_QDS_cell_xys)
SWAFR_HDS_cell_nos_from_QDS_xys <-
  cellFromXY(SWAFR_richness_HDS, SWAFR_QDS_cell_xys)

GCFR_3QDS_HDS_QDS_cells_df <- tibble(
  region = "GCFR",
  QDS_cell_x = GCFR_QDS_cell_xys[, 1],
  QDS_cell_y = GCFR_QDS_cell_xys[, 2],
  QDS_cell_no = GCFR_QDS_cell_nos,
  HDS_cell_no = GCFR_HDS_cell_nos_from_QDS_xys,
  threeQDS_cell_no = GCFR_3QDS_cell_nos_from_QDS_xys
)
SWAFR_3QDS_HDS_QDS_cells_df <- tibble(
  region = "SWAFR",
  QDS_cell_x = SWAFR_QDS_cell_xys[, 1],
  QDS_cell_y = SWAFR_QDS_cell_xys[, 2],
  QDS_cell_no = SWAFR_QDS_cell_nos,
  HDS_cell_no = SWAFR_HDS_cell_nos_from_QDS_xys,
  threeQDS_cell_no = SWAFR_3QDS_cell_nos_from_QDS_xys
)

# Compile into 1 object
cells <- rbind(
  GCFR_3QDS_HDS_QDS_cells_df,
  SWAFR_3QDS_HDS_QDS_cells_df
)

# Tidy up
rm(
  GCFR_3QDS_cell_nos,
  GCFR_HDS_cell_nos,
  GCFR_QDS_cell_nos,
  GCFR_QDS_cell_xys,
  GCFR_3QDS_cell_nos_from_QDS_xys,
  GCFR_HDS_cell_nos_from_QDS_xys,

  SWAFR_3QDS_cell_nos,
  SWAFR_HDS_cell_nos,
  SWAFR_QDS_cell_nos,
  SWAFR_QDS_cell_xys,
  SWAFR_3QDS_cell_nos_from_QDS_xys,
  SWAFR_HDS_cell_nos_from_QDS_xys,

  GCFR_3QDS_HDS_QDS_cells_df,
  SWAFR_3QDS_HDS_QDS_cells_df
)
