# 4) Compiling the clean floras --------------------------------------------------

GCFR_clean_flora <- GBIF_GCFR_tidy
SWAFR_clean_flora <- GBIF_SWAFR_tidy
write.csv(GCFR_clean_flora, here::here(
  "Data",
  "derived-data",
  "flora",
  "GCFR_clean_flora_2017-09-14.csv"
))
write.csv(SWAFR_clean_flora, here::here(
  "Data",
  "derived-data",
  "flora",
  "SWAFR_clean_flora_2017-09-14.csv"
))

# Done!


# Make rasters of floral richness from floral occs -----------------------------

import_clean_flora <- function() {
  # Use `readr::read_csv()` to prevent type changes in columns,
  # and to make tibble :^)
  GCFR_clean_flora <- read_csv(here::here(
    "Data",
    "derived-data",
    "flora",
    "GCFR_clean_flora_2017-09-14.csv"
  ))
  SWAFR_clean_flora <- read_csv(here::here(
    "Data",
    "derived-data",
    "flora",
    "SWAFR_clean_flora_2017-09-14.csv"
  ))
}

import_QDS_rasters <- function() {
  GCFR_QDS_raster <- raster(here::here(
    "Data",
    "derived-data",
    "Borders",
    "GCFR_QDS_raster.tif"
  ))
  GCFR_HDS_raster <- raster(here::here(
    "Data",
    "derived-data",
    "Borders",
    "GCFR_HDS_raster.tif"
  ))
  GCFR_3QDS_raster <- raster(here::here(
    "Data",
    "derived-data",
    "Borders",
    "GCFR_3QHDS_raster.tif"
  ))
  SWAFR_QDS_raster <- raster(here::here(
    "Data",
    "derived-data",
    "Borders",
    "SWAFR_QDS_raster.tif"
  ))
  SWAFR_HDS_raster <- raster(here::here(
    "Data",
    "derived-data",
    "Borders",
    "SWAFR_HDS_raster.tif"
  ))
  SWAFR_3QDS_raster <- raster(here::here(
    "Data",
    "derived-data",
    "Borders",
    "SWAFR_3QHDS_raster.tif"
  ))
}

GCFR <- function() {
  GCFR_richness_QDS <- make_richness_raster(
    flora_occs    = GCFR_clean_flora,
    region_raster = GCFR_QDS_raster,
    crs           = std_CRS
  )
  plot(GCFR_richness_QDS$raster)
  GCFR_richness_HDS <- make_richness_raster(
    flora_occs    = GCFR_clean_flora,
    region_raster = GCFR_HDS_raster,
    crs           = std_CRS
  )
  plot(GCFR_richness_HDS$raster)
  GCFR_richness_3QDS <- make_richness_raster(
    flora_occs    = GCFR_clean_flora,
    region_raster = GCFR_3QDS_raster,
    crs           = std_CRS
  )
  plot(GCFR_richness_3QDS$raster)
  save <- function() {
    writeRaster(GCFR_richness_QDS$raster, here::here(
      "Data",
      "derived-data",
      "flora",
      "GCFR_richness_QDS_2017-09-16.tif"
    ))
    writeRaster(GCFR_richness_HDS$raster, here::here(
      "Data",
      "derived-data",
      "flora",
      "GCFR_richness_HDS_2017-09-16.tif"
    ))
    writeRaster(GCFR_richness_3QDS$raster, here::here(
      "Data",
      "derived-data",
      "flora",
      "GCFR_richness_3QDS_2017-09-16.tif"
    ))
  }
}
SWAFR <- function() {
  SWAFR_richness_QDS <- make_richness_raster(
    flora_occs    = SWAFR_clean_flora,
    region_raster = SWAFR_QDS_raster,
    crs           = std_CRS
  )
  plot(SWAFR_richness_QDS$raster)
  SWAFR_richness_HDS <- make_richness_raster(
    flora_occs    = SWAFR_clean_flora,
    region_raster = SWAFR_HDS_raster,
    crs           = std_CRS
  )
  plot(SWAFR_richness_HDS$raster)
  SWAFR_richness_3QDS <- make_richness_raster(
    flora_occs    = SWAFR_clean_flora,
    region_raster = SWAFR_3QDS_raster,
    crs           = std_CRS
  )
  plot(SWAFR_richness_3QDS$raster)
  save <- function() {
    writeRaster(SWAFR_richness_QDS$raster, here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_richness_QDS_2017-09-16.tif"
    ))
    writeRaster(SWAFR_richness_HDS$raster, here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_richness_HDS_2017-09-16.tif"
    ))
    writeRaster(SWAFR_richness_3QDS$raster, here::here(
      "Data",
      "derived-data",
      "flora",
      "SWAFR_richness_3QDS_2017-09-16.tif"
    ))
  }
}

# Trim floral occurrences outside of regions -----------------------------------

GCFR_clean_flora <- read_csv(here::here("data/derived-data/flora/GCFR_clean_flora_2017-09-14.csv"))
SWAFR_clean_flora <- read_csv(here::here("data/derived-data/flora/SWAFR_clean_flora_2017-09-14.csv"))

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

# Save
write_rds(
  trimmed_GCFR_clean_flora_spdf_species,
  here::here("data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_species")
)
write_rds(
  trimmed_GCFR_clean_flora_spdf_genus,
  here::here("data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_genus")
)
write_rds(
  trimmed_GCFR_clean_flora_spdf_family,
  here::here("data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_family")
)
write_rds(
  trimmed_SWAFR_clean_flora_spdf_species,
  here::here("data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_species")
)
write_rds(
  trimmed_SWAFR_clean_flora_spdf_genus,
  here::here("data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_genus")
)
write_rds(
  trimmed_SWAFR_clean_flora_spdf_family,
  here::here("data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_family")
)

# Compile communities by cell --------------------------------------------------

communities_by_cell_GCFR_QDS <- compile_communities_by_cell(
  trimmed_GCFR_clean_flora_spdf_species,
  "species"
)
communities_by_cell_GCFR_QDS_genus <- compile_communities_by_cell(
  trimmed_GCFR_clean_flora_spdf_genus,
  "genus"
)
communities_by_cell_GCFR_QDS_family <- compile_communities_by_cell(
  trimmed_GCFR_clean_flora_spdf_family,
  "family"
)
communities_by_cell_SWAFR_QDS <- compile_communities_by_cell(
  trimmed_SWAFR_clean_flora_spdf_species,
  "species"
)
communities_by_cell_SWAFR_QDS_genus <- compile_communities_by_cell(
  trimmed_SWAFR_clean_flora_spdf_genus,
  "genus"
)
communities_by_cell_SWAFR_QDS_family <- compile_communities_by_cell(
  trimmed_SWAFR_clean_flora_spdf_family,
  "family"
)

# Save
write_rds(
  communities_by_cell_GCFR_QDS_species,
  here::here("data/derived-data/flora/communities_by_cell_GCFR_QDS_species.RDS")
)
write_rds(
  communities_by_cell_GCFR_QDS_genus,
  here::here("data/derived-data/flora/communities_by_cell_GCFR_QDS_genus.RDS")
)
write_rds(
  communities_by_cell_GCFR_QDS_family,
  here::here("data/derived-data/flora/communities_by_cell_GCFR_QDS_family.RDS")
)
write_rds(
  communities_by_cell_SWAFR_QDS_species,
  here::here("data/derived-data/flora/communities_by_cell_SWAFR_QDS_species.RDS")
)
write_rds(
  communities_by_cell_SWAFR_QDS_genus,
  here::here("data/derived-data/flora/communities_by_cell_SWAFR_QDS_genus.RDS")
)
write_rds(
  communities_by_cell_SWAFR_QDS_family,
  here::here("data/derived-data/flora/communities_by_cell_SWAFR_QDS_family.RDS")
)
