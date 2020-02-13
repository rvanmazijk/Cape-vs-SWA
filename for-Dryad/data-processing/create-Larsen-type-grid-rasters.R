# Heterogeneity and species richness: replicating Larsen et al. 2009 grids
# R. van Mazijk
# CC-BY-4.0 2019

# Import region extent polygons ------------------------------------------------

GCFR_box  <- readOGR(here("data/derived-data/borders/GCFR_box"))
SWAFR_box <- readOGR(here("data/derived-data/borders/SWAFR_box"))

# Import Larsen grid polygons --------------------------------------------------

ZA_EDS <- readOGR(here("data/raw-data/QDGC/qdgc_zaf"), layer = "qdgc_03_zaf")
AU_EDS <- readOGR(here("data/raw-data/QDGC/qdgc_aus"), layer = "qdgc_03_aus")
ZA_QDS <- readOGR(here("data/raw-data/QDGC/qdgc_zaf"), layer = "qdgc_02_zaf")
AU_QDS <- readOGR(here("data/raw-data/QDGC/qdgc_aus"), layer = "qdgc_02_aus")
ZA_HDS <- readOGR(here("data/raw-data/QDGC/qdgc_zaf"), layer = "qdgc_01_zaf")
AU_HDS <- readOGR(here("data/raw-data/QDGC/qdgc_aus"), layer = "qdgc_01_aus")

# Generate polygons I need -----------------------------------------------------

# Crop each Larsen grid to regions' extents
GCFR_EDS  <- crop(ZA_EDS, GCFR_box)
SWAFR_EDS <- crop(AU_EDS, SWAFR_box)
GCFR_QDS  <- crop(ZA_QDS, GCFR_box)
SWAFR_QDS <- crop(AU_QDS, SWAFR_box)
GCFR_HDS  <- crop(ZA_HDS, GCFR_box)
SWAFR_HDS <- crop(AU_HDS, SWAFR_box)

# Merge regions' Larsen grids
Larsen_grid_EDS <- rbind(GCFR_EDS, SWAFR_EDS)
Larsen_grid_QDS <- rbind(GCFR_QDS, SWAFR_QDS)
Larsen_grid_HDS <- rbind(GCFR_HDS, SWAFR_HDS)

# Tidy up Larsen grid data and note QDS, HDS and DS
Larsen_grid_EDS$edgc <- as.character(Larsen_grid_EDS$qdgc)
Larsen_grid_EDS$qdgc <- str_remove(Larsen_grid_EDS$edgc, ".$")
Larsen_grid_EDS$hdgc <- str_remove(Larsen_grid_EDS$qdgc, ".$")
Larsen_grid_EDS$dgc  <- str_remove(Larsen_grid_EDS$hdgc, ".$")

Larsen_grid_QDS$qdgc <- as.character(Larsen_grid_QDS$qdgc)
Larsen_grid_QDS$hdgc <- str_remove(Larsen_grid_QDS$qdgc, ".$")
Larsen_grid_QDS$dgc  <- str_remove(Larsen_grid_QDS$hdgc, ".$")

Larsen_grid_HDS$hdgc <- as.character(Larsen_grid_HDS$qdgc)
Larsen_grid_HDS$dgc  <- str_remove(Larsen_grid_HDS$hdgc, ".$")

# Make numeric data properly numeric instead of factors
Larsen_grid_EDS@data[, c("lon", "lat", "areakm2")] %<>%
  map(as.character) %>%
  map(as.numeric)
Larsen_grid_QDS@data[, c("lon", "lat", "areakm2")] %<>%
  map(as.character) %>%
  map(as.numeric)
Larsen_grid_HDS@data[, c("lon", "lat", "areakm2")] %<>%
  map(as.character) %>%
  map(as.numeric)
# NOTE: lon, lat data for grid polygons are the midpoints

# Save to disc
writeOGR(
  Larsen_grid_EDS,
  here("data/derived-data/May-2019/Larsen_grid_EDS"),
  layer = "Larsen_grid_EDS",
  driver = "ESRI Shapefile"
)
writeOGR(
  Larsen_grid_QDS,
  here("data/derived-data/May-2019/Larsen_grid_QDS"),
  layer = "Larsen_grid_QDS",
  driver = "ESRI Shapefile"
)
writeOGR(
  Larsen_grid_HDS,
  here("data/derived-data/May-2019/Larsen_grid_HDS"),
  layer = "Larsen_grid_HDS",
  driver = "ESRI Shapefile"
)

# Create my own blank template rasters of/from the Larsen grids ----------------

Larsen_grid_EDS_ras <- grid2raster(Larsen_grid_EDS, 0.125)
Larsen_grid_QDS_ras <- grid2raster(Larsen_grid_QDS, 0.25)
Larsen_grid_HDS_ras <- grid2raster(Larsen_grid_HDS, 0.5)

# Save these blank template rasters --------------------------------------------

Larsen_grid_EDS_ras[] <- 0
Larsen_grid_QDS_ras[] <- 0
Larsen_grid_HDS_ras[] <- 0

writeRaster(
  Larsen_grid_EDS_ras,
  here("data/derived-data/May-2019/Larsen_grid_EDS_ras.tif"),
  overwrite = TRUE
)
writeRaster(
  Larsen_grid_QDS_ras,
  here("data/derived-data/May-2019/Larsen_grid_QDS_ras.tif"),
  overwrite = TRUE
)
writeRaster(
  Larsen_grid_HDS_ras,
  here("data/derived-data/May-2019/Larsen_grid_HDS_ras.tif"),
  overwrite = TRUE
)
