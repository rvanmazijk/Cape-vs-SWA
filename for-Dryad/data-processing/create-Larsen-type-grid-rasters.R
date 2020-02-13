# Heterogeneity and species richness: replicating Larsen et al. 2009 grids
# R. van Mazijk
# CC-BY-4.0 2019

# Import region polygons -------------------------------------------------------

GCFR_border_buffered <-
  readOGR(here("data/derived-data/borders/GCFR_border_buffered/"))
SWAFR_border_buffered <-
  readOGR(here("data/derived-data/borders/SWAFR_border_buffered/"))

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

# Merge regions' borders
borders_buffered <- rbind(GCFR_border_buffered, SWAFR_border_buffered)

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

# Create my own blank rasters of/from the Larsen grids -------------------------

Larsen_grid_EDS_ras <- grid2raster(Larsen_grid_EDS, 0.125)
Larsen_grid_QDS_ras <- grid2raster(Larsen_grid_QDS, 0.25)
Larsen_grid_HDS_ras <- grid2raster(Larsen_grid_HDS, 0.5)

# Plots to check
if (FALSE) {
  # Create dummy data in rasters for plottign
  Larsen_grid_EDS_ras2 <- Larsen_grid_EDS_ras
  Larsen_grid_QDS_ras2 <- Larsen_grid_QDS_ras
  Larsen_grid_HDS_ras2 <- Larsen_grid_HDS_ras
  Larsen_grid_EDS_ras2[] <- 1:ncell(Larsen_grid_EDS_ras2)
  Larsen_grid_QDS_ras2[] <- 1:ncell(Larsen_grid_QDS_ras2)
  Larsen_grid_HDS_ras2[] <- 1:ncell(Larsen_grid_HDS_ras2)

  # Plot rasters and region polygons to check extents
  plot(Larsen_grid_EDS_ras2)
  plot(borders_buffered, add = TRUE)

  plot(Larsen_grid_QDS_ras2)
  plot(borders_buffered, add = TRUE)

  plot(Larsen_grid_HDS_ras2)
  plot(borders_buffered, add = TRUE)

  # Plot rasters and cells midpoints check
  Larsen_grid_EDS_ras2 %>%
    crop(GCFR_border_buffered) %>%
    {
      plot(.)
      points(xyFromCell(., 1:ncell(.)))
    }
  Larsen_grid_QDS_ras2 %>%
    crop(GCFR_border_buffered) %>%
    {
      plot(.)
      points(xyFromCell(., 1:ncell(.)))
    }
  Larsen_grid_HDS_ras2 %>%
    crop(GCFR_border_buffered) %>%
    {
      plot(.)
      points(xyFromCell(., 1:ncell(.)))
    }
}

# Test putting arbitrary data into raster via lon-lat lookup from tibble -------

if (FALSE) {
  Larsen_grid_EDS_ras2 <- Larsen_grid_EDS_ras
  cells_to_fill <- cellFromXY(
    Larsen_grid_EDS_ras2,
    as.data.frame(Larsen_grid_EDS_data[, c("lon", "lat")])
  )
  Larsen_grid_EDS_ras2[cells_to_fill] <- Larsen_grid_EDS_data$areakm2
  plot(Larsen_grid_EDS_ras2)
}

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
