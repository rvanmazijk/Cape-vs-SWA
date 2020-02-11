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

# Detemine which DS, HDS & QDS have all 4 of their HDS, QDS & EDS --------------
# (within the regions' borders)

# Query the border polygon and store results in Larsen grid
Larsen_grid_EDS@data <- cbind(
  Larsen_grid_EDS@data,
  Larsen_grid_EDS %over% borders_buffered
)
# For larger cells, just use longitude for region classification,
# because later I filter the cells based on whether their constituent cells
# are in the regions (from EDS above)
Larsen_grid_QDS@data$region <-
  ifelse(Larsen_grid_QDS@data$lon > 90,
    "SWAFR", "GCFR"
  )
Larsen_grid_HDS@data$region <-
  ifelse(Larsen_grid_HDS@data$lon > 90,
    "SWAFR", "GCFR"
  )

# Filter to EDS that are within the regions' borders
Larsen_grid_EDS <- Larsen_grid_EDS[!is.na(Larsen_grid_EDS$region), ]

# Make grids tibbles for easier wrangling
Larsen_grid_EDS_data <- as_tibble(Larsen_grid_EDS@data)
Larsen_grid_QDS_data <- as_tibble(Larsen_grid_QDS@data)
Larsen_grid_HDS_data <- as_tibble(Larsen_grid_HDS@data)

# Pull out QDS-codes of QDS with all 4 EDS (within borders)
QDS_w_all_EDS <- Larsen_grid_EDS_data %>%
  group_by(qdgc, region) %>%
  dplyr::select(edgc) %>%
  distinct() %>%  # just in case
  summarise(n_EDS = n()) %>%
  filter(n_EDS == 4) %>%
  pull(qdgc)

# Pull out HDS-codes of HDS with all 4 HDS (within borders)
HDS_w_all_QDS <- Larsen_grid_QDS_data %>%
  group_by(hdgc, region) %>%
  dplyr::select(qdgc) %>%
  distinct() %>%
  filter(qdgc %in% QDS_w_all_EDS) %>%
  summarise(n_QDS = n()) %>%
  filter(n_QDS == 4) %>%
  pull(hdgc)

# Pull out DS-codes of DS with all 4 DS (within borders)
DS_w_all_HDS <- Larsen_grid_HDS_data %>%
  group_by(dgc, region) %>%
  dplyr::select(hdgc) %>%
  distinct() %>%
  filter(hdgc %in% HDS_w_all_QDS) %>%
  summarise(n_HDS = n()) %>%
  filter(n_HDS == 4) %>%
  pull(dgc)

# Plot grids and cell midpoints to check
# NOTE:
#   Plots EDS that belong to a QDS with all 4 EDS in region,
#   **not** the QDS themselves,
#   etc. for other scales
if (FALSE) {
  # GCFR:
  plot(border = "green", Larsen_grid_EDS[
    Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS &
    Larsen_grid_EDS$region == "GCFR",
  ])
  points(col = "green",
    Larsen_grid_EDS$lon[Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS],
    Larsen_grid_EDS$lat[Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS]
  )
  plot(border = "red", add = TRUE, Larsen_grid_QDS[
    Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS &
    Larsen_grid_QDS$region == "GCFR",
  ])
  points(col = "red",
    Larsen_grid_QDS$lon[Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS],
    Larsen_grid_QDS$lat[Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS]
  )
  plot(border = "blue", add = TRUE, Larsen_grid_HDS[
    Larsen_grid_HDS$dgc %in% DS_w_all_HDS &
    Larsen_grid_HDS$region == "GCFR",
  ])
  points(col = "blue",
    Larsen_grid_HDS$lon[Larsen_grid_HDS$dgc %in% DS_w_all_HDS],
    Larsen_grid_HDS$lat[Larsen_grid_HDS$dgc %in% DS_w_all_HDS]
  )

  # SWAFR:
  plot(border = "green", Larsen_grid_EDS[
    Larsen_grid_EDS$qdgc %in% QDS_w_all_EDS &
    Larsen_grid_EDS$region == "SWAFR",
  ])
  plot(border = "red", add = TRUE, Larsen_grid_QDS[
    Larsen_grid_QDS$hdgc %in% HDS_w_all_QDS &
    Larsen_grid_QDS$region == "SWAFR",
  ])
  plot(border = "blue", add = TRUE, Larsen_grid_HDS[
    Larsen_grid_HDS$dgc %in% DS_w_all_HDS &
    Larsen_grid_HDS$region == "SWAFR",
  ])
}

# Create my own blank rasters of/from the Larsen grids -------------------------

# (Remake un-filtered EDS grid)
Larsen_grid_EDS_raw <- rbind(GCFR_EDS, SWAFR_EDS)

Larsen_grid_EDS_ras <- grid2raster(Larsen_grid_EDS_raw, 0.125)
Larsen_grid_QDS_ras <- grid2raster(Larsen_grid_QDS,     0.25)
Larsen_grid_HDS_ras <- grid2raster(Larsen_grid_HDS,     0.5)

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

Larsen_grid_EDS_ras2 <- Larsen_grid_EDS_ras
cells_to_fill <- cellFromXY(
  Larsen_grid_EDS_ras2,
  as.data.frame(Larsen_grid_EDS_data[, c("lon", "lat")])
)
Larsen_grid_EDS_ras2[cells_to_fill] <- Larsen_grid_EDS_data$areakm2
plot(Larsen_grid_EDS_ras2)

# Save these blank template rasters --------------------------------------------

Larsen_grid_EDS_ras[] <- 0
writeRaster(
  Larsen_grid_EDS_ras,
  here("data/derived-data/May-2019/Larsen_grid_EDS_ras.tif"),
  overwrite = TRUE
)

Larsen_grid_QDS_ras[] <- 0
writeRaster(
  Larsen_grid_QDS_ras,
  here("data/derived-data/May-2019/Larsen_grid_QDS_ras.tif"),
  overwrite = TRUE
)

Larsen_grid_HDS_ras[] <- 0
writeRaster(
  Larsen_grid_HDS_ras,
  here("data/derived-data/May-2019/Larsen_grid_HDS_ras.tif"),
  overwrite = TRUE
)
