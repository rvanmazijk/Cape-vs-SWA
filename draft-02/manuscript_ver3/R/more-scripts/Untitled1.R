# Load packages
library(tidyverse)
library(here)
library(glue)
library(raster)
library(rgdal)
library(rgeos)
library(stringr)
library(ggfortify)  # for PCAs

# Helper functions
source(here("draft-02/R/functions/helper-functions.R"))

# Import borders and Larsen et al. (2009) grids
import_region_polygons()

# Import processed environmental data
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
data_dir <- here("data/derived-data/May-2019")
GCFR_file_names  <- glue("{data_dir}/GCFR_{var_names}_masked.tif")
SWAFR_file_names <- glue("{data_dir}/SWAFR_{var_names}_masked.tif")
GCFR_variables  <- stack(GCFR_file_names)
SWAFR_variables <- stack(SWAFR_file_names)
names(GCFR_variables)  <- str_replace_all(var_names, " ", "_")
names(SWAFR_variables) <- str_replace_all(var_names, " ", "_")

raster2df <- function(r) {
  lon_lat <- xyFromCell(r, 1:ncell(r))
  colnames(lon_lat) <- c("lon", "lat")
  df <- as.data.frame(r)
  df <- cbind(lon_lat, df)
  df
}
GCFR_variables %>%
  raster2df() %>%
  na.exclude() %>%
  ggplot(aes(lon, lat, fill = Elevation)) +
    geom_tile()

# ...

d <- rbind(
  cbind(region = 1, na.exclude(GCFR_variables[])),
  cbind(region = 2, na.exclude(SWAFR_variables[]))
)
foo <- prcomp(d[, -1], center = TRUE, scale. = TRUE)
summary(foo)
autoplot(foo, data = d, colour = "region",
  alpha = 0.25,
  loadings = TRUE, loadings.colour = "black",
  loadings.label = TRUE, loadings.label.colour = "black",
  loadings.label.hjust = -0.25
) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)

# ...

# The goal, as I have learnt is the easiest approach, is to construct a
# SpatialPixelsDataFrame to collate environmental and species data.

# First, get the centroids of each QDS polygon
GCFR_QDS_centroids <- map_df(GCFR_QDS@polygons,
  ~ data.frame(
    x = .x@labpt[[1]],
    y = .x@labpt[[2]]
  )
)
SWAFR_QDS_centroids <- map_df(SWAFR_QDS@polygons,
  ~ data.frame(
    x = .x@labpt[[1]],
    y = .x@labpt[[2]]
  )
)

# Put those in a Spatial**Points**DataFrame
GCFR_QDS_centroids_pts <- SpatialPointsDataFrame(
  coords = GCFR_QDS_centroids,
  data   = GCFR_QDS@data["qdgc"]
)
SWAFR_QDS_centroids_pts <- SpatialPointsDataFrame(
  coords = SWAFR_QDS_centroids,
  data   = SWAFR_QDS@data["qdgc"]
)

# Plot to inspect
plot(GCFR_QDS_centroids_pts, col = "red")
plot(GCFR_QDS, add = TRUE)
plot(SWAFR_QDS_centroids_pts, col = "red")
plot(SWAFR_QDS, add = TRUE)
# Good, they line up exactly as they should: the centroids (red) fall
# in between the the "grid" of polygons (black)

# Use those points to make a SpatialPixelsDataFrame
GCFR_QDS_pixels <- SpatialPixelsDataFrame(
  points      = GCFR_QDS_centroids_pts,
  proj4string = crs(GCFR_QDS),
  data        = GCFR_QDS@data["qdgc"]
)
SWAFR_QDS_pixels <- SpatialPixelsDataFrame(
  points      = SWAFR_QDS_centroids_pts,
  proj4string = crs(SWAFR_QDS),
  data        = SWAFR_QDS@data["qdgc"]
)

# Coerce QDS codes from factor to character
GCFR_QDS_pixels@data$qdgc  %<>% as.character()
SWAFR_QDS_pixels@data$qdgc %<>% as.character()

# Now, extract all environmental data in these pixels
GCFR_QDS_pixels@data %<>% cbind(
  extract(stack(GCFR_variables_masked2),  GCFR_QDS_pixels)
)
SWAFR_QDS_pixels@data %<>% cbind(
  extract(stack(SWAFR_variables_masked2), SWAFR_QDS_pixels)
)

# Tidy into a dataframe
GCFR_QDS_variables_cells <-
  cbind(GCFR_QDS_pixels@coords, GCFR_QDS_pixels@data) %>%
  na.exclude() %>%
  as_tibble() %>%
  rename(lon = x, lat = y)
SWAFR_QDS_variables_cells <-
  cbind(SWAFR_QDS_pixels@coords, SWAFR_QDS_pixels@data) %>%
  na.exclude() %>%
  as_tibble() %>%
  rename(lon = x, lat = y)
GCFR_QDS_variables_cells
SWAFR_QDS_variables_cells
