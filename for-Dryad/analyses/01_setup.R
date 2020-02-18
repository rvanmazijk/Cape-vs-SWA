# Heterogeneity and species richness: analysis setup
# R. van Mazijk
# CC-BY-4.0 2019

# Load packages ----------------------------------------------------------------

# General programming
library(tidyverse)
library(here)  # for more reliable file paths
library(glue)  # better than paste()
library(magrittr)  # for %<>% & %$%

# GIS
library(raster)
library(rgdal)
library(rgeos)

# Analyses
library(canprot)  # for CLES
library(broom)  # to tidy model outputs

# Figures
library(cowplot)  # for panelling
library(ggfortify)  # for autoplot() of PCAs
library(rasterVis)
library(scales)
library(grid)

# Set global variables ---------------------------------------------------------

data_dir <- here("data/derived-data/May-2019")

# Environmental variable names in nice order
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

# Figure things ----------------------------------------------------------------

# Preserve clean plotting environment (for base:: figures)
op <- par()

# Set global theme (for ggplot2:: figures)
my_theme <-
  theme_bw() +
  theme(
    strip.background = element_blank(),
    panel.grid       = element_blank()
  )
theme_set(my_theme)

# Make a blank plot object (useful as filler when arranging panels)
white_rect <- grid.rect(gp = gpar(col = "white"))

# Helper-functions -------------------------------------------------------------

grid_dim <- function(x) {
  # Gets the latitudinal and longitudinal range of a SpatialPolygonsDataFrame
  list(
    width  = extent(x)[2] - extent(x)[1],
    height = extent(x)[4] - extent(x)[3],
    crs    = proj4string(x)
  )
}

grid2raster <- function(x, resol = c(0.125, 0.25, 0.5, 1)) {
  # Creates a raster with the same dimensions and number of cells
  # as a Larsen-type grid SpatialPolygonsDataFrame
  n_gc_wide <- grid_dim(x)$width  / resol
  n_gc_high <- grid_dim(x)$height / resol
  raster(
    nrow = n_gc_high, ncol = n_gc_wide,
    crs = proj4string(x),
    xmn = extent(x)[1], xmx = extent(x)[2],
    ymn = extent(x)[3], ymx = extent(x)[4]
  )
}

raster2df <- function(r, Larsen_grid_data) {
  # Creates a dataframe of raster layer/stack/brick data
  # with columns for the lon and lat of the midpoint of each cell
  df <- cbind(
    xyFromCell(r, 1:ncell(r)),
    as.data.frame(r)
  )
  names(df)[1:2] <- c("lon", "lat")
  full_join(Larsen_grid_data, df)
}

force_positive_PC1 <- function(PCA) {
  # Ammends all variables' loadings in a PCA to be positive
  # if they are all negative, for simplicity
  if (all(PCA$rotation[, 1] <= 0)) {
    message("Multiplying this one by -1")
    PCA$rotation[, 1] %<>% multiply_by(-1)
    PCA$x[, 1]        %<>% multiply_by(-1)
  }
  PCA
}

make_SpatialPointsDataFrame <- function(df) {
  # Make a SpatialPointsDataFrame out of the (cleaned) GBIF occurrence data
  # for GCFR and SWAFR vascular plants
  SpatialPointsDataFrame(
    coords      = df[, c("decimallongitude", "decimallatitude")],
    data        = df[, "species"],
    proj4string = crs(borders_buffered)  # depends on this object existing!
  )
}

rasterise_data <- function(df, df_col, r) {
  # TODO: describe this function
  r[cellFromXY(r, as.data.frame(df[, c("lon", "lat")]))] <- df[[df_col]]
  r[r == 0] <- NA
  r
}
