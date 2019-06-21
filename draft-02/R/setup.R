# Project setup
# Cape vs SWA
# Ruan van Mazijk

# Misc. ------------------------------------------------------------------------

# Preserve starting plotting environment settings,
# to reset after par(mfrow = ...) etc.
op <- par()

# Load packages ----------------------------------------------------------------

# General programming
library(tidyverse)
library(here)
library(glue)
library(magrittr)

# GIS
library(raster)
library(rgdal)
library(rgeos)

# Analyses
library(MASS)  # for LDA
# FIXME: fix MASS::select() conflicts w/ raster::select() and dplyr::select()
library(canprot)  # for CLES
library(broom)  # to tidy model outputs
library(vegan)

# Figures
library(rasterVis)  # for maps
library(ggfortify)  # for PCAs
# Panelling figures
library(grid)
library(cowplot)

# Source helper functions ------------------------------------------------------

function_filenames <- list.files(
  here("draft-02/R/functions"),
  full.names = TRUE
)
walk(function_filenames, source)

# Global ggplot2-theme settings ------------------------------------------------

my_theme <-
  theme_bw() +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank()
  )
theme_set(my_theme)

# Global graphics objects ------------------------------------------------------

my_palette <- c(
  "#E69F00",  # Cape (GCFR) orange
  "#307aa5"   # SWA (SWAFR) blue
)
var_colours <- c(
  # <https://colourco.de/>
  "grey50",   # grey   for elevation
  "#507CC5",  # blue   for climate
  "#37A541",  # greeen for NDVI
  "#BA793E",  # brown  for soils
  "red"       #        for PC1
)
var_shapes <- c(
  17,  # triangle      for elevation
  16,  # filled circle for MAP
  1,   # open circle   for PDQ
  15,  # square        for surfact T
  4,   # x             for NDVI,
  17,  # triangle      for CEC
  16,  # filled circle for clay
  1,   # open circle   for soil C
  15   # square        for pH
)

var_names <- c(
  # Environmental variable names in nice order
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

white_rect <- grid.rect(gp = gpar(col = "white"))
# Useful when arranging panels

# Record session info ----------------------------------------------------------

capture.output(
  sessionInfo(),
  file = here("draft-02/outputs/sessionInfo.txt")
)
