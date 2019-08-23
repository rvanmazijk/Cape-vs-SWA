# Analyses setup
# GCFR vs SWAFR ms
# Ruan van Mazijk

# Misc. ------------------------------------------------------------------------

# Preserve inital plotting environment,
# to reset after par(mfrow = ...) etc.
op <- par()

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
library(canprot)  # for CLES
library(broom)  # to tidy model outputs
library(vegan)
library(olsrr)

# Figures
library(rasterVis)  # for maps
library(ggfortify)  # for PCAs
# Panelling figures
library(grid)
library(cowplot)

# Source helper functions' scripts ---------------------------------------------

function_filenames <- list.files(
  here("draft-02/R/functions"),
  full.names = TRUE
)
walk(function_filenames, source)

# Figure things ----------------------------------------------------------------

# Global ggplot2 theme settings
theme_set(theme_bw() + theme(
  strip.background = element_blank(),
  panel.grid = element_blank()
))

white_rect <- grid.rect(gp = gpar(col = "white"))
# Useful when arranging panels

# Record session info ----------------------------------------------------------

capture.output(
  sessionInfo(),
  file = here("draft-02/outputs/sessionInfo.txt")
)
