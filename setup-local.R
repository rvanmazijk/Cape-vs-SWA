# Project setup specific for my machine
# Cape vs SWA publication
# Ruan van Mazijk

# Load and/or download necessary packages
if (!require(pacman)) install.packages("pacman", dependencies = TRUE)
pacman::p_load(
  # General programming
  magrittr, here, glue, stringr, foreach, xfun,
  # Parallel processing
  parallel, microbenchmark,
  # GIS
  raster, sp, rgdal,
  # Taxonomy
  taxize,
  # Tabular data-manipulation
  tidyverse, reshape2, readr,
  # Statistics and modelling
  quantreg, broom, canprot, spgwr,
  # Visualisations
  ggplot2, grid, gridExtra, cowplot, scales, rasterVis, ggspatial
)

# Source the main project setup.R
source(here::here("setup.R"))
