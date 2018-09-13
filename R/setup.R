# Project setup
# Cape vs SWA publication
# Ruan van Mazijk

# Load necessary packages ------------------------------------------------------

# General programming
library(magrittr)
library(here)
library(glue)
library(stringr)
library(foreach)
library(xfun)

# Parallel processing
library(parallel)
library(microbenchmark)

# GIS
library(rgdal)
library(raster)
library(sp)
library(maptools)

# Taxonomy and species name cleaning
#library(taxize)

# Pairwise Jaccard distance calculations
library(vegan)

# Tabular data-manipulation
library(tidyverse)
library(reshape2)
library(readr)

# Statistics and modelling
library(quantreg)
library(broom)
library(canprot)
library(spgwr)

# Visualisations
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
library(scales)
#library(rasterVis)
#library(ggspatial)

# Record session information ---------------------------------------------------

capture.output(
  sessionInfo(),
  file =
    if (is_macos()) {
      here("outputs/sessionInfo_macos.txt")
    } else if (is_windows()) {
      here("outputs/sessionInfo_windows.txt")
    }
)

# Custom settings and functions for this project -------------------------------

# Import all functions in R-scripts in functions/
my_functions <- list.files(
  here("R/functions"),
  pattern = ".R",
  recursive = TRUE,
  full.names = TRUE
)
map(my_functions, source)
rm(my_functions)

# Global GIS variables
std_CRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Global ggplot2 theme settings
my_palette <- c(
  "#E69F00",  # Cape (GCFR) orange
  "#307aa5"   # SWA (SWAFR) blue
)
my_theme <-
  theme_bw() +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank()
  )
theme_set(my_theme)

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
