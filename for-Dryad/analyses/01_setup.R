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
