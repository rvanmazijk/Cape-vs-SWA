# Load packages ----------------------------------------------------------------

# General programming
library(tidyverse)
library(here)  # for more reliable file paths
library(glue)  # better than paste()
library(magrittr)  # for %<>% & %$%

# GIS
library(raster)
library(rgdal)

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

# Preserve clean plotting environment
op <- par()

# Global ggplot2 theme settings
my_theme <-
  theme_bw() +
  theme(
    strip.background = element_blank(),
    panel.grid       = element_blank()
  )
theme_set(my_theme)

white_rect <- grid.rect(gp = gpar(col = "white")) # useful when arranging panels
