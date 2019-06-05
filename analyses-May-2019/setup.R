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
library(rasterVis)  # fpr maps
library(ggfortify)  # for PCAs

# Source helper functions ------------------------------------------------------

function_filenames <- list.files(
  here("analyses-May-2019/functions"),
  full.names = TRUE
)
walk(function_filenames, source)
