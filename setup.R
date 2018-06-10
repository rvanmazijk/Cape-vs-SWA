# Setup
# GCFR-SWAFR publication
# Ruan van Mazijk

# Pkg setup --------------------------------------------------------------------

if (!require(pacman)) install.packages("pacman", dependencies = TRUE)
library(pacman)
p_load(
    # Data wrangling
    tidyverse, magrittr, reshape2, readr, readxl, rlang,
    here, glue, stringr, lubridate,
    # Stats
    quantreg, broom, lmodel2, canprot,
    # Visualisation
    ggplot2, grid, gridExtra, visreg, ggfortify, cowplot, scales,
    # Parallel processing, nicer loops
    parallel, foreach,
    # GIS
    raster, rasterVis, sp, rgdal, spatstat, simecol,
    # Taxonomy
    taxize,
    # BRTs
    dismo, gbm
)

# GIS setup --------------------------------------------------------------------

#giswd <- "/Users/ruanvanmazijk/Downloads"  # Needed for soils data (macOS)
giswd <- "C:\\Users\\user\\Documents\\" # Needed for soils data (Windows)

std_CRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
