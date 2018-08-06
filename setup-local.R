# Project setup
# Cape vs SWA publication
# Ruan van Mazijk

# Load and/or download necessary packages --------------------------------------

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

# Record session information ---------------------------------------------------

capture.output(
  sessionInfo(),
  file =
    if (is_macos()) {
      here::here("outputs/sessionInfo_macos.txt")
    } else if (is_windows()) {
      here::here("outputs/sessionInfo_windows.txt")
    }
)

# Custom settings and functions for this project -------------------------------

# Import all functions in R-scripts in functions/
my_functions <- list.files(
  here::here("functions/"),
  pattern = ".R",
  full.names = TRUE
)
map(my_functions, source)

# Global GIS variables
std_CRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Global ggplot2 theme settings
# Colourblind friendly palette, from
# <http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette>
my_palette <- c(
  "#E69F00",  # Cape (GCFR) orange
  "#56B4E9"   # SWA (SWAFR) blue
)
# Cleaner theme
my_theme <-
  theme_bw() +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank()
  )
theme_set(my_theme)

# Define analysis & output paths -----------------------------------------------

pre_analysis_import_paths <- list.files(
  here::here("analyses"),
  pattern = "^\\d{2}_import-.*\\.R",
  full.names = TRUE
)

analysis_paths <- list.files(
  here::here("analyses"),
  pattern = "^\\d{2}_analyse-.*\\.R",
  full.names = TRUE
)

no_ext <- "^[^.]+$"
output_paths <- list.files(
  here::here("outputs"),
  pattern = no_ext,
  full.names = TRUE
)
rm(no_ext)
