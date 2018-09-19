# Manual package installation
#   Useful if lack permissions to use packrat (e.g. on a guest PC)
# Cape vs SWA publication
# Ruan van Mazijk

# Install necessary packages ------------------------------------------------------
install.packages(dependencies = TRUE, c(

  # General programming
  "magrittr",
  "here",
  "glue",
  "stringr",
  "foreach",
  "xfun",

  # Parallel processing
  "parallel",
  "microbenchmark",

  # GIS
  "rgdal",
  "raster",
  "sp",
  "maptools",

  # Taxonomy and species name cleaning
  "taxize",

  # Pairwise Jaccard distance calculations
  "vegan",

  # Tabular data-manipulation
  "tidyverse",
  "reshape2",
  "readr",

  # Statistics and modelling
  "quantreg",
  "broom",
  "canprot",
  "spgwr",

  # Visualisations
  "ggplot2",
  "grid",
  "gridExtra",
  "cowplot",
  "scales",
  "rasterVis",
  "ggspatial"

))
