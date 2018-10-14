# Manual package installation
#   For use on UCT HPC (vmzrua001@hex.uct.ac.za:/home/vmzrua001)
# Cape vs SWA publication
# Ruan van Mazijk

# Install necessary packages ------------------------------------------------------
install.packages(
  dependencies = TRUE,
  lib = "~/R-3.5.1/lib/",
  #repos = "https://cloud.r-project.org",
  pkgs = c(

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

  )
)

