# Setup
# Cape vs SWA publication
# Ruan van Mazijk

# Load and/or download necessary packages --------------------------------------

if (!require(pacman)) {
    install.packages("pacman", dependencies = TRUE)
}
library(pacman)
p_load(

    # .... Analyses ------------------------------------------------------------

    # Data wrangling
    tidyverse, magrittr, reshape2, readr, readxl, rlang,
    here, glue, stringr, lubridate,
    # Stats
    quantreg, broom, lmodel2, canprot,
    # Visualisation
    ggplot2, grid, gridExtra, visreg, ggfortify, cowplot, scales,

    # .... Data processing -----------------------------------------------------

    # Parallel processing, nicer loops
    parallel, foreach,
    # GIS
    raster, rasterVis, sp, rgdal, spatstat, simecol,
    # Taxonomy
    taxize

)

# Create bibliography of all loaded packages
p_load(bibtex)
my_pkgs <- loadedNamespaces()
bibtex::write.bib(
    entry = my_pkgs,
    file = here::here("manuscript/pkgs.bib")
)
# Tidy up
rm(my_pkgs)

# Record session information ---------------------------------------------------

sink(here::here("manuscript/sessionInfo.txt"))
sessionInfo()
sink(NULL)

# Import functions in functions/ -----------------------------------------------

my_functions <- list.files(
    here::here("functions/"),
    pattern = ".R",
    full.names = TRUE
)
map(my_functions, source)
# Tidy up
rm(my_functions)

# Global GIS variables ---------------------------------------------------------

std_CRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
