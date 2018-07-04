# Setup
# Cape vs SWA publication
# Ruan van Mazijk

# Load and/or download necessary packages --------------------------------------

if (!"pacman" %in% installed.packages()) {
  install.packages("pacman", dependencies = TRUE)
}
pacman::p_load(
  # General programming
  magrittr, here, glue, stringr, foreach, #? rlang,
  # Data manipulation
  tidyverse, reshape2, readr, #? lubridate,
  # Stats
  quantreg, broom, canprot, #? lmodel2,
  # Visualisations
  ggplot2, grid, gridExtra, cowplot, scales, #? ggfortify,
  # Parallel processing
  parallel,
  # GIS
  raster, sp, rgdal, #? spatstat, simecol,
  # Taxonomy
  taxize
)

# Record session & pkg information ---------------------------------------------

# Note, although packrat records projects' pkg dependencies,
# here I record what is used per-session, just in case

# Record R session details and loaded packages
capture.output(
  sessionInfo(),
  file = here::here("outputs/sessionInfo.txt")
)

# Create bibliography of all loaded packages
knitr::write_bib(
  loadedNamespaces(),
  file = here::here("outputs/pkgs.bib"),
  tweak = FALSE
)

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

# Global ggplot2 theme settings ------------------------------------------------

# Colourblind friendly palette, from
# <http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette>
my_palette <- c(
  "#E69F00",  # Cape (GCFR) orange
  "#56B4E9"   # SWA (SWAFR) blue
)

# Cleaner theme
theme_set(theme_bw() + theme(strip.background = element_blank(),
                             panel.grid = element_blank()))
