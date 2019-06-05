# Combine species occurence and environmental data
#   And calculating derived variables
# Cape vs SWA
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

# Load packages and functions
library(here)
source(here("analyses-May-2019/setup.R"))

# Import processed environmental data
var_names <- c(
  # Environmental variable names in nice order
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
data_dir <- here("data/derived-data/May-2019")
GCFR_variables_masked2 <- map(var_names,
  ~raster(glue("{data_dir}/GCFR_{.x}_masked2.tif"))
)
names(GCFR_variables_masked2) <- var_names
SWAFR_variables_masked2 <- map(var_names,
  ~raster(glue("{data_dir}/SWAFR_{.x}_masked2.tif"))
)
names(SWAFR_variables_masked2) <- var_names

# Calculate roughness layers across scales -------------------------------------

# Define roughness **here** as the mean absolute difference of a focal cell's
# value (in a 3 x 3 neighbourhood) to it's ca. 8 neighbours
body(roughness)

# Test
foo <- roughness(GCFR_variables_masked2$pH)
gplot(foo) +
  geom_tile(aes(fill = value))
foo <- roughness(GCFR_variables_masked2$Elevation)
gplot(foo) +
  geom_tile(aes(fill = value))
# Good!

# Run on all data, aggregating to a higher scale **before** calculating
# roughness (b/c: I know why!)
# (Eighth Degree Square = EDS, Quarter Degree Square       = QDS,
#  Half Degree Square   = HDS, Three-Quarter Degree Square = 3QDS)
scales <- list(base = 0.05, EDS = 0.125, QDS = 0.25, HDS = 0.50, `3QDS` = 0.75)
GCFR_roughness <- map(scales, function(scale) {
  map(GCFR_variables_masked2, function(layer) {
    layer %>%
      aggregate(fact = scale / 0.05) %>%
      roughness()
  })
})
SWAFR_roughness <- map(scales, function(scale) {
  map(SWAFR_variables_masked2, function(layer) {
    layer %>%
      aggregate(fact = scale / 0.05) %>%
      roughness()
  })
})

# Plot to check
par(mfrow = c(2, 3))
iwalk(GCFR_roughness, ~plot(.x$pH, main = glue("GCFR {.y} pH")))
par(op)
par(mfrow = c(2, 3))
iwalk(SWAFR_roughness, ~plot(.x$pH, main = glue("SWAFR {.y} pH")))
par(op)

# Tidy up
GCFR_roughness_matrices <- map(GCFR_roughness,
  ~ .x %>%
    stack() %>%
    as.data.frame() %>%
    na.exclude() %>%
    {cbind(region = "GCFR", .)}
)
SWAFR_roughness_matrices <- map(SWAFR_roughness,
  ~ .x %>%
    stack() %>%
    as.data.frame() %>%
    na.exclude() %>%
    {cbind(region = "SWAFR", .)}
)
roughness_matrices <- map2(
  GCFR_roughness_matrices, SWAFR_roughness_matrices,
  rbind
)

# Species turnover -------------------------------------------------------------

GCFR_species <- read_rds(here(
  "data/derived-data/flora/trimmed_GCFR_clean_flora_spdf_species"
))
SWAFR_species <- read_rds(here(
  "data/derived-data/flora/trimmed_SWAFR_clean_flora_spdf_species"
))

# Explore
GCFR_species
SWAFR_species

qdgc2hdgc <- function(x) {
  # QDS -> HDS by dropping the last letter
  substr(x, 1, nchar(x) - 1)
}
get_geocodes <- function(flora_points, QDS_polygon) {
  flora_points@data$qdgc <- over(flora_points, QDS_polygon)[[1]]
  flora_points@data$hdgc <- map_chr(flora_points@data$qdgc, ~
    .x %>%
      as.character() %>%
      qdgc2hdgc()
  )
  flora_points
}
# Put QDS geocodes in species SpatialPointsDataFrame
GCFR_species  %<>% get_geocodes(GCFR_QDS[,  "qdgc"])
SWAFR_species %<>% get_geocodes(SWAFR_QDS[, "qdgc"])

# New
GCFR_QDS$Elevation <- extract(GCFR_roughness_QDS$Elevation, GCFR_QDS)


