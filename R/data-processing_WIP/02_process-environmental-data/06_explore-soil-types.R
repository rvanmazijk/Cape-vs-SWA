# Exploring discrete soil classification data
#   (Cape: ARC Land Type Survey,
#    SWA: TODO: get SWA soil type layer)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))
import_region_polygons()

soil_types_Cape <- readOGR(here("data/raw-data/ARC-Land-Type-Survey/"))
soil_types_SWA <- "TODO"

# Explore the data, consulting the docs too ------------------------------------

summary(soil_types_Cape)
plot(soil_types_Cape[, "BROAD"])

soil_types_Cape@data %>%
  select(BROAD, LANDTYPE) %>%
  group_by(BROAD) %>%
  summarise(n = n()) %>%
  as.data.frame()

soil_types_Cape$LANDTYPE
