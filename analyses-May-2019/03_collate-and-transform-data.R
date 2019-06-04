# Combine species occurence and environmental data
#   And calculating derived variables
# Cape vs SWA
# Ruan van Mazijk


# Setup ------------------------------------------------------------------------

# .... Misc. -------------------------------------------------------------------

# Preserve starting plotting environment settings,
# to reset after par(mfrow = ...) etc.
op <- par()

# .... Load packages -----------------------------------------------------------

# General programming
library(tidyverse)
library(here)
library(glue)
library(magrittr)

# GIS
library(raster)
library(rgdal)
library(rgeos)

# Figures
library(rasterVis)

# .... Source helper functions -------------------------------------------------

function_filenames <- list.files(
  here("analyses-May-2019/functions"),
  full.names = TRUE
)
map(function_filenames, source)

# .... Import environmental data -----------------------------------------------

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
import_environmental_data()

# Inspect environmental data ---------------------------------------------------

map(GCFR_variables,  res)
map(SWAFR_variables, res)
# All at 0.05º x 0.05º resolution

# Plot all layers
par(mfrow = c(3, 3))
iwalk(GCFR_variables,  ~plot(.x, main = glue("GCFR {.y}")))
iwalk(SWAFR_variables, ~plot(.x, main = glue("SWAFR {.y}")))
par(op)

# Plot exemplary layers to investigate edge problems
# (DONE)

# The problem is that some variables have hectic bad values along the coast,
# for e.g. pH:
gplot(GCFR_variables$pH) +
  geom_tile(aes(fill = value))
gplot(SWAFR_variables$pH) +
  geom_tile(aes(fill = value))
# We want to trim the coastal pixels, but without the "buffer" in the inland
# part of the regions.

# We can't mask() with *_border, as we would lose the inland buffer.
# And we can't mask() with *_border_buffered, as we would not trim the coastal
# pixels.

# Hence, we will mask() with a new crisp border (from GADM) that itself has
# been cropped and masked to *_box

# Download &/or import GADM borders
ZA <- getData("GADM",
  country = "ZAF", level = 0,
  path = here("data/raw-data/ZA-border")
)
AU <- getData("GADM",
  country = "AUS", level = 1,
  path = here("data/raw-data/AU-border")
)

# Crop those borders to *_box
ZA_crop <- crop(ZA, GCFR_box)
AU_crop <- crop(AU, SWAFR_box)

# Have a look
plot(ZA_crop)
plot(AU_crop)

# Now mask with those:
# (WARNING: this takes a while...)
if (FALSE) {  # Only run if haven't already run and saved to disc
  GCFR_variables_masked  <- map(GCFR_variables,  mask, ZA_crop)
  SWAFR_variables_masked <- map(SWAFR_variables, mask, AU_crop)
  # Write to disc for safe keeping
  imap(GCFR_variables_masked, ~
    writeRaster(.x, filename = here(
      "data/derived-data/May-2019",
      glue("GCFR_{.y}_masked.tif")
    ))
  )
  imap(SWAFR_variables_masked, ~
    writeRaster(.x, filename = here(
      "data/derived-data/May-2019",
      glue("SWAFR_{.y}_masked.tif")
    ))
  )
}

# Have a look at pH **now**:
gplot(GCFR_variables_masked$pH) +
  geom_tile(aes(fill = value))
gplot(SWAFR_variables_masked$pH) +
  geom_tile(aes(fill = value))
# Hmmm... still need to remove a few coastal pixels. (I remember Mike saying
# it would be good to drop most coastal pixels anyway b/c sand dunes etc.)

# To do this, I need to flag pixels as having <= 5 neighbours
# (for a 3 x 3 neighbourhood) and remove them, or flat those w/ > 5 and
# keep them, whichever is easier.

# As it turns out, it is easier to flag when <= 5 neighbours
# and replace those with NAs:

# Create moving window function to flag pixels w/ <= 5 neighbours
leq5neighbours <- function(x, ...) {
  focal(
    x = x,
    w = matrix(1, nrow = 3, ncol = 3),
    function(x, ...) {
      focal_cell <- x[[5]]
      if (is.na(focal_cell) | is.nan(focal_cell)) {
        return(NA)
      } else {
        # No. neighbours of focal cell = those w/ actual values,
        # excluding the focal cell (hence minus 1)
        n_neighbours <- length(x[!is.na(x) & !is.nan(x)]) - 1
        return(n_neighbours <= 5)
      }
    }
  )
}

# Test this on some data
foo <- SWAFR_variables_masked$pH
# Before:
gplot(foo) +
  geom_tile(aes(fill = value))
# Replace flagged pixels with NAs
flagged <- foo %>%
  leq5neighbours() %>%
  as.vector() %>%
  as.logical() %>%
  which()
foo[flagged] <- NA
# After:
gplot(foo) +
  geom_tile(aes(fill = value))

# Success!
# We do lose the outermost layer of the inlad buffer too, but that's okay!
# Apply to all data
GCFR_variables_masked2 <- map(GCFR_variables_masked, function(x) {
  flagged <- x %>%
    leq5neighbours() %>%
    as.vector() %>%
    as.logical() %>%
    which()
  x[flagged] <- NA
  x
})
SWAFR_variables_masked2 <- map(SWAFR_variables_masked, function(x) {
  flagged <- x %>%
    leq5neighbours() %>%
    as.vector() %>%
    as.logical() %>%
    which()
  x[flagged] <- NA
  x
})
# Write to disc for safe keeping
imap(GCFR_variables_masked2, ~
  writeRaster(.x, filename = here(
    "data/derived-data/May-2019",
    glue("GCFR_{.y}_masked2.tif")
  ))
)
imap(SWAFR_variables_masked2, ~
  writeRaster(.x, filename = here(
    "data/derived-data/May-2019",
    glue("SWAFR_{.y}_masked2.tif")
  ))
)

# Again, have a look at pH **now**:
gplot(GCFR_variables_masked2$pH) +
  geom_tile(aes(fill = value))
gplot(SWAFR_variables_masked2$pH) +
  geom_tile(aes(fill = value))
