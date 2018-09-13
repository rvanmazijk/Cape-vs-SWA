# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 1: Setup and data collation
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))
source(here("R/02_analyses/generate-roughness.R"))
source(here("R/02_analyses/generate-turnover.R"))

output_path <- here("outputs/species-environment-relationships")

library(dismo)
library(virtualspecies)

# HDS-scale for now:

# Combine all data -------------------------------------------------------------

# Tidy names(<region>_species) after SpatialPointsDataFrame import
names(GCFR_species) <- c(
  #"family", "genus", "species",
  "species", "cell_nos",
  "qdgc", "hdgc",
  "HDS_richness", "n_QDS", "mean_QDS_richness", "mean_QDS_turnover"
)
names(SWAFR_species) <- c(
  #"family", "genus", "species",
  "species", "cell_nos",
  "qdgc", "hdgc",
  "HDS_richness", "n_QDS", "mean_QDS_richness", "mean_QDS_turnover"
)

variables_HDS_stacks <- pmap(
  # For each region:
  .l = list(
    vars = list(GCFR_variables, SWAFR_variables),
    rough_vars = list(GCFR_roughness_HDS, SWAFR_roughness_HDS),
    species = list(GCFR_species, SWAFR_species)
  ),
  .f = function(vars, rough_vars, species) {

    # Generate absolute environmental values at HDS-scale
    vars %<>% map(aggregate, fact = 0.50 / 0.05)

    # Add roughnes layers to data list
    names(rough_vars) %<>% paste0("rough_", .)
    vars %<>% c(rough_vars)

    # Convert NA-turnover to nonsensicle value (rasterize() can't handles NAs)
    species$mean_QDS_turnover[is.na(species$mean_QDS_turnover)] <- 9999

    # Add raster of HDS richness and mean QDS turnover to data list
    species_rasters <- list(
      HDS_richness = rasterize(
        species,
        vars$Elevation,
        field = "HDS_richness"
      ),
      mean_QDS_turnover = rasterize(
        species,
        vars$Elevation,
        field = "mean_QDS_turnover"
      )
    )

    # Convert nonsense back to NAs
    nonsense <- species_rasters$mean_QDS_turnover == 9999
    species_rasters$mean_QDS_turnover[nonsense] <- NA

    # Add richness and turnover to data list
    vars <- c(species_rasters, vars)

    # And make it all a RasterStack
    stack(vars)
  }
)

GCFR_variables_HDS_stack <- variables_HDS_stacks[[1]]
SWAFR_variables_HDS_stack <- variables_HDS_stacks[[2]]
