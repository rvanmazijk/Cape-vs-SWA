# Generate environmental roughness data
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

output_path <- here::here("outputs/roughness")

# ... --------------------------------------------------------------------------

source(here::here("setup.R"))
source(here::here("data/import-environmental-data.R"))

# Compute roughness data -------------------------------------------------------

GCFR_roughness <- map(
  .x = GCFR_variables,
  .f = ~ get_roughness(.x)
)
GCFR_roughness_QDS <- map(
  .x = GCFR_variables,
  .f = ~ get_roughness(.x, resolution = 0.25)
)
GCFR_roughness_HDS <- map(
  .x = GCFR_variables,
  .f = ~ get_roughness(.x, resolution = 0.50)
)
GCFR_roughness_3QDS <- map(
  .x = GCFR_variables,
  .f = ~ get_roughness(.x, resolution = 0.75)
)

SWAFR_roughness <- map(
  .x = SWAFR_variables,
  .f = ~ get_roughness(.x)
)
SWAFR_roughness_QDS <- map(
  .x = SWAFR_variables,
  .f = ~ get_roughness(.x, resolution = 0.25)
)
SWAFR_roughness_HDS <- map(
  .x = SWAFR_variables,
  .f = ~ get_roughness(.x, resolution = 0.50)
)
SWAFR_roughness_3QDS <- map(
  .x = SWAFR_variables,
  .f = ~ get_roughness(.x, resolution = 0.75)
)
