# ...
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("R/setup.R"))
source(here(
  "R/analyses/",
  "analyse-species-environment-relationships/01_collate-data.R"
))

output_path <- here(
  "outputs/species-environment-relationships/from-local-machines/"
)

# Inspect collated data --------------------------------------------------------

GCFR_data_QDS_stack
SWAFR_data_QDS_stack
GCFR_data_HDS_stack
SWAFR_data_HDS_stack

max_richness <- max(na.rm = TRUE,
  getValues(GCFR_data_QDS_stack$QDS_richness),
  getValues(SWAFR_data_QDS_stack$QDS_richness)
)

# Import representative BRT-models ---------------------------------------------

# FIXME: get to work on Ubuntu machine
model_paths <- c(
  "GCFR_HDS-richness-gbm-step_BRT_2018-12-06_312.RDS",
  "GCFR_HDS-turnover-gbm-step_BRT_2018-12-11_746.RDS",
  "GCFR_QDS-richness-gbm-step_BRT_2018-12-04_305.RDS",
  "SWAFR_HDS-richness-gbm-step_BRT_2018-12-06_325.RDS",
  "SWAFR_HDS-turnover-gbm-step_BRT_2018-12-10_406.RDS",
  "SWAFR_QDS-richness-gbm-step_BRT_2018-12-03_59.RDS"
)
models <- model_paths %>%
  paste0(output_path, .) %>%
  map(read_rds)
names(models) <- c(
  "GCFR_HDS_richness",
  "GCFR_HDS_turnover",
  "GCFR_QDS_richness",
  "SWAFR_HDS_richness",
  "SWAFR_HDS_turnover",
  "SWAFR_QDS_richness"
)
