# Fitting permuted BRT sets
#   (tc=3, lr=0.001)
# Cape vs SWA publication
# Ruan van Mazijk

source("setup.R")

run_permuted_BRTs(
  preset = list(tc = 3, lr = 0.001),
  model_config = model_configs$GCFR_richness,
  model_config_name = "GCFR_richness"
)
run_permuted_BRTs(
  preset = list(tc = 3, lr = 0.001),
  model_config = model_configs$GCFR_turnover,
  model_config_name = "GCFR_richness"
)
run_permuted_BRTs(
  preset = list(tc = 3, lr = 0.001),
  model_config = model_configs$SWAFR_richness,
  model_config_name = "SWAFR_richness"
)
run_permuted_BRTs(
  preset = list(tc = 3, lr = 0.001),
  model_config = model_configs$SWAFR_turnover,
  model_config_name = "SWAFR_turnover"
)
