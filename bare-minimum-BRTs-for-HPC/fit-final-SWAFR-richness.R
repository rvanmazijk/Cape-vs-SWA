# Fitting final BRT sets: SWAFR richness model
#   (tc=3, lr=0.001)
# Cape vs SWA publication
# Ruan van Mazijk

source("setup.R")
set.seed(1234)

gbm_steps_simp <- run_final_BRTs(preset = list(tc = 3, lr = 0.001), model = "SWAFR_richness")
saveRDS(
  gbm_steps_simp,
  paste0("final-BRT_SWAFR_richness_BRTs.RDS")
)
