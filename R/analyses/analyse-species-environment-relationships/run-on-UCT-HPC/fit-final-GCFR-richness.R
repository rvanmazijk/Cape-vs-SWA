# Fitting final BRT sets: GCFR richness model
#   (tc=5, lr=0.001)
# Cape vs SWA publication
# Ruan van Mazijk

source("setup.R")
set.seed(1234)

gbm_steps_simp <- run_final_BRTs(preset = list(tc = 5, lr = 0.001), model = "GCFR_richness")
saveRDS(
  gbm_steps_simp,
  paste0("final-BRT_GCFR_richness_BRTs.RDS")
)
