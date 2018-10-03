# Fitting final BRT sets
#   (tc=3, lr=0.001)
# Cape vs SWA publication
# Ruan van Mazijk

source("setup.R")
set.seed(1234)

model_code <- paste0(
  "worker-", Sys.getpid(),
  "_tc-3",
  "_lr-0.001",
  "_", Sys.Date()
)

gbm_steps_simp <- run_final_BRTs(preset = list(tc = 3, lr = 0.001))
saveRDS(
  gbm_steps_simp,
  paste0("final-BRT-set_", model_code, "_BRTs.RDS")
)
