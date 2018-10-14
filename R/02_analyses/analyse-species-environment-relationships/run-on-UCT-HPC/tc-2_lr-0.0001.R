# Finding ideal BRT-model tc and lr presets:
#   tc=2, lr=0.0001
# Cape vs SWA publication
# Ruan van Mazijk

source("setup.R")
set.seed(1234)

model_code <- paste0(
  "worker-", Sys.getpid(),
  "_tc-2",
  "_lr-0.0001",
  "_", Sys.Date()
)

gbm_steps_simp <- run_initial_BRTs(preset = list(tc = 2, lr = 0.0001))
saveRDS(
  gbm_steps_simp,
  paste0("all-tc-lr-BRTs_", model_code, "_BRTs.RDS")
)
