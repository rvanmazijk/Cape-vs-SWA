# Finding ideal BRT-model tc and lr presets:
#   tc=3, lr=0.01
# Cape vs SWA publication
# Ruan van Mazijk

source("setup.R")
set.seed(1234)

model_code <- paste0(
  "worker-", Sys.getpid(),
  "_tc-3",
  "_lr-0.01",
  "_", Sys.Date()
)

gbm_steps_simp <- run_initial_BRTs(preset = list(tc = 3, lr = 0.01))
saveRDS(
  gbm_steps_simp,
  paste0("all-tc-lr-BRTs_", model_code, "_BRTs.RDS")
)
