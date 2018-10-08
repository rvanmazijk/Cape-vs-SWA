# Fitting GCFR richness ~ turnover
#   (tc=3, lr=0.001)
# Cape vs SWA publication
# Ruan van Mazijk

source("setup.R")
set.seed(1234)

saveRDS(
  fit_gbm_step(
    GCFR_variables_HDS,
    "mean_QDS_turnover", "HDS_richness", log_response = TRUE,
    tc = 3, lr = 0.001, nt = 10000
  ),
  "BRT_GCFR_richness-turnover.RDS"
)

