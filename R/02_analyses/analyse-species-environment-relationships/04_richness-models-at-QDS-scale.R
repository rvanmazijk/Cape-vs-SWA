# Fitting BRTs of richness at QDS-scale
# Cape vs SWA publication
# Ruan van Mazijk

GCFR_model <- gbm.step(
  data = GCFR_data_QDS,
  gbm.x = GCFR_predictor_names_QDS,
  gbm.y = "log_QDS_richness",
  tree.complexity = 3,
  learning.rate = 0.001,
  max.trees = 10000,
  family = "gaussian",
  plot.main = TRUE
)
my_BRT_summary(GCFR_model)
my_BRT_summary(GCFR_model)$contribs

SWAFR_model <- gbm.step(
  data = SWAFR_data_QDS,
  gbm.x = SWAFR_predictor_names_QDS,
  gbm.y = "log_QDS_richness",
  tree.complexity = 3,
  learning.rate = 0.001,
  max.trees = 10000,
  family = "gaussian",
  plot.main = TRUE
)
my_BRT_summary(SWAFR_model)
my_BRT_summary(SWAFR_model)$contribs
