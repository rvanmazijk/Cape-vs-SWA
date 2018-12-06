set.seed(1234)
BOTH_model <- gbm.step(
  data = BOTH_data_QDS,
  gbm.x = BOTH_predictor_names_QDS,
  gbm.y = "log_QDS_richness",
  tree.complexity = 3,
  learning.rate = 0.001,
  max.trees = 10000,
  family = "gaussian",
  plot.main = TRUE
)
my_BRT_summary(BOTH_model)
my_BRT_summary(BOTH_model)$contribs
plot(
  BOTH_model$fit ~ BOTH_model$data$y,
  col = BOTH_data_QDS$region
)

BOTH_data_QDS$region %<>% as.factor()
set.seed(1234)
BOTH_model_w_discrete <- gbm.step(
  data = BOTH_data_QDS,
  gbm.x = c("region", BOTH_predictor_names_QDS),
  gbm.y = "log_QDS_richness",
  tree.complexity = 3,
  learning.rate = 0.001,
  max.trees = 10000,
  family = "gaussian",
  plot.main = TRUE
)
my_BRT_summary(BOTH_model_w_discrete)
my_BRT_summary(BOTH_model_w_discrete)$contribs
plot(
  BOTH_model_w_discrete$fit ~ BOTH_model_w_discrete$data$y,
  col = BOTH_data_QDS$region
)

y_hat1 <- predict(
  BOTH_model_w_discrete,
  newdata = cbind(region = "GCFR", GCFR_data_QDS),
  n.trees = 10000
)
y_hat2 <- predict(
  BOTH_model,
  newdata = GCFR_data_QDS,
  n.trees = 10000
)
y <- GCFR_data_QDS$log_QDS_richness
plot((y_hat1 - y) ~ y)
abline(lm((y_hat1 - y) ~ y))
abline(h = 0, lty = "dashed")
plot((y_hat2 - y) ~ y)
abline(lm((y_hat2 - y) ~ y))
abline(h = 0, lty = "dashed")
y_hat1 <- predict(
  BOTH_model_w_discrete,
  newdata = cbind(region = "SWAFR", SWAFR_data_QDS),
  n.trees = 10000
)
y_hat2 <- predict(
  BOTH_model,
  newdata = SWAFR_data_QDS,
  n.trees = 10000
)
y <- SWAFR_data_QDS$log_QDS_richness
plot((y_hat1 - y) ~ y)
abline(lm((y_hat1 - y) ~ y))
abline(h = 0, lty = "dashed")
plot((y_hat2 - y) ~ y)
abline(lm((y_hat2 - y) ~ y))
abline(h = 0, lty = "dashed")
