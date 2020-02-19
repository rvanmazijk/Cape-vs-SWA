# Import richness data ---------------------------------------------------------

data <- list(
  QDS = read_csv(glue("{data_dir}/richness-data-QDS.csv")),
  HDS = read_csv(glue("{data_dir}/richness-data-HDS.csv")),
  DS  = read_csv(glue("{data_dir}/richness-data-DS.csv"))
)

data %<>% map(na.exclude)

# Derive turnover partition of richness at HDS- and DS-scale -------------------

data$HDS %<>%
  mutate(HDS_turnover_prop = (HDS_richness - mean_QDS_richness)/HDS_richness)

data$DS %<>%
  mutate(DS_turnover_prop = (DS_richness - mean_HDS_richness)/DS_richness)

# Test for differences in richness and turnover --------------------------------

responses <- c(
  "QDS_richness", "HDS_richness",      "DS_richness",
                  "HDS_turnover_prop", "DS_turnover_prop"
)
richness_test_results <- map_dfr(responses, test_diff)

# Print results
as.data.frame(richness_test_results)
##              metric    GCFR_mean    SWAR_mean          P_U CLES_value
## 1      QDS_richness  389.9917127  341.0304487 6.243103e-01  0.4899773
## 2      HDS_richness 1073.1549296  839.9104478 6.070644e-02  0.5794618
## 3       DS_richness 2385.8000000 1735.5416667 5.046383e-02  0.7166667
## 4 HDS_turnover_prop    0.6456831    0.5962909 4.029911e-09  0.7499474
## 5  DS_turnover_prop    0.5732989    0.4947685 1.021901e-06  0.9666667

# Save results to disc ---------------------------------------------------------

write_csv(richness_test_results, here("results/richness-test-results.csv"))
