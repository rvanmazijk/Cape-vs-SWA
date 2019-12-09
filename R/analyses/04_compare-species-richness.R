# Import data ------------------------------------------------------------------

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS.csv"))
)

# Test for differences in richness and turnover --------------------------------

test_diff <- function(response, sub_sample = FALSE) {
  dataset <- data %$% {
    if      (response ==     "QDS_richness")                       QDS
    else if (response %in% c("HDS_richness", "QDS_turnover_prop")) HDS
    else if (response %in% c("DS_richness",  "HDS_turnover_prop")) DS
  }

  x_GCFR  <- dataset[[response]][dataset$region == "GCFR"]
  x_SWAFR <- dataset[[response]][dataset$region == "SWAFR"]

  if (sub_sample) {
    # Ensure GCFR and SWAFR are compared w/ same no. of cells
    n <- min(length(x_GCFR), length(x_SWAFR))
    x_GCFR  %<>% sample(n)
    x_SWAFR %<>% sample(n)
  }

  U_test <- wilcox.test(x_GCFR, x_SWAFR, conf.int = TRUE)
  tibble(
    metric     = response,
    GCFR_mean  = mean(x_GCFR),
    SWAR_mean  = mean(x_SWAFR),
    P_U        = tidy(U_test)$p.value,
    CLES_value = CLES(x_SWAFR, x_GCFR)
  )
}

responses <- c(
  "QDS_richness", "HDS_richness", "DS_richness",
  "QDS_turnover_prop", "HDS_turnover_prop"
)
richness_test_results <-
  map_dfr(responses, test_diff)
richness_test_results_sub_sample <-
  map_dfr(responses, test_diff, sub_sample = TRUE)

# Save
write_csv(
  richness_test_results,
  here("results/for-Figure-2.csv")
)
write_csv(
  richness_test_results,
  here("results/for-Figure-2_sub-sampled.csv")
)

