# Import data ------------------------------------------------------------------

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS.csv"))
)

# Test for differences in richness and turnover --------------------------------

test_diff <- function(response) {
  dataset <- data %$% {
    if      (response ==     "QDS_richness")                       QDS
    else if (response %in% c("HDS_richness", "QDS_turnover_prop")) HDS
    else if (response %in% c("DS_richness",  "HDS_turnover_prop")) DS
  }
  U_test <- wilcox.test(
    dataset[[response]][dataset$region == "GCFR"],
    dataset[[response]][dataset$region == "SWAFR"],
    conf.int = TRUE
  )
  #print(tidy(U_test))
  tibble(
    metric     = response,
    GCFR_mean  = mean(dataset[[response]][dataset$region == "GCFR"]),
    SWAR_mean  = mean(dataset[[response]][dataset$region == "SWAFR"]),
    P_U        = tidy(U_test)$p.value,
    CLES_value = CLES(
      dataset[[response]][dataset$region == "SWAFR"],
      dataset[[response]][dataset$region == "GCFR"]
    )
  )
}

responses <- c(
  "QDS_richness", "HDS_richness", "DS_richness",
  "QDS_turnover_prop", "HDS_turnover_prop"
)
richness_test_results <- map_dfr(responses, test_diff)

# Save
write_csv(
  richness_test_results,
  here("draft-02/manuscript_ver3-4/results/for-Figure-2.csv")
)

