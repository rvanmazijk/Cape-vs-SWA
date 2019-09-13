# Import data ------------------------------------------------------------------

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS.csv"))
)

# Test for differences in richness and turnover --------------------------------

# TODO: functionalise this
richness_test_results <- data %$% rbind(
  QDS %$% tibble(
    metric = "QDS_richness",
    P_U = tidy(wilcox.test(QDS_richness ~ region))$p.value,
    CLES_value = CLES(
      QDS_richness[region == "SWAFR"],
      QDS_richness[region == "GCFR"]
    )
  ),
  HDS %$% tibble(
    metric = "HDS_richness",
    P_U = tidy(wilcox.test(HDS_richness ~ region))$p.value,
    CLES_value = CLES(
      HDS_richness[region == "SWAFR"],
      HDS_richness[region == "GCFR"]
    )
  ),
  DS %$% tibble(
    metric = "DS_richness",
    P_U = tidy(wilcox.test(DS_richness ~ region))$p.value,
    CLES_value = CLES(
      DS_richness[region == "SWAFR"],
      DS_richness[region == "GCFR"]
    )
  ),
  HDS %$% tibble(
    metric = "QDS_turnover_prop",
    P_U = tidy(wilcox.test(QDS_turnover_prop ~ region))$p.value,
    CLES_value = CLES(
      QDS_turnover_prop[region == "SWAFR"],
      QDS_turnover_prop[region == "GCFR"]
    )
  ),
  DS %$% tibble(
    metric = "HDS_turnover_prop",
    P_U = tidy(wilcox.test(HDS_turnover_prop ~ region))$p.value,
    CLES_value = CLES(
      HDS_turnover_prop[region == "SWAFR"],
      HDS_turnover_prop[region == "GCFR"]
    )
  )
)

# Print table
richness_test_results
