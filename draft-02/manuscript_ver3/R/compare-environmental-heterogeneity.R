# CLES-analysis ----------------------------------------------------------------

heterogeneity_for_CLES <- list(
  GCFR = heterogeneity %>%
    map(filter, region == "GCFR") %>%
    map(dplyr::select, -region),
  SWAFR = heterogeneity %>%
    map(filter, region == "SWAFR") %>%
    map(dplyr::select, -region)
)
CLES_results <- heterogeneity_for_CLES %$%
  map2_dfr(GCFR, SWAFR, .id = "scale",
    # For every spatial scale, ...
    ~ map2_df(.x, .y, .id = "variable",
      # ... for every variable in each region, ...
      ~ tibble(
        # ... calculate the CLES, ...
        CLES_value = CLES(.y, .x),
        # ... & run a Mann-Whitney U-test.
        U_test = wilcox.test(.x, .y, conf.int = TRUE) %>%
          tidy() %>%
          list()
      )
    )
  )
CLES_results %<>%
  mutate(
    variable = factor(variable, levels = var_names %>%
      str_replace_all(" ", "_") %>%
      c("PC1")
    ),
    scale = case_when(
      scale == "point1" ~ 0.10,
      scale == "QDS"    ~ 0.25,
      scale == "HDS"    ~ 0.50,
      scale == "DS"     ~ 1.00
    ),
    diff  = map_dbl(U_test, "estimate"),
    P_U   = map_dbl(U_test, "p.value"),
    U_low = map_dbl(U_test, "conf.low"),
    U_upp = map_dbl(U_test, "conf.high")
  ) %>%
  dplyr::select(-U_test)

# Fit linear models of CLES vs scale for each variable
CLES_models <- CLES_results %>%
  split(.$variable) %>%
  map(~lm(CLES_value ~ scale, .x))

# Summarise those models
CLES_model_summaries <- CLES_models %>%
  map_df(.id = "variable", tidy) %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = case_when(
    p.value < 0.05 ~ "*",
    p.value < 0.10 ~ ".",
    TRUE           ~ " "
  )) %>%
  mutate(variable = factor(variable, levels = var_names %>%
    str_replace_all(" ", "_") %>%
    c("PC1")
  )) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  dplyr::select(variable, estimate, p.value, sig)
# Print table
CLES_model_summaries
