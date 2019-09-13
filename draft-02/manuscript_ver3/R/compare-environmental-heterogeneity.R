# Import and tidy heterogeneity data -------------------------------------------

heterogeneity_point1 <- read_csv(glue("{data_dir}/heterogeneity.csv")) %>%
  filter(scale == "point1") %>%
  split(.$region) %>%
  map(dplyr::select, -scale, -region)

heterogeneity_for_CLES <- read_csv(glue("{data_dir}/data.csv")) %>%
  split(.$region) %>%
  map(~split(.x, .x$scale)) %>%
  map(map, dplyr::select_at,
    c(str_replace_all(var_names, " ", "_"), "PC1")
  )
heterogeneity_for_CLES$GCFR$point1 <- heterogeneity_point1$GCFR
heterogeneity_for_CLES$SWAFR$point1 <- heterogeneity_point1$SWAFR

# CLES analysis ----------------------------------------------------------------

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

# Tidy
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

# Fit linear models of CLES vs scale for each variable -------------------------

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

# Print summary table
as.data.frame(CLES_model_summaries)
