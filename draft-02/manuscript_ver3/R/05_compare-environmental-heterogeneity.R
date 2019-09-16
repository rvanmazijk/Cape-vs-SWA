# Import and tidy heterogeneity data -------------------------------------------

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS.csv"))
)

heterogeneity_for_CLES <- data %>%
  map(~split(.x, .x$region)) %>%
  map(map, dplyr::select_at,
    c(str_replace_all(var_names, " ", "_"), "PC1")
  )
heterogeneity_point1 <- read_csv(glue("{data_dir}/heterogeneity.csv")) %>%
  filter(scale == "point1") %>%
  split(.$region) %>%
  map(dplyr::select, -scale, -region)

heterogeneity_for_CLES$point1$GCFR  <- heterogeneity_point1$GCFR
heterogeneity_for_CLES$point1$SWAFR <- heterogeneity_point1$SWAFR

heterogeneity_for_CLES2 <- list()

heterogeneity_for_CLES2$GCFR <- list(
  point1 = heterogeneity_for_CLES$point1$GCFR,
  QDS    = heterogeneity_for_CLES$QDS$GCFR,
  HDS    = heterogeneity_for_CLES$HDS$GCFR,
  DS     = heterogeneity_for_CLES$DS$GCFR
)
heterogeneity_for_CLES2$SWAFR <- list(
  point1 = heterogeneity_for_CLES$point1$SWAFR,
  QDS    = heterogeneity_for_CLES$QDS$SWAFR,
  HDS    = heterogeneity_for_CLES$HDS$SWAFR,
  DS     = heterogeneity_for_CLES$DS$SWAFR
)

heterogeneity_for_CLES <- heterogeneity_for_CLES2

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
#>     variable estimate p.value sig
#> 1  Elevation    0.059   0.168
#> 2        MAP   -0.157   0.095   .
#> 3        PDQ   -0.042   0.401
#> 4  Surface_T   -0.112   0.169
#> 5       NDVI    0.116   0.000   *
#> 6        CEC   -0.043   0.295
#> 7       Clay    0.158   0.037   *
#> 8     Soil_C   -0.030   0.524
#> 9         pH    0.014   0.678
#> 10       PC1   -0.014   0.266
