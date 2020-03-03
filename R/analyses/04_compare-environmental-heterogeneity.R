# Import heterogeneity data ----------------------------------------------------

data <- list(
  point1 = read_csv(glue("{data_dir}/heterogeneity-data-0.10.csv")),
  QDS    = read_csv(glue("{data_dir}/heterogeneity-data-QDS.csv")),
  HDS    = read_csv(glue("{data_dir}/heterogeneity-data-HDS.csv")),
  DS     = read_csv(glue("{data_dir}/heterogeneity-data-DS.csv"))
)

data %<>% map(na.exclude)

# CLES analysis ----------------------------------------------------------------

vars <- c(var_names_tidy, "PC1")

CLES_results <- map_dfr(data, .id = "scale", function(each_scale) {
  map_df(vars, function(each_var) {
    each_scale %$% tibble(
      variable = each_var,
      CLES_value = CLES(
        .[[each_var]][region == "SWAFR"],
        .[[each_var]][region == "GCFR"]
      ),
      P_U =
        wilcox.test(
          .[[each_var]][region == "SWAFR"],
          .[[each_var]][region == "GCFR"]
        ) %>%
        tidy() %>%
        pull(p.value)
    )
  })
})

# Fit linear models of CLES vs scale for each variable -------------------------

CLES_models <- CLES_results %>%
  mutate(scale = case_when(
    scale == "point1" ~ 0.10,
    scale == "QDS"   ~ 0.25,
    scale == "HDS"   ~ 0.50,
    scale == "DS"    ~ 1.00
  )) %>%
  mutate(variable = factor(variable, levels = vars)) %>%
  arrange(variable) %>%
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
  mutate_if(is.numeric, round, digits = 3) %>%
  dplyr::select(variable, estimate, p.value, sig)

# Print summary tables
as.data.frame(CLES_model_summaries)
##     variable estimate p.value sig
## 1  Elevation    0.059   0.168
## 2        MAP   -0.157   0.095   .
## 3        PDQ   -0.042   0.401
## 4  Surface_T   -0.112   0.169
## 5       NDVI    0.116   0.000   *
## 6        CEC   -0.043   0.295
## 7       Clay    0.158   0.037   *
## 8     Soil_C   -0.030   0.524
## 9         pH    0.014   0.678
## 10       PC1   -0.014   0.266

# Save results to disc ---------------------------------------------------------

write_csv(CLES_results,         here("results/CLES-results.csv"))
write_csv(CLES_model_summaries, here("results/CLES-model-summaries.csv"))
