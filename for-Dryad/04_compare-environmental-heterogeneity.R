# Heterogeneity and species richness:
#   Comparing environmental heterogeneity between GCFR and SWAFR grid-cells
# Ruan van Mazijk, <ruanvmazijk@gmail.com>
# CC-BY-4.0 2021

# Import heterogeneity data ====================================================

data <- list(
  point1 = read_csv("heterogeneity_0.1.csv"),
  QDS    = read_csv("heterogeneity_QDS.csv"),
  HDS    = read_csv("heterogeneity_HDS.csv"),
  DS     = read_csv("heterogeneity_DS.csv")
)

# Remove cells with any missing data (just in case)
data %<>% map(na.exclude)

# CLES analysis ================================================================

# Include PC1 (major axis of heterogeneity) in list of variables to test
vars <- c(var_names_tidy, "PC1")

CLES_results <-
  map_dfr(data, .id = "scale", function(each_scale) {
    # For each scale:
    map_df(vars, function(each_var) {
      # For each heterogeneity variable:
      # Make a dataframe of CLES values and Mann-Whitney U-test results
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

# Fit linear models of CLES vs scale for each variable =========================

CLES_models <- CLES_results %>%
  # Tidy results
  mutate(scale = case_when(
    scale == "point1" ~ 0.10,
    scale == "QDS"   ~ 0.25,
    scale == "HDS"   ~ 0.50,
    scale == "DS"    ~ 1.00
  )) %>%
  mutate(variable = factor(variable, levels = vars)) %>%
  arrange(variable) %>%
  # Split results into a list according to each heterogeneity variable
  split(.$variable) %>%
  # For each variable, regress the CLES score against the spatial scale
  map(~lm(CLES_value ~ scale, .x))

# Summarise those models
CLES_model_summaries <- CLES_models %>%
  # Tidy the model results
  map_df(.id = "variable", tidy) %>%
  filter(term != "(Intercept)") %>%
  # Simplify P-values
  mutate(sig = case_when(
    p.value < 0.05 ~ "*",
    p.value < 0.10 ~ ".",
    TRUE           ~ " "
  )) %>%
  # Round-off coefficients and P--values
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
