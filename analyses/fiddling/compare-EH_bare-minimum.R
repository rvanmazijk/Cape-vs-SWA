# Comparing regions' environmental heterogeneity (EH)
# GCFR vs SWAFR manuscript
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

library(here)
source(here("draft-02/R/setup.R"))

# Import EH-matrices -----------------------------------------------------------

output_path      <- here("draft-02/outputs/roughness")
resolutions      <- c("base", "EDS", "QDS", "HDS", "3QDS")
matrix_filenames <- glue("{output_path}/{resolutions}_roughness_matrix.csv")

roughness_matrices <- map(matrix_filenames, read_csv)
names(roughness_matrices) <- resolutions

# PCA-analysis -----------------------------------------------------------------

roughness_PCAs <- map(roughness_matrices,
  ~ .x[, -1] %>%
    log1p() %>%
    prcomp(scale. = TRUE)
)

# Force PC1 scores to be positive if all vars rotations are negative
roughness_PCAs %<>% map(function(PCA) {
  if (all(PCA$rotation[, 1] <= 0)) {
    PCA$rotation[, 1] %<>% multiply_by(-1)
    PCA$x[, 1]        %<>% multiply_by(-1)
  }
  PCA
})

# Store PC1 & 2 in matrices for later
roughness_matrices <- map2(roughness_PCAs, roughness_matrices,
  function(PCA, layer) {
    layer$PC1 <- PCA$x[, 1]
    layer$PC2 <- PCA$x[, 2]
    layer
  }
)

# CLES-analysis ----------------------------------------------------------------

set.seed(1234)

# Collate data for this analysis
GCFR_roughness_data <- roughness_matrices %>%
  map(filter, region == "GCFR") %>%
  map(dplyr::select, -region, -PC2)
SWAFR_roughness_data <- roughness_matrices %>%
  map(filter, region == "SWAFR") %>%
  map(dplyr::select, -region, -PC2)

# Calculate the CLESs
CLES_results <- map2_df(GCFR_roughness_data, SWAFR_roughness_data,
  .id = "resolution",  # for every spatial resolution,
  ~ map2_df(.x, .y,
    .id = "variable",  # for every variable in each region,
    ~tibble(
      CLES_value = CLES(.y, .x),
      P_U        = wilcox.test(.y, .x)$p.value
    )
  )
)

# Fit linear models of CLES ~ spatial scale for each variable
CLES_models <- CLES_results %>%
  split(.$variable) %>%
  map(~lm(CLES_value ~ resolution, .x))

# Summarise those models
CLES_model_summaries <- CLES_models %>%
  map_df(.id = "variable", tidy) %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = case_when(
    p.value <= 0.05 ~ "*",
    p.value <= 0.10 ~ ".",
    TRUE            ~ " "
  )) %>%
  mutate(variable = factor(variable, levels = var_names %>%
    str_replace_all(" ", ".") %>%
    c("PC1")
  )) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  dplyr::select(variable, estimate, p.value, sig)

# Have a look at the summary table
as.data.frame(CLES_model_summaries)
