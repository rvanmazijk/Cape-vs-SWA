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

# Plot PCA biplots
roughness_PCA_plots <- map2(roughness_PCAs, roughness_matrices,
  ~ autoplot(.x,
      data = .y, colour = "region", alpha = 0.25,
      loadings = TRUE, loadings.colour = "black",
      loadings.label = TRUE, loadings.label.colour = "black",
      loadings.label.hjust = -0.25
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)
)
# Get legend to use as panel
my_legend <- get_legend(roughness_PCA_plots[[1]])
# Remove legends from PCA plots themselves
roughness_PCA_plots %<>% map(~.x + theme(legend.position = "none"))
# Store panels in list
roughness_PCA_plots <- c(roughness_PCA_plots, my_legend = list(my_legend))
# Plot panels
plot_grid(
  plotlist = roughness_PCA_plots,
  labels   = c("0.05º", "EDS", "QDS", "HDS", "3QDS", ""),
  nrow     = 3
)

# CLES-analysis ----------------------------------------------------------------

# Run CLES-analyses OR import it's results if run & saved previously
if (!file.exists(glue("{output_path}/CLES_results.csv"))) {
  # (WARNING: this takes a while... Only run if haven't already...)
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
      ~tibble(CLES_value = CLES(.y, .x))
    )
  )
  # Save results to disc
  write_csv(
    CLES_results,
    glue("{output_path}/CLES_results.csv")
  )
} else {
  # (... Or import from disc.)
  CLES_results <- read_csv(glue("{output_path}/CLES_results.csv"))
}

# Tidy up the results
CLES_results %<>%
  filter(variable != "region", variable != "PC2") %>%
  mutate(resolution = case_when(
    resolution == "base" ~ 0.05,
    resolution == "EDS"  ~ 0.125,
    resolution == "QDS"  ~ 0.25,
    resolution == "HDS"  ~ 0.50,
    resolution == "3QDS" ~ 0.75
  )) %>%
  mutate(variable = factor(variable, levels = c(
    "Elevation",
    "MAP", "PDQ", "Surface.T",
    "NDVI",
    "CEC", "Clay", "Soil.C", "pH",
    "PC1"
  )))

# Tested for CLES being different to 0.5 across scales
CLES_results2 <- CLES_results %>%
  split(.$variable) %>%
  map(pull, CLES_value)
# Test for normality first
CLES_results2 %>%
  map(shapiro.test) %>%
  map_df(.id = "variable", tidy) %>%
  dplyr::select(variable, p.value) %>%
  mutate(sig = p.value < 0.05)
# Result: All normally distributed (P ≥ 0.05)
# Now test for different to 0.5 with two-sided t-tests
CLES_results2 %>%
  map(t.test, mu = 0.5) %>%
  map_df(.id = "variable", tidy) %>%
  dplyr::select(variable, p.value) %>%
  mutate(sig = p.value < 0.05)

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
# Have a loog at the table
as.data.frame(CLES_model_summaries)
