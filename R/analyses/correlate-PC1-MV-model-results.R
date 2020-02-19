# Import data ------------------------------------------------------------------

data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

# Derive pred. richness of each model from obs. richness & residuals -----------

data$QDS %<>% mutate(
  PC1_expected          = QDS_richness - PC1_residual,
  multivariate_expected = QDS_richness - multivariate_residual
)
data$HDS %<>% mutate(
  PC1_expected          = HDS_richness - PC1_residual,
  multivariate_expected = HDS_richness - multivariate_residual
)
data$DS %<>% mutate(
  PC1_expected          = DS_richness  - PC1_residual,
  multivariate_expected = DS_richness  - multivariate_residual
)

# Correlate pred. richness / residuals from each model -------------------------

cor_model_results <- function(x) {
  x %$% as_tibble(rbind(
    cbind(
      test = "expected",
      tidy(cor.test(multivariate_expected, PC1_expected))
    ),
    cbind(
      test = "residual",
      tidy(cor.test(multivariate_residual, PC1_residual))
    )
  ))
}

data %>%
  map_dfr(.id = "scale", cor_model_results) %>%
  dplyr::select(test, scale, estimate, p.value) %>%
  arrange(test, scale)
