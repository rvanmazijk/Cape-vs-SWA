data <- list(
  QDS = read_csv(glue("{data_dir}/data-QDS-w-residuals.csv")),
  HDS = read_csv(glue("{data_dir}/data-HDS-w-residuals.csv")),
  DS  = read_csv(glue("{data_dir}/data-DS-w-residuals.csv"))
)

plot_PC1_models(data, keep_outliers = FALSE, ext = "pdf")
plot_PC1_models(data, keep_outliers = FALSE, ext = "png")
plot_PC1_models(data, keep_outliers = TRUE,  ext = "pdf")
plot_PC1_models(data, keep_outliers = TRUE,  ext = "png")
