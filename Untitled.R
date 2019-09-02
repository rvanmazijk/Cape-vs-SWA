foo <- rbind(
  cbind(region = "GCFR", GCFR_variables %>%
    getValues() %>%
    as.data.frame() %>%
    na.exclude()
  ),
  cbind(region = "SWAFR", SWAFR_variables %>%
    getValues() %>%
    as.data.frame() %>%
    na.exclude()
  )
)
foo_PCA <- prcomp(foo[, -1], center = TRUE, scale. = TRUE)
foo_PCA %$%
  {rotation * scale} %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  as_tibble() %>%
  ggplot(aes(PC1, PC2)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)
foo_PCA %>%
  autoplot(
    data   = foo,
    colour = "region",
    alpha  = 0.1,
    loadings       = TRUE, loadings.colour       = "black",
    loadings.label = TRUE, loadings.label.colour = "black",
    loadings.label.hjust = -0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)
