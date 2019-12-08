# Assessing collinearity between explanatory variables
# (On the QDS/HDS grid)
# Cape vs SWA publication
# Ruan van Mazijk

library(here)
source(here("draft-02/R/setup.R"))

# Import data
HDS <- read_csv(here("draft-02/outputs/QDS_data_cells.csv"))
QDS <- read_csv(here("draft-02/outputs/EDS_data_cells.csv"))

# Remove cells w/ < 4 sub-cells
HDS %<>% filter(n_QDS == 4)
QDS %<>% filter(n_EDS == 4)

# log(x + 1) & scale roughness values to match PCAs were done on logged data
# (See section 3)
HDS[, str_which(names(HDS), "roughness")] %<>%
  log1p() %>%
  scale()
QDS[, str_which(names(QDS), "roughness")] %<>%
  log1p() %>%
  scale()

test_collinearity <- function(df) {
  cor_results <- matrix(nrow = ncol(df), ncol = ncol(df))
  rownames(cor_results) <- colnames(cor_results) <- colnames(df)
  for (i in seq_along(df)) {
    for (j in seq_along(df)) {
      cor_results[i, j] <- cor(df[[i]], df[[j]])
    }
  }
  cor_results %<>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(var1 = rowname) %>%
    gather(var2, cor_coeff, -var1) %>%
    filter(var1 != var2)

  message("These are the most collinear variables (r >= 0.5):")
  cor_results %>%
    filter(cor_coeff >= 0.5) %>%
    distinct(cor_coeff, .keep_all = TRUE) %>%
    print()

  ggplot(cor_results, aes(var1, var2, fill = cor_coeff)) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme(axis.text.x = element_text(angle = 90))
}

test_collinearity(HDS[, str_which(names(HDS), "roughness")])
test_collinearity(QDS[, str_which(names(QDS), "roughness")])
