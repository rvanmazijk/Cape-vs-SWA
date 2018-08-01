# Analyse environmental roughness varying across spatial scales
# (Now with jackknife-sampling!)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
map(pre_analysis_import_paths, source)

set.seed(1234)

# Compute and store all pairwise comparisons of roughness in cells -------------

# QDS
pw_comparisons_QDS <- map2(
  .x = GCFR_variables_QDS,
  .y = SWAFR_variables_QDS,
  .f = ~
    list(.x, .y) %>%
    map(prep_layer2) %>%
    pairwise_matrix() %>%
    pairwise_compare()
)
# HDS
pw_comparisons_HDS <- map2(
  .x = GCFR_variables_HDS,
  .y = SWAFR_variables_HDS,
  .f = ~
    list(.x, .y) %>%
    map(prep_layer2) %>%
    pairwise_matrix() %>%
    pairwise_compare()
)
# 3QDS
pw_comparisons_3QDS <- map2(
  .x = GCFR_variables_3QDS,
  .y = SWAFR_variables_3QDS,
  .f = ~
    list(.x, .y) %>%
    map(prep_layer2) %>%
    pairwise_matrix() %>%
    pairwise_compare()
)
# 0.05º
# TODO
# NOTE: do NOT put before QDS, as set.seed affects commands in order
#pw_comparisons_0.05 <- map2(
#  .x = GCFR_variables,
#  .y = SWAFR_variables,
#  .f = ~
#    list(.x, .y) %>%
#    map(prep_layer2) %>%
#    pairwise_matrix() %>%
#    pairwise_compare()
#)

# Jackknife-sample those and get CLES for each jackknife-sample ----------------

# (And not for 3QDS as that is the limiting scale)

n_jackknifes <- 1000
nrow_3QDS <- unique(map_int(pw_comparisons_3QDS, nrow))
ncol_3QDS <- unique(map_int(pw_comparisons_3QDS, ncol))

# QDS
jackknifed_CLES_QDS <- map_df(pw_comparisons_QDS,
  CLES_jackknife,
  n = n_jackknifes,
  size_x = nrow_3QDS,
  size_y = ncol_3QDS
)
# HDS
jackknifed_CLES_HDS <- map_df(pw_comparisons_HDS,
  CLES_jackknife,
  n = n_jackknifes,
  size_x = nrow_3QDS,
  size_y = ncol_3QDS
)
# 0.05º
# TODO
# NOTE: do NOT put before QDS, as set.seed affects commands in order
#jackknifed_CLES_0.05 <- map_df(pw_comparisons_0.05,
#  CLES_jackknife,
#  n = n_jackknifes,
#  size_x = nrow_3QDS,
#  size_y = ncol_3QDS
#)

# Summarise the jackknifed CLES values -----------------------------------------

jackknifed_CLES_summary_QDS <- summarise_all(
  jackknifed_CLES_QDS,
  .funs = list(mean = mean, sd = sd)
)
jackknifed_CLES_summary_HDS <- summarise_all(
  jackknifed_CLES_HDS,
  .funs = list(mean = mean, sd = sd)
)
# 0.05º
# TODO
# NOTE: do NOT put before QDS, as set.seed affects commands in order
#jackknifed_CLES_summary_0.05 <- summarise_all(
#  jackknifed_CLES_0.05,
#  .funs = list(mean = mean, sd = sd)
#)

# Save jackknifed samples + summaries ------------------------------------------

write_csv(
  jackknifed_CLES_QDS,
  here::here("outputs/04_roughness-across-scales/jackknifed_CLES_QDS.csv")
)
write_csv(
  jackknifed_CLES_HDS,
  here::here("outputs/04_roughness-across-scales/jackknifed_CLES_HDS.csv")
)
# TODO
#write_csv(
#  jackknifed_CLES_0.05,
#  here::here("outputs/04_roughness-across-scales/jackknifed_CLES_0.05.csv")
#)

write_csv(
  jackknifed_CLES_summary_QDS,
  here::here("outputs/04_roughness-across-scales/jackknifed_CLES_summary_QDS.csv")
)
write_csv(
  jackknifed_CLES_summary_HDS,
  here::here("outputs/04_roughness-across-scales/jackknifed_CLES_summary_HDS.csv")
)
# TODO
#write_csv(
#  jackknifed_CLES_summary_0.05,
#  here::here("outputs/04_roughness-across-scales/jackknifed_CLES_0.05.csv")
#)
