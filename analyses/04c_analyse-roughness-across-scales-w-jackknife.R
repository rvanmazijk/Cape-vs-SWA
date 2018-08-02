# Analyse environmental roughness varying across spatial scales
# (Now with jackknife-sampling!)
# Cape vs SWA publication
# Ruan van Mazijk

# Setup ------------------------------------------------------------------------

source(here::here("setup.R"))
map(pre_analysis_import_paths, source)

set.seed(1234)

# Compute and store all pairwise comparisons of roughness in cells -------------

# .... 0.05 --------------------------------------------------------------------

# Let's plan how long this should take
if (FALSE) {

  n_GCFR_cells <- length(prep_layer2(GCFR_variables[[1]]))
  n_SWAFR_cells <- length(prep_layer2(SWAFR_variables[[1]]))
  print(glue(
    "There are {n_GCFR_cells} x {n_SWAFR_cells} = \\
    {n_GCFR_cells * n_SWAFR_cells} comparisons \\
    to do at the 0.05 x 0.05 degree scale"
  ))
  n <- 10
  size <- 1000
  pw_benchmark_1e9 <- microbenchmark(times = n, {
    pairwise_compare(matrix(
      nrow = size, ncol = size,
      dimnames = list(seq(size), seq(size))
    ))
  })
  time_bench_comparisons <- mean(pw_benchmark_1e9$time / 1e9)  # Convert to s
  time_GCFR_SWAFR_comparisons <-
    (n_GCFR_cells * n_SWAFR_cells) %>%
    divide_by(size ^ 2) %>%  # Convert to millions
    multiply_by(time_bench_comparisons) %>%  # Time in s
    divide_by(60)  # Convert to minutes
  print(glue(
    "Linearly running 1000000 comparisons takes ca. \\
    {format(time_bench_comparisons, digits = 4)} seconds.
    Thus, running {n_GCFR_cells * n_SWAFR_cells} comparisons should take ca. \\
    {format(time_GCFR_SWAFR_comparisons, digits = 4)} minutes.
    For 9 variables, this should take at least \\
    {format((9 * time_GCFR_SWAFR_comparisons) / 60, digits = 4)} hours total.
    Because things rarely go *that* well, \\
    let's be safe and leave 6 to 8 hours for this."
  ))

}


# Done:
parallel_elev_pw <-
  list(GCFR_variables[[1]], SWAFR_variables[[1]]) %>%
  map(prep_layer2) %>%
  pairwise_matrix() %>%
  pairwise_compare(use_parallel = TRUE)
write_csv(
  as.data.frame(parallel_elev_pw),
  here::here("outputs/04_roughness-across-scales/pw-comparisons_Elevation_0.05_parallel_2018-08-02.csv")
)
rm(parallel_elev_pw)

# TODO:
parallel_MAP_pw <-
  list(GCFR_variables[[2]], SWAFR_variables[[2]]) %>%
  map(prep_layer2) %>%
  pairwise_matrix() %>%
  pairwise_compare(use_parallel = TRUE)
write_csv(
  as.data.frame(parallel_MAP_pw),
  here::here("outputs/04_roughness-across-scales/pw-comparisons_MAP_0.05_parallel_2018-08-02.csv")
)
rm(parallel_MAP_pw)

# TODO:
parallel_PDQ_pw <-
  list(GCFR_variables[[3]], SWAFR_variables[[3]]) %>%
  map(prep_layer2) %>%
  pairwise_matrix() %>%
  pairwise_compare(use_parallel = TRUE)
write_csv(
  as.data.frame(parallel_PDQ_pw),
  here::here("outputs/04_roughness-across-scales/pw-comparisons_PDQ_0.05_parallel_2018-08-02.csv")
)
rm(parallel_PDQ_pw)

# TODO:
parallel_surfT_pw <-
  list(GCFR_variables[[4]], SWAFR_variables[[4]]) %>%
  map(prep_layer2) %>%
  pairwise_matrix() %>%
  pairwise_compare(use_parallel = TRUE)
write_csv(
  as.data.frame(parallel_surfT_pw),
  here::here("outputs/04_roughness-across-scales/pw-comparisons_Surface-T_0.05_parallel_2018-08-02.csv")
)
rm(parallel_surfT_pw)

# TODO:
parallel_NDVI_pw <-
  list(GCFR_variables[[5]], SWAFR_variables[[5]]) %>%
  map(prep_layer2) %>%
  pairwise_matrix() %>%
  pairwise_compare(use_parallel = TRUE)
write_csv(
  as.data.frame(parallel_NDVI_pw),
  here::here("outputs/04_roughness-across-scales/pw-comparisons_NDVI_0.05_parallel_2018-08-02.csv")
)
rm(parallel_NDVI_pw)

# TODO:
parallel_CEC_pw <-
  list(GCFR_variables[[6]], SWAFR_variables[[6]]) %>%
  map(prep_layer2) %>%
  pairwise_matrix() %>%
  pairwise_compare(use_parallel = TRUE)
write_csv(
  as.data.frame(parallel_CEC_pw),
  here::here("outputs/04_roughness-across-scales/pw-comparisons_CEC_0.05_parallel_2018-08-02.csv")
)
rm(parallel_CEC_pw)

# TODO:
parallel_clay_pw <-
  list(GCFR_variables[[7]], SWAFR_variables[[7]]) %>%
  map(prep_layer2) %>%
  pairwise_matrix() %>%
  pairwise_compare(use_parallel = TRUE)
write_csv(
  as.data.frame(parallel_clay_pw),
  here::here("outputs/04_roughness-across-scales/pw-comparisons_Clay_0.05_parallel_2018-08-02.csv")
)
rm(parallel_clay_pw)

# TODO:
parallel_soilC_pw <-
  list(GCFR_variables[[8]], SWAFR_variables[[8]]) %>%
  map(prep_layer2) %>%
  pairwise_matrix() %>%
  pairwise_compare(use_parallel = TRUE)
write_csv(
  as.data.frame(parallel_soilC_pw),
  here::here("outputs/04_roughness-across-scales/pw-comparisons_Soil-C_0.05_parallel_2018-08-02.csv")
)
rm(parallel_soilC_pw)

# TODO:
parallel_pH_pw <-
  list(GCFR_variables[[9]], SWAFR_variables[[9]]) %>%
  map(prep_layer2) %>%
  pairwise_matrix() %>%
  pairwise_compare(use_parallel = TRUE)
write_csv(
  as.data.frame(parallel_pH_pw),
  here::here("outputs/04_roughness-across-scales/pw-comparisons_pH_0.05_parallel_2018-08-02.csv")
)
rm(parallel_pH_pw)

# .... QDS ---------------------------------------------------------------------

pw_comparisons_QDS <- map2(
  .x = GCFR_variables_QDS,
  .y = SWAFR_variables_QDS,
  .f = ~
    list(.x, .y) %>%
    map(prep_layer2) %>%
    pairwise_matrix() %>%
    pairwise_compare()
)

# .... HDS ---------------------------------------------------------------------

pw_comparisons_HDS <- map2(
  .x = GCFR_variables_HDS,
  .y = SWAFR_variables_HDS,
  .f = ~
    list(.x, .y) %>%
    map(prep_layer2) %>%
    pairwise_matrix() %>%
    pairwise_compare()
)

# .... 3QDS --------------------------------------------------------------------

pw_comparisons_3QDS <- map2(
  .x = GCFR_variables_3QDS,
  .y = SWAFR_variables_3QDS,
  .f = ~
    list(.x, .y) %>%
    map(prep_layer2) %>%
    pairwise_matrix() %>%
    pairwise_compare()
)

# Jackknife-sample those and get CLES for each jackknife-sample ----------------

# (And not for 3QDS as that is the limiting scale)

n_jackknifes <- 1000
nrow_3QDS <- unique(map_int(pw_comparisons_3QDS, nrow))
ncol_3QDS <- unique(map_int(pw_comparisons_3QDS, ncol))

# QDS
jackknifed_CLES_QDS <- map_df(pw_comparisons_QDS,
  CLES_jackknife,
  pw_format = "matrix",
  n = n_jackknifes,
  size_x = nrow_3QDS,
  size_y = ncol_3QDS
)
# HDS
jackknifed_CLES_HDS <- map_df(pw_comparisons_HDS,
  CLES_jackknife,
  pw_format = "matrix",
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
