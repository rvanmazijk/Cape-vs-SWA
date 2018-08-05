# Tests for jackknifed roughness analysis pipeline
# Cape vs SWA publication
# Ruan van Mazijk

if (FALSE) {

compare_benchmarks <- function(x, y, n) {
  print(glue(
    "{100 * length(which(x$time < y$time)) / n}% \\
    of times x was faster than using y"
  ))
}

# Choosing betw. expand.grid() and matrix() method of constructing
# pairwise comparison matrix
n <- 50
expand_grid_bm <- microbenchmark(times = n, {
 list(GCFR_variables_3QDS[[1]], SWAFR_variables_3QDS[[1]]) %>%
   map(prep_layer2) %>%
   pairwise_compare(method = "expand.grid")  # Long form pairwise matrix
})
matrix_bm <- microbenchmark(times = n, {
  list(GCFR_variables_3QDS[[1]], SWAFR_variables_3QDS[[1]]) %>%
    map(prep_layer2) %>%
    pairwise_matrix() %>%
    pairwise_compare(method = "matrix")  # Manually fill a matrix
})
compare_benchmarks(matrix_bm, expand_grid_bm, n)
# 100% were faster, t.f. using matrix() method from now on... (default)

# Next step: Parallel-ising this for 0.05º resolution data (to run on Rm3.10 PC)

# First, let's test how long it takes to construct, in parallel w/ 3 cores, a
parallel_once <-
  list(GCFR_variables[[1]], SWAFR_variables[[1]]) %>%
  map(prep_layer2) %>%
  pairwise_matrix() %>%
  pairwise_compare(use_parallel = TRUE)

n <- 10
linear_bm <- microbenchmark(times = n, {
  list(GCFR_variables[[1]], SWAFR_variables[[1]]) %>%
    map(prep_layer2) %>%
    pairwise_matrix() %>%
    pairwise_compare(use_parallel = FALSE)
})
parallel_bm <- microbenchmark(times = n, {
  list(GCFR_variables[[1]], SWAFR_variables[[1]]) %>%
    map(prep_layer2) %>%
    pairwise_matrix() %>%
    pairwise_compare(use_parallel = TRUE)
})
compare_benchmarks(linear_bm, parallel_bm, n)

}
