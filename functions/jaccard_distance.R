jaccard_distance <- function(a, b) {
  (length(dplyr::union(a, b)) - length(dplyr::intersect(a, b))) /
  length(dplyr::union(a, b))
}
