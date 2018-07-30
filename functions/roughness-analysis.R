#' Implement our roughness function (equation in manuscript)
#'
#' @param x RasterLayer
#' @param ...
#'
#' @return A RasterLayer
focal_sd <- function(x, ...) {focal(x = x,
                                    w = matrix(1, nrow = 3, ncol = 3),
                                    function(x, ...) {
  focal_cell <- x[[5]]
  if (is.na(focal_cell) | is.nan(focal_cell)) return(NA)
  x <- x[!is.na(x) & !is.nan(x)]
  diffs <- vector(length = length(x))
  for (i in seq_along(diffs)) {
    diffs[[i]] <-
      if (!is.na(x[[i]]) & !is.nan(x[[i]])) {
        (focal_cell - x[[i]]) ^ 2
      } else if (x[[i]] == focal_cell) {
        NA
      }
  }
  mean(sqrt(diffs), na.rm = TRUE)
})}
# TODO: rename as "roughness()"

#' Aggregate a layer to 0.05º, run \code{focal_sd()}
#'
#' @description And sub-sample cells if need be
#'
#' @param x RasterLayer
#' @param ...
#'
#' @return A data-frame
prep_layer <- function(x, ...) {
  x %<>%
    aggregate(fact = resolution / 0.05) %>%
    focal_sd() %>%
    getValues()
  if (resolution == 0.05) {
    x %<>% base::sample(size = 5000)  # max n Wilcox test accepts
  }
  x
}

#' Title
#'
#' @param x
#' @param y
#' @param resolution
#' @param raw
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
compare_roughness <- function(x, y, resolution, raw = FALSE, ...) {
  x %<>%
    na.omit() %>%
    prep_layer()
  y %<>%
    na.omit() %>%
    prep_layer()
  test <- compare_samples(x, y, "two.sided", ...)$test
  if (raw) {
    return(test)
  } else {
    return(broom::tidy(test))
  }
}

#' Title
#'
#' @param x
#' @param y
#' @param resolution
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
describe_roughness <- function(x, y, resolution, ...) {
  x %<>% prep_layer()
  y %<>% prep_layer()
  compare_samples(x, y, "two.sided", ...)$assumptions
}

IQ99R <- function(x) quantile(x, 0.99) - quantile(x, 0.01)
IQ95R <- function(x) quantile(x, 0.95) - quantile(x, 0.05)

# Jackknife-version of analysis
prep_layer2 <- function(x, resolution = NULL) {
  print(glue(
    "Creating roughness layers..."
  ))
  x %>%
    focal_sd() %>%
    getValues() %>%
    na.omit()
}
pairwise_matrix <- function(...) {
  print(glue(
    "Constructing pairwise matrix of values..."
  ))
  input <- c(...)
  if (!is.list(input)) {
    input <- list(...)
  }
  x <- input[[1]]
  y <- input[[2]]
  pw <- matrix(nrow = length(x), ncol = length(y))
  rownames(pw) <- x
  colnames(pw) <- y
  pw
}
pairwise_compare <- function(pw, method = "for") {
  stopifnot(method %in% c("for", "expand.grid"))
  print(glue(
    "Comparing values in pw matrix..."
  ))
  row_vals <- as.numeric(rownames(pw))
  col_vals <- as.numeric(colnames(pw))
  pb <- txtProgressBar(0, length(row_vals) * length(col_vals))
  if (method == "for") {
    k <- 0
    for (i in seq_along(rownames(pw))) {
      for (j in seq_along(colnames(pw))) {
        pw[i, j] <- row_vals[[i]] > col_vals[[j]]
        k <- k + 1
        setTxtProgressBar(pb, k)
      }
    }
  } else if (method == "expand.grid") {
    pw <- expand.grid(x = x_vals, y = y_vals)
    for (i in seq(nrow(pw))) {
      pw$x_coord[i] <- which(pw$x[i] == x_vals)
      pw$y_coord[i] <- which(pw$y[i] == y_vals)
      setTxtProgressBar(pb, i)
    }
    close(pb)
    pw$diffs <- pw$x > pw$y
  }
  pw
}
CLES_jackknife <- function(pw, n, size_x, size_y) {
  print(glue(
    "Calculating CLES for each jackknife-sample of the pw matrix..."
  ))
  CLES_values <- vector(length = n)
  pb <- txtProgressBar(0, n)
  for (i in 1:n) {
    random_rows <- sample(seq(max(pw$x_coord)), size_x, replace = FALSE)
    random_cols <- sample(seq(max(pw$y_coord)), size_y, replace = FALSE)
    jackknifed_pw <- filter(pw,
      x_coord %in% random_rows &
      y_coord %in% random_cols
    )
    x_gt_y <- sum(jackknifed_pw$diffs, na.rm = TRUE)
    total <- length(jackknifed_pw)
    CLES_values[[i]] <- x_gt_y / total
    setTxtProgressBar(pb, i)
  }
  close(pb)
  CLES_values
}
