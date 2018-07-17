#' Convert 95% confidence intervals to P-values
#'
#' @param est
#' @param u
#' @param l
#' @param t
#'
#' @return
#' @export
#'
#' @examples
CI_to_P <- function(est, u, l, t = 1.96) {
  # Based on <http://www.bmj.com/content/343/bmj.d2304>
  SE <- (u - l) / (2 * t)
  Z <- est / SE
  P <- exp(
    (-0.717 * Z) +
    (-0.416 * (Z ^ 2))
  )
  P
}

#' Title
#'
#' @param x
#' @param y
#' @param alternative
#' @param alpha
#' @param force_mann_whitney_u
#'
#' @return
#' @export
#'
#' @examples
compare_samples <- function(x, y,
                            alternative = c("two.sided", "greater", "less"),
                            alpha = 0.05,
                            force_mann_whitney_u = FALSE) {

  if (force_mann_whitney_u) {

    return(list(test = wilcox.test(x, y, alternative)))

  } else {

    check_assumptions <- function(x, y) {

      test_with <- function(test_function) {
        function(x, y = NULL) {
          if (is.null(y)) {
            p <- test_function(x)$p.value
          } else {
            p <- test_function(x, y)$p.value
          }
          p <= alpha
        }
      }

      test_x_and_y <- function(assumption) {
        function(x, y) {
          (assumption(x) && assumption(y)) ||
          (assumption(log(x + 1)) && assumption(log(y + 1)))
        }
      }

      is_normal <- test_with(shapiro.test)
      both_normal <- test_x_and_y(is_normal)

      have_equvar <- test_with(var.test)

      out <- list(
        normal_and_homoscedastic = NULL,
        log_normal_and_homoscedastic = NULL
      )

      out$normal_and_homoscedastic <-
        both_normal(x, y) &&
        have_equvar(x, y)
      if (!out$normal_and_homoscedastic) {
        out$log_normal_and_homoscedastic <-
          both_normal(log(x + 1), log(y + 1)) &&
          have_equvar(log(x + 1), log(y + 1))
      }

      out

    }

    choose_test <- function(x, y, assumptions) {
      if (assumptions$normal_and_homoscedastic) {
        return(t.test(x, y, alternative))
      } else if (assumptions$log_normal_and_homoscedastic) {
        return(t.test(log(x + 1), log(y + 1), alternative))
      } else {
        return(wilcox.test(x, y, alternative))
      }
    }

    assumptions <- check_assumptions(x, y)
    test <- choose_test(x, y, assumptions)

    return(list(
      assumptions = unlist(assumptions),
      test = test
    ))

  }

}

#' Title
#'
#' @param mu1
#' @param mu2
#' @param sd
#' @param n_sim
#'
#' @return
#' @export
#'
#' @examples
test_error_rate <- function(mu1 = 0, mu2 = mu2, sd = 1, n_sim = 100) {
  type_1_errors <- 0
  for (i in 1:n_sim) {
    sim <- compare_samples(
      x = rnorm(n = 1000, mean = mu1, sd = sd),
      y = rnorm(n = 1000, mean = mu1, sd = sd),
      alternative = "two.sided"
    )
    p <- sim$test$p.value
    if (p <= 0.05) {
      type_1_errors <- type_1_errors + 1
    }
  }
  type_1_error_rate <- type_1_errors / n_sim
  type_1_error_rate
}
