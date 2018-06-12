compare_samples <- function(x, y,
                            alternative = c("two.sided", "greater", "less"),
                            alpha = 0.05,
                            force_mann_whitney_u = FALSE) {

    if (force_mann_whitney_u) {

        return(list(test = wilcox.test(
            x,
            y,
            alternative
        )))

    } else {

        check_assumptions <- function(x, y) {

            test_with <- function(test_function) {
                function(x, y = NULL) {
                    if (is.null(y)) {
                        p <- test_function(x)$p.value
                    } else {
                        p <- test_function(x, y)$p.value
                    }
                    if (p <= alpha) {
                        return(FALSE)
                    } else {
                        return(TRUE)
                    }
                }
            }

            test_x_and_y <- function(assumption) {
                function(x, y) {
                    if (assumption(x) && assumption(y)) {
                        return(TRUE)
                    } else if (assumption(log(x + 1)) && assumption(log(y + 1))) {
                        return(TRUE)
                    } else {
                        return(FALSE)
                    }
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

            return(out)

        }

        choose_test <- function(x, y, assumptions) {
            if (assumptions$normal_and_homoscedastic) {
                t.test(
                    x,
                    y,
                    alternative
                )
            } else if (assumptions$log_normal_and_homoscedastic) {
                t.test(
                    x = log(x + 1),
                    y = log(y + 1),
                    alternative = alternative
                )
            } else {
                print(wilcox.test(
                    x,
                    y,
                    alternative
                ))
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
    return(type_1_error_rate)
}
