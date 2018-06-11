focal_sd <- function(x, ...) {
    focal(
        x = x,
        w = matrix(1, nrow = 3, ncol = 3),
        function(x, ...) {
            diffs <- vector(length = 8)
            diffs[1] <- (x[5] - x[1]) ^ 2
            diffs[2] <- (x[5] - x[2]) ^ 2
            diffs[3] <- (x[5] - x[3]) ^ 2
            diffs[4] <- (x[5] - x[4]) ^ 2
            diffs[5] <- (x[5] - x[6]) ^ 2
            diffs[6] <- (x[5] - x[7]) ^ 2
            diffs[7] <- (x[5] - x[8]) ^ 2
            diffs[8] <- (x[5] - x[9]) ^ 2
            return(sqrt(mean(diffs)))
        }
    )
}

prep_layer <- function(x, ...) {
    x %<>%
        aggregate(fact = resolution / 0.05) %>%
        focal_sd() %>%
        `[`()
    if (resolution == 0.05) {
        x %<>% base::sample(size = 5000)  # maxi sample size Wilcox test accepts
    }
    return(x)
}

compare_roughness <- function(x, y, resolution, raw = FALSE, ...) {
    x %<>% prep_layer()
    y %<>% prep_layer()
    test <- compare_samples(x, y, "two.sided", ...)$test
    if (raw) {
        return(test)
    } else {
        return(broom::tidy(test))
    }
}

describe_roughness <- function(x, y, resolution, ...) {
    x %<>% prep_layer()
    y %<>% prep_layer()
    return(compare_samples(x, y, "two.sided", ...)$assumptions)
}
