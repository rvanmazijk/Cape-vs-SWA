CI_to_P <- function(est, u, l, t = 1.96) {
    # Based on <http://www.bmj.com/content/343/bmj.d2304>
    SE <- (u - l) / (2 * t)
    Z <- est / SE
    P <- exp(
        (-0.717 * Z) +
        (-0.416 * (Z ^ 2))
    )
    return(P)
}
