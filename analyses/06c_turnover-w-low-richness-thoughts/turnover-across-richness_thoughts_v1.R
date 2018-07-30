# Setup ------------------------------------------------------------------------

community <- function(regional_richness, size) {
    regional_pool <- 1:regional_richness
    sample(
        x = regional_pool,
        size = size,
        replace = TRUE
    )
}
richness <- function(x) {
    length(unique(x))
}
jaccard_distance <- function(a, b) {
    (length(dplyr::union(a, b)) - length(dplyr::intersect(a, b))) /
    length(dplyr::union(a, b))
}

# Simple example
a <- community(100, 50)
b <- community(100, 25)
richness(a)
richness(b)
jaccard_distance(a, b)

# Richness ~ community size ----------------------------------------------------

richness_across_sizes <- matrix(nrow = 100, ncol = 100)
for (regional_richness in 1:100) {
    for (size in 1:100) {
        richness_across_sizes[regional_richness, size] <-
            richness(community(regional_richness, size))
    }
}
plot(
    richness_across_sizes[1, ],
    type = "l",
    ylim = c(0, 100),
    ylab = "Richness",
    xlab = "Community size (no. individuals)"
)
for (regional_richness in 2:100) {
    lines(richness_across_sizes[regional_richness, ])
    Sys.sleep(0.1)
}

# Turnover ~ richness of a & b -------------------------------------------------
# w/ community size = 100 always

turnover_across_richnesses <- matrix(nrow = 100, ncol = 100)
for (a in 1:100) {
    for (b in 1:100) {
        turnover_across_richnesses[a, b] <- jaccard_distance(
            community(a, 1000),
            community(b, 1000)
        )
    }
}
plot(
    turnover_across_richnesses[1, ],
    type = "l",
    ylab = "Turnover",
    xlab = "Richness (community B)",
    main = "Lighter = richer community A",
    col = rgb(0, 0, 0, alpha = 1 - (1 / 10))
)
for (a in 2:10) {
    lines(
        turnover_across_richnesses[a, ],
        col = rgb(0, 0, 0, alpha = 1 - (a / 10))
    )
    #Sys.sleep(0.1)
}

library(raster)
plot(
    flip(raster(turnover_across_richnesses), direction = "y"),
    # Flip in y-direction,
    # so that up on the y-axis corresponds to a more rich community a.
    col = seq(0, 1, 0.01),
    xlab = "Community B richness",
    ylab = "Community A richness",
    zlim = c(0, 1),
    legend.args = list(text = "Turnover")
)

