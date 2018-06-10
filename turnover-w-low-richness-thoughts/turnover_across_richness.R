turnover_across_richness <- function(max_richness_master,
                                     regional_richness_master,
                                     plot = TRUE) {

    community <- function(richness, regional_richness) {
        regional_pool <- 1:regional_richness
        sample(
            x = regional_pool,
            size = richness,
            replace = FALSE
        )
    }
    richness <- function(x) {
        length(unique(x))
    }
    jaccard_distance <- function(a, b) {
        (length(dplyr::union(a, b)) - length(dplyr::intersect(a, b))) /
        length(dplyr::union(a, b))
    }

    turnover_results <- matrix(
        nrow = max_richness_master,
        ncol = max_richness_master
    )
    for (a in 1:max_richness_master) {
        for (b in 1:max_richness_master) {
            turnover_results[a, b] <- jaccard_distance(
                community(a, regional_richness_master),
                community(b, regional_richness_master)
            )
        }
    }

    if (plot) {
        library(raster)
        plot(
            flip(raster(turnover_results), direction = "y"),
            # Flip in y-direction,
            # so that up on the y-axis corresponds to a more rich community a.
            col = seq(0, 1, 0.01),
            xlab = "Community B richness",
            ylab = "Community A richness",
            zlim = c(0, 1),
            legend.args = list(text = "Turnover"),
            yaxt = "n",
            xaxt = "n"
        )
        axis(
            side = 2,
            at = c(0.01, 0.51, 1),
            labels = c(1, max_richness_master / 2, max_richness_master)
        )
        axis(
            side = 1,
            at = c(0.01, 0.51, 1),
            labels = c(1, max_richness_master / 2, max_richness_master)
        )
    }

    return(turnover_results)

}
