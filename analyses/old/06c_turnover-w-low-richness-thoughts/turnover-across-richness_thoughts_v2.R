# Setup ------------------------------------------------------------------------

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

# Turnover ~ richness of a & b -------------------------------------------------
# w/ regional pool richness = 100 always

turnover_across_richnesses <- matrix(nrow = 100, ncol = 100)
for (a in 1:100) {
  for (b in 1:100) {
    turnover_across_richnesses[a, b] <- jaccard_distance(
      community(a, 100),
      community(b, 100)
    )
  }
}

# Line plots
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
  # Sys.sleep(0.1)
}

# Colour raster plots
library(raster)
plot(
  flip(raster(turnover_across_richnesses), direction = "y"),
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
axis(side = 2, at = c(0.01, 0.51, 1), labels = c(1, 50, 100))
axis(side = 1, at = c(0.01, 0.51, 1), labels = c(1, 50, 100))
