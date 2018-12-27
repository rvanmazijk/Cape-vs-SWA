clean_plot <- function(x) {
  function() {
    op <- par()
    on.exit(par(op))
    par(mar = c(1, 1, 1, 1))
    eval(substitute(
      x, 
      parent.env(environment())  # to allow multiple evals of returned fun
    ))
  }
}
plot_coexistence <- clean_plot({
  # Generate random temp and rainfall preference data for 2x species
  temp <- c(rnorm(20, -0.5, 0.25), rnorm(20, 0.5, 0.25))
  rain <- c(rnorm(20, -0.5, 0.25), rnorm(20, 0.5, 0.25))
  species <- c(rep("green", 20), rep("red", 20))
  plot(
    temp, rain, col = species, 
    pch = 16,
    xaxt = "n", yaxt = "n"
  )
  title(xlab = "Temperature", line = 0)
  title(ylab = "Rainfall", line = 0)
})
plot_hypothesis <- clean_plot({
  richness <- c(0.2, 0.8)
  heterogeneity <- c(0.2, 0.8)
  region <- c("blue", "orange")
  plot(
    heterogeneity, richness, col = NA,
    xlim = c(0, 1), ylim = c(0, 1),
    xaxt = "n", yaxt = "n"
  )
  abline(0, 1, lty = "dashed")
  points(heterogeneity, richness, col = region, pch = 16)
  mtext("Heterogeneity", side = 1, line = 0, las = 1)
  mtext("S", side = 2, line = 0, las = 1)
})
plot_neighbourhood <- clean_plot({
  plot.new()
  # Draw 3x3 grid
  for (i in c(0, 0.33, 0.66, 1)) {
    abline(h = i)
    abline(v = i)
  }
  # Populate with labels x1 to x9, with xfocal a.o.t. x5
  i <- 1
  for (y in c(0.81, 0.48, 0.15)) {
    for (x in c(0.15, 0.48, 0.81)) {
      text(x, y, labels =
        if (i == 5) bquote(italic(x)[focal])
        #else if (x_i_labels) bquote(italic(x)[.(i)])
      )
      i <- i + 1
    }
  }
})
