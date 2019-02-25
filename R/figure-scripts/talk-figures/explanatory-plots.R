library(here)

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
png(
  here("SAAB-AMA-SASSB-2019-talk/figures/co-existence.png"), 
  width = 5, height = 5, units = "cm", 
  res = 300
)
plot_coexistence()
dev.off()

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
png(
  here("SAAB-AMA-SASSB-2019-talk/figures/hypothesis.png"), 
  width = 5, height = 5, units = "cm", 
  res = 300
)
plot_hypothesis()
dev.off()

plot_nxn_grid <- function(n,
                          xlab = NULL, ylab = NULL, main = NULL, 
                          cell_lab = NULL, 
                          cell_lab_dir = c("LRTB", "RLTB", "LRBT", "RLBT"),
                          custom_lab = NULL) {
  plot.new()
  if (!is.null(xlab)) mtext(xlab, side = 1, line = 0, las = 1)
  if (!is.null(ylab)) mtext(ylab, side = 2, line = 0, las = 0)
  if (!is.null(main)) mtext(main, side = 3, line = 1, las = 1)
  breaks <- seq(0, 1, 1/n)
  for (i in breaks) {
    abline(h = i)
    abline(v = i)
  }
  if (!is.null(cell_lab)) {
    mids <- vector(length = length(breaks) - 1)
    for (i in 1:(length(breaks) - 1)) {
      mids[[i]] <- mean(c(breaks[[i]],  breaks[[i + 1]]))
    }
    cell_lab_dir <- cell_lab_dir[[1]]  # LRTB default
    y <- switch(cell_lab_dir, 
      "LRTB" = rev(mids), "RLTB" = rev(mids),
      "LRBT" = mids,      "RLBT" = mids
    )
    x <- switch(cell_lab_dir, 
      "LRTB" = mids,      "RLTB" = rev(mids),
      "LRBT" = mids,      "RLBT" = rev(mids)
    )
    i <- 1
    for (y_ in y) {
      for (x_ in x) {
        text(x_, y_, bquote(.(cell_lab)[.(i)]))
        i <- i + 1
      }
    }
  }
  if (!is.null(custom_lab)) {
    stopifnot(is.list(custom_lab))
    text(custom_lab$x, custom_lab$y, custom_lab$labels)
  }
}
plot_nxn_grid(2, xlab = "HDS", cell_lab = "QDS")
plot_nxn_grid(3, custom_lab = list(
  x = 0.5, y = 0.5, 
  labels = bquote(italic("x")["focal"])
))

plot_neighbourhood <- clean_plot(plot_nxn_grid(
  n = 3,
  xlab = bquote(italic("N")),
  custom_lab = list(
    x = 0.5, y = 0.5, 
    labels = bquote(italic("x")["focal"])
  )
))
png(
  here("SAAB-AMA-SASSB-2019-talk/figures/neighbourhood.png"), 
  width = 5, height = 5, units = "cm", 
  res = 300
)
plot_neighbourhood()
dev.off()

plot_HDS_QDS <- clean_plot(plot_nxn_grid(
  n = 2, 
  xlab = "HDS", cell_lab = "QDS"
))
png(
  here("SAAB-AMA-SASSB-2019-talk/figures/HDS-QDS.png"), 
  width = 5, height = 5, units = "cm", 
  res = 300
)
plot_HDS_QDS()
dev.off()
