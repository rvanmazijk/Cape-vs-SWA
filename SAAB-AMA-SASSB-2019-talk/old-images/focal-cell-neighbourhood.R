pdf("focal-cell-neighbourhood.pdf", width = 2, height = 2)
par(mar = c(1, 1, 1, 1))
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
      if (i == 5) bquote(x[focal])
      else bquote(x[.(i)])
    )
    i <- i + 1
  }
}
dev.off()
