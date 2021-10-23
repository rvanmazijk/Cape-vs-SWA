png("IBS-ECBC-2021-talk/GCFR_variables.png", width = 900, height = 900)
op <- par()
par(
  mar   = c(2, 0, 0, 0),
  mfrow = c(3, 3)
)
for (i in seq_along(var_names)) {
  plot(GCFR_variables[[i]],  axes = FALSE, legend = FALSE)
}
dev.off()
png("IBS-ECBC-2021-talk/SWAFR_variables.png", width = 900, height = 900)
par(
  mar   = c(2, 0, 0, 0),
  mfrow = c(3, 3)
)
for (i in seq_along(var_names)) {
  plot(SWAFR_variables[[i]], axes = FALSE, legend = FALSE)
}
par(op)
dev.off()

GCFR_heterogeneity_file_names  <- glue(
  "{data_dir}/raster-layers/",
  "GCFR_heterogeneity_{str_replace_all(var_names, ' ', '_')}_QDS.tif"
)
SWAFR_heterogeneity_file_names <- glue(
  "{data_dir}/raster-layers/",
  "SWAFR_heterogeneity_{str_replace_all(var_names, ' ', '_')}_QDS.tif"
)
GCFR_heterogeneity  <- stack(GCFR_heterogeneity_file_names)
SWAFR_heterogeneity <- stack(SWAFR_heterogeneity_file_names)
png("IBS-ECBC-2021-talk/GCFR_heterogeneity.png", width = 900, height = 900)
op <- par()
par(
  mar   = c(2, 0, 0, 0),
  mfrow = c(3, 3)
)
for (i in seq_along(var_names)) {
  plot(GCFR_heterogeneity[[i]],  axes = FALSE, legend = FALSE)
}
dev.off()
png("IBS-ECBC-2021-talk/SWAFR_heterogeneity.png", width = 900, height = 900)
par(
  mar   = c(2, 0, 0, 0),
  mfrow = c(3, 3)
)
for (i in seq_along(var_names)) {
  plot(SWAFR_heterogeneity[[i]], axes = FALSE, legend = FALSE)
}
par(op)
dev.off()
