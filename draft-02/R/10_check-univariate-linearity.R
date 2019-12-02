GCFR_heterogeneity <- map(c(QDS = "QDS",
                             HDS = "HDS",
                             DS  = "DS"), function(each_scale) {
  vars <- brick(map(var_names, function(each_var) {
    each_var %<>% str_replace_all(" ", "_")
    raster(glue(
      "{data_dir}/",
      "GCFR_{each_var}_masked2_{each_scale}_heterogeneity.tif"
    ))
  }))
  names(vars) <- str_replace_all(var_names, " ", "_")
  vars
})
SWAFR_heterogeneity <- map(c(QDS = "QDS",
                             HDS = "HDS",
                             DS  = "DS"), function(each_scale) {
  vars <- brick(map(var_names, function(each_var) {
    each_var %<>% str_replace_all(" ", "_")
    raster(glue(
      "{data_dir}/",
      "SWAFR_{each_var}_masked2_{each_scale}_heterogeneity.tif"
    ))
  }))
  names(vars) <- str_replace_all(var_names, " ", "_")
  vars
})

pdf("GCFR-QDS-log10-heterogeneity-pairs.pdf", width = 10, height = 10)
pairs(log10(GCFR_heterogeneity$QDS),  cor = TRUE)
dev.off()

pdf("SWAFR-QDS-log10-heterogeneity-pairs.pdf", width = 10, height = 10)
pairs(log10(SWAFR_heterogeneity$QDS), cor = TRUE)
dev.off()

pdf("GCFR-HDS-log10-heterogeneity-pairs.pdf", width = 10, height = 10)
pairs(log10(GCFR_heterogeneity$HDS),  cor = TRUE)
dev.off()

pdf("SWAFR-HDS-log10-heterogeneity-pairs.pdf", width = 10, height = 10)
pairs(log10(SWAFR_heterogeneity$HDS), cor = TRUE)
dev.off()

pdf("GCFR-DS-log10-heterogeneity-pairs.pdf", width = 10, height = 10)
pairs(log10(GCFR_heterogeneity$DS),  cor = TRUE)
dev.off()

pdf("SWAFR-DS-log10-heterogeneity-pairs.pdf", width = 10, height = 10)
pairs(log10(SWAFR_heterogeneity$DS), cor = TRUE)
dev.off()
