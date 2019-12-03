# Import data ------------------------------------------------------------------

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

# Check for collinearity within regions' data ----------------------------------

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

# Check for collinearity with both regions merged ------------------------------

# .... Merge regions' data -----------------------------------------------------

# Have different origins
map(GCFR_heterogeneity,  origin)
map(SWAFR_heterogeneity, origin)

# Ammend origins to be same
set_origin <- function(r, ox = 0, oy = 0) {
  origin(r) <- c(ox, oy)
  r
}
GCFR_heterogeneity2  <- map(GCFR_heterogeneity,  set_origin)
SWAFR_heterogeneity2 <- map(SWAFR_heterogeneity, set_origin)

# Check
map(GCFR_heterogeneity2, origin)
map(SWAFR_heterogeneity2, origin)

# Merge
heterogeneity2 <- map2(GCFR_heterogeneity2, SWAFR_heterogeneity2, merge)

# Rename layers
heterogeneity2 %<>% map(function(r) {
  names(r) <- str_replace_all(var_names, " ", "_")
  r
})

# .... Do the collinearity check proper ----------------------------------------

imap(heterogeneity2, function(each_scale, each_scales_name) {
  pdf(
    glue("{each_scales_name}-log10-heterogeneity-pairs.pdf"),
    width = 10, height = 10
  )
  pairs(log10(each_scale), cor = TRUE)
  dev.off()
})
