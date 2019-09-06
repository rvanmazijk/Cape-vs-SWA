library(here)
source(here("draft-02/manuscript_ver3/R/setup.R"))

tidy_var_names <- c(
  str_replace_all(var_names, ' ', '_')#,
  #"PC1"  # TODO: save to disc before running this script
)

GCFR_heterogneity <- stack(glue(
  "{data_dir}/",
  "GCFR_{tidy_var_names}_masked2_HDS_heterogeneity.tif"
))
names(GCFR_heterogneity) <- tidy_var_names

SWAFR_heterogneity <- stack(glue(
  "{data_dir}/",
  "SWAFR_{tidy_var_names}_masked2_HDS_heterogeneity.tif"
))
names(SWAFR_heterogneity) <- tidy_var_names

tiff(
  here("draft-02/manuscript_ver3/figures/heterogeneity-maps-GCFR.tiff"),
  width = 400, height = 800
)
par(mar = c(3, 3, 0, 0))
plot(
  GCFR_heterogneity,
  xlab = "Longitude", ylab = "Latitude",
  legend = FALSE,
  nr = 5
)
par(op)
dev.off()

plot(
  SWAFR_heterogneity,
  xlab = "Longitude", ylab = "Latitude",
  legend = FALSE,
  nr = 5
)
