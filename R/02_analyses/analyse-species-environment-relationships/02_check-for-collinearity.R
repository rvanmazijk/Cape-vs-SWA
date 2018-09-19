# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 2: Check for collinearity between environmental variables
# Cape vs SWA publication
# Ruan van Mazijk

# GCFR -------------------------------------------------------------------------

png(
  filename = glue("{output_path}/Cape_collinearity-check_0.8-cutoff_HDS.png"),
  width = 17, height = 15, units = "cm", res = 100
)
GCFR_collinearity <- removeCollinearity(
  raster.stack = GCFR_variables_HDS_stack[[-c(1, 2)]],  # exclude responses
  select.variables = FALSE,  # To do manually
  multicollinearity.cutoff = 0.8,  # My thumb-suck. TODO: try 0.7?
  plot = TRUE
)
dev.off()

# SWAFR ------------------------------------------------------------------------

png(
  filename = glue("{output_path}/SWA_collinearity-check_0.8-cutoff_HDS.png"),
  width = 17, height = 15, units = "cm", res = 100
)
SWAFR_collinearity <- removeCollinearity(
  raster.stack = SWAFR_variables_HDS_stack[[-c(1, 2)]],
  select.variables = FALSE,
  multicollinearity.cutoff = 0.8,
  plot = TRUE
)
dev.off()

# Select final predictor variables ---------------------------------------------

# Take first variable in each collinear cluster
GCFR_predictor_names <- map_chr(GCFR_collinearity, 1)
SWAFR_predictor_names <- map_chr(SWAFR_collinearity, 1)
