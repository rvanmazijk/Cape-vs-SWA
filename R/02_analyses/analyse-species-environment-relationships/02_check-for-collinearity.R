# Analyse value of environmental & heterogeneity variables for predicting
#   vascular plant species richness and turnover---using BRTs
# Part 2: Check for collinearity between environmental variables
# Cape vs SWA publication
# Ruan van Mazijk

output_path <- here("outputs/species-environment-relationships")

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

# For bare-minimum BRT work on UCT HPC:
output_path <- here(
  "R/02_analyses/",
  "analyse-species-environment-relationships/run-on-UCT-HPC/"
)
write.csv(
  GCFR_predictor_names,
  glue("{output_path}/GCFR_predictor_names.csv")
)
write.csv(
  SWAFR_predictor_names,
  glue("{output_path}/SWAFR_predictor_names.csv")
)
