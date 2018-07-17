# Import and/or generate outputs needed for all figures
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))

if (all(!folder_is_empty(output_paths))) {
  map(output_paths, import_objects)
} else {
  map(pre_analysis_import_paths, source)
  map2(
    output_paths, analysis_paths,
    source_if_needed, import = TRUE
  )
}

var_names <- c(
  "Elevation",
  "MAP",
  "PDQ",
  "Surface T",
  "NDVI",
  "CEC",
  "Clay",
  "Soil C",
  "pH"
)
