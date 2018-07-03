# Import and/or generate outputs needed for all figures
# Cape vs SWA publication
# Ruan van Mazijk

source(here::here("setup.R"))

pre_analysis_import_paths <- list.files(
  here::here("analyses"),
  pattern = "^\\d{2}_import-.*\\.R",
  full.names = TRUE
)
analysis_paths <- list.files(
  here::here("analyses"),
  pattern = "^\\d{2}_analyse-.*\\.R",
  full.names = TRUE
)

no_ext <- "^[^.]+$"
output_paths <- list.files(
  here::here("outputs"),
  pattern = no_ext,
  full.names = TRUE
)

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
