Heterogeneity and species richness:
  Analyses in R and derived datasets
Ruan van Mazijk, <ruanvmazijk@gmail.com>
CC-BY-4.0 2021

Outline:
  R-scripts
  CSV-files
    Inputs
    Outputs
      Analyses' results
      Based on raster-files
  Shape-files (inputs only)
  Raster-files
    Inputs
    Outputs

== R-scripts ===================================================================

The analysis is reproducible using these 5 R-scripts, run in numbered order:

`00_setup.R`
  (loads necessary packages, defines repeatedly used helper functions,
  and imports input data)
`01_generate-richness-data.R`
  (uses the input GBIF occurrence data to collate vascular plant species
  richness and turnover in grid-cells)
`02_generate-heterogeneity-data.R`
  (uses the input environmental data to calculate various forms of environmental
  heterogeneity and the major axis thereof through PCA)
`03_compare-species-richness.R`
  (uses the data resulting from script #01 to compare GCFR and SWAFR cells'
  species richness and turnover values)
`04_compare-environmental-heterogeneity.R`
  (uses the data resulting from script #02 to compare GCFR and SWAFR cells'
  environmental heterogeneity values)
`05_explaining-richness-w-heterogeneity.R`
  (uses the data resulting from scripts #01 and #02 to regress species richness
  against environmental heterogeneity across the GCFR and SWAFR,
  using a variety of univariate and multivariate models)

Note, the other files in this includes both inputs and the outputs of the above
scripts. Consequently, running these scripts will re-generate the outputs.

Additionally, be sure to un-zip all the .zip-files, as these contain shape-files
needed as inputs for parts of the analysis.

== CSV-files ===================================================================

-- Inputs ----------------------------------------------------------------------

`cleaned-species-occ_GCFR.csv`
`cleaned-species-occ_SWAFR.csv`

Cleaned and filtered vascular plant species (= Tracheophyta) occurrence data
based on that from GBIF, within the GCFR and SWAFR. Note, this includes
occurrences of taxa with <5 occurrences in either region (as opposed to the
shape-file `species_occ2.zip`, below).

Citations:

GBIF.org (24 July 2017) GBIF Occurrence Download.
DOI: https://doi.org/10.15468/dl.n6u6n0.
URL: https://www.gbif.org/occurrence/download/0005227-170714134226665.

GBIF.org (24 July 2017) GBIF Occurrence Download.
DOI: https://doi.org/10.15468/dl.46okua.
URL: https://www.gbif.org/occurrence/download/0005227-170714134226665.

-- Outputs ---------------------------------------------------------------------

Analyses' results:

`comparing-residuals-w-and-wo-outliers_F-tests.csv`
`comparing-residuals-w-and-wo-outliers.csv`
`list-outlier-squares.csv`
`multivariate-model-ANOVAs.csv`
`multivariate-model-results_refit.csv`
`multivariate-model-results.csv`

The summaries and results from univariate models at various spatial scales have
file-names of the form:

`<scale>_richness_univariate_model_results.csv`

E.g.: `QDS_richness_univariate_model_results.csv`

Outputs based on raster-files:

These contain the species richness and environmental heterogeneity data in GCFR
and SWAFR grid cells. They have file-names of the form:

`species-richness_<scale>.csv`
E.g.: `species-richness_QDS.csv`

`heterogeneity_<scale>.csv`
E.g.: `heterogeneity_QDS.csv`

== Shape-files (inputs only) ===================================================

Be sure to un-zip all these .zip-files, as these contain the shape-files needed
as inputs for parts of the analysis.

The boundaries of the two regions used here:

`GCFR_border_buffered.zip`
`SWAFR_border_buffered.zip`

Various spatial scales' grid-cell lattices, with file-names of the form:

`Larsen_grid_<scale>.zip`
E.g.: `Larsen_grid_QDS.zip`

Lastly, `species_occ2.zip` contains the cleaned and filtered vascular plant
species (= Tracheophyta) occurrence data, additionally filtered to only contain
occurrences of taxa with ≥5 occurrences in either region, as a shape-file.

== Raster-files ================================================================

Note, the suffixes of file-names here denote the spatial scale, as follows:

QDS = quarter-degree square resolution
HDS = half-degree square resolution
DS  = degree square resolution

-- Inputs ----------------------------------------------------------------------

Raster versions of the grid-cell lattice shape-files above,
with file-names of the form:

`Larsen_grid_<scale>_ras.tif`
E.g.: `Larsen_grid_QDS_ras.tif`

Absolute environmental variables (0.05ºx0.05º resolution) in each region
separately, with file-names of the form:

`<region>_<variable>.tif`
E.g.: `GCFR_CEC.tif`, `SWAFR_CEC.tif`

-- Outputs ---------------------------------------------------------------------

Raster-form species richness data, with file-names of the form:

`species-richness_<scale>.tif`
E.g.: `species-richness_QDS.tif`

`mean-<sub-cell scale>-richness_<scale>.tif`
E.g.: `mean-QDS-richness_HDS.tif`

Environmental heterogeneity layers based on the absolute environmental variables
above (derived as the variance of sub-grid-cell values within a grid-cell). The
file-names are of the form:

`heterogeneity-<variable>_<scale>.tif`
E.g.: `heterogeneity-CEC_QDS.tif`

And finally, raster-form residuals from PC1-based univariate models and
multivariate models of species richness at various spatial scales.
File-names are of the form:

`MV-residual_<scale>.tif`
E.g.: `MV-residual_QDS.tif`

`PC1-residual_<scale>.tif`
E.g.: `PC1-residual_QDS.tif`
