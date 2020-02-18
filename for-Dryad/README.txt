Heterogeneity and species richness: derived data and analyses
R. van Mazijk
CC-BY-4.0 2019

Checklist
================================================================================

- [ ] Data
    - [x] Cleaned species lists
        - [x] GCFR
        - [x] SWAFR
    - [ ] Species occurence matrix (QDS-scale, both regions together)
    - [ ] CSVs (at 3x scales):

      | Column             | QDS | HDS | DS  |
      |====================|=====|=====|=====|
      | Region             | x   | x   | x   |
      | Grid-cell          | x   | x   | x   |
      | Lat.               | x   | x   | x   |
      | Lon.               | x   | x   | x   |
      |--------------------|-----|-----|-----|
      | S                  | x   | x   | x   |
      | Mean S partition   | -   | x   | x   |
      | Turnover partition | -   | x   | x   |
      |--------------------|-----|-----|-----|
      | Elev.              | x   | x   | x   |
      | MAP                | x   | x   | x   |
      | PDQ                | x   | x   | x   |
      | Surf. T            | x   | x   | x   |
      | NDVI               | x   | x   | x   |
      | CEC                | x   | x   | x   |
      | Clay               | x   | x   | x   |
      | pH                 | x   | x   | x   |
      | Soil C             | x   | x   | x   |
      | PC1                | x   | x   | x   |
      |--------------------|-----|-----|-----|
      | Res. S [PC1]       |     |     |     |
      | Res. S [MV]        |     |     |     |

    - [ ] Raster layers (at 4x scales):

      | Variable           | 0.01 | QDS  | HDS  | DS   |
      |====================|======|======|======|======|
      | S                  | -    | x    | x    | x    |
      | Mean S partition   | -    | -    | x    | x    |
      |--------------------|------|------|------|------|
      | Elev.              |      | x    | x    | x    |
      | MAP                |      | x    | x    | x    |
      | PDQ                |      | x    | x    | x    |
      | Surf. T            |      | x    | x    | x    |
      | NDVI               |      | x    | x    | x    |
      | CEC                |      | x    | x    | x    |
      | Clay               |      | x    | x    | x    |
      | pH                 |      | x    | x    | x    |
      | Soil C             |      | x    | x    | x    |
      | PC1                |      | x    | x    | x    |
      |--------------------|------|------|------|------|
      | Res. S [PC1]       | -    |      |      |      |
      | Res. S [MV]        | -    |      |      |      |

- [ ] R-scripts
    - [ ] Analyses
        - [x] 01_setup.R
            - [x] Incl. all helper-functions
            - [ ] Move data-processing/-used helper functions back there
        - [x] 02_generate-richness-data.R
        - [x] 03_generate-heterogeneity-data.R
            - [ ] Move PCA-biplots -> figure-scripts/
        - [ ] 04_compare-species-richness.R
            - [ ] Move results' CSVs -> results/
        - [ ] 05_compare-environmental-heterogeneity.R
            - [ ] Move results' CSVs -> results/
        - [ ] 06_explaining-richness-w-heterogeneity.R
            - [ ] Incl. refits w/o richness hotspots
            - [ ] Incl. collinearity checks
            - [ ] Incl. univariate linearity checks
            - [ ] Move UVM-plots -> figure-scripts/
            - [ ] Move results' CSVs -> results/
        - [ ] 07_correlate-PC1-MV-model-results.R
    - [ ] Data processing
        - ...
        - [ ] *_create-Larsen-type-grid-rasters.R
        - ...
    - Figure-scripts?

- [ ] Larsen shapefile URL
    - [ ] In manuscript too
- [ ] data/raw/docs/SoilGrids250m.txt in this README
- [ ] Other data/raw/docs/*.txt in this README

- [ ] Merge "for-Dryad/"-approach back into main folders?

Outline and contents
================================================================================

- README.txt
- analyses/
    - 01_setup.R
    - 02_generate-richness-data.R
    - 03_generate-heterogeneity-data.R
    - 04_compare-species-richness.R
    - 05_compare-environmental-heterogeneity.R
    - 06_explaining-richness-w-heterogeneity.R
    - 07_correlate-PC1-MV-model-results.R
- data-processing/
    - ...
- data/
    - data-QDS.csv
    - data-HDS.csv
    - data-DS.csv
    - GCFR-species.csv
    - SWAFR-species.csv
    - species-occurence-matrix-QDS.csv
    - raster-layers/
        - ...
