Heterogeneity and species richness: derived data and analyses
R. van Mazijk
CC-BY-4.0 2019

Checklist
================================================================================

- [ ] Data
    - [ ] Cleaned species lists
        - [ ] GCFR
        - [ ] SWAFR
    - [ ] Species occurence matrix (QDS-scale, both regions together)
    - [ ] CSVs (at 3x scales):

      | Column             | QDS | HDS | DS  |
      |====================|=====|=====|=====|
      | Region             |     |     |     |
      | Grid-cell          |     |     |     |
      | Lat.               |     |     |     |
      | Lon.               |     |     |     |
      |--------------------|-----|-----|-----|
      | S                  |     |     |     |
      | Mean S partition   | -   |     |     |
      | Turnover partition | -   |     |     |
      |--------------------|-----|-----|-----|
      | Elev.              |     |     |     |
      | MAP                |     |     |     |
      | PDQ                |     |     |     |
      | Surf. T            |     |     |     |
      | NDVI               |     |     |     |
      | CEC                |     |     |     |
      | Clay               |     |     |     |
      | pH                 |     |     |     |
      | Soil C             |     |     |     |
      | PC1                |     |     |     |
      |--------------------|-----|-----|-----|
      | Res. S [PC1]       |     |     |     |
      | Res. S [MV]        |     |     |     |

    - [ ] Raster layers (at 4x scales):

      | Variable           | 0.01 | QDS  | HDS  | DS   |
      |====================|======|======|======|======|
      | S                  | -    |      |      |      |
      | Mean S partition   | -    | -    |      |      |
      | Turnover partition | -    | -    |      |      |
      |--------------------|------|------|------|------|
      | Elev.              |      |      |      |      |
      | MAP                |      |      |      |      |
      | PDQ                |      |      |      |      |
      | Surf. T            |      |      |      |      |
      | NDVI               |      |      |      |      |
      | CEC                |      |      |      |      |
      | Clay               |      |      |      |      |
      | pH                 |      |      |      |      |
      | Soil C             |      |      |      |      |
      | PC1                |      |      |      |      |
      |--------------------|------|------|------|------|
      | Res. S [PC1]       | -    |      |      |      |
      | Res. S [MV]        | -    |      |      |      |

- [ ] R-scripts
    - [ ] Analyses
        - [ ] 01_setup.R
            - [ ] Incl. all helper-functions
        - [ ] 02_generate-richness-data.R
        - [ ] 03_generate-heterogeneity-data.R
        - [ ] 04_compare-species-richness.R
        - [ ] 05_compare-environmental-heterogeneity.R
        - [ ] 06_explaining-richness-w-heterogeneity.R
            - [ ] Incl. refits w/o richness hotspots
            - [ ] Incl. collinearity checks
            - [ ] Incl. univariate linearity checks
        - [ ] 07_correlate-PC1-MV-model-results.R
    - [ ] Data processing
        - ...
    - Figure-scripts?

- [ ] Larsen shapefile URL
    - [ ] In manuscript too
- [ ] data/raw/docs/SoilGrids250m.txt in this README
- [ ] Other data/raw/docs/*.txt in this README

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
