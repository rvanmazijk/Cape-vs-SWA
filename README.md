# Similar axes of environmental heterogeneity associated with plant species richness in two hyper-diverse floras

_Ruan van Mazijk, Michael D. Cramer and G. Anthony Verboom_

- Department of Biological Sciences, University of Cape Town, Rondebosch, South Africa
- Corresponding author: RvM, ruanvmazijk@gmail.com

<p>
  <img src="logos/UCT-logo.png"       height="100" />
  <img src="logos/BIO-logo.png"       height="100" />
  <img src="logos/DST-logo.png"       height="80"  />
  <img src="logos/NRF-logo.png"       height="80"  />
  <img src="logos/SAAB-logo.png"      height="80"  />
</p>

This is an open access repository for (some) data-sets, reproducible analyses, conference slides and manuscript drafts for a publication based on my BSc Hons project (in preparation for submission to the Journal of Biogeography). Please see the [ResearchGate page](https://www.researchgate.net/project/Plant-species-richness-turnover-environmental-heterogeneity-in-the-Cape-and-SW-Australia) for more.

We aim to quantify the explanatory power of heterogeneity in predicting plant species richness and turnover in the Greater Cape Floristic Region and in the Southwest Australia Floristic Region. We compare the environmental heterogeneity in each region, how species richness and turnover interact in each region to produce the observed patterns of richness, and what different forms of environmental heterogeneity better predict richness in each region.

We expect the Cape to be more heterogeneous in most environmental axes, and at a finer grain, such that the consequent high levels of species turnover explain the Cape's greater species richness per unit area. We also conjecture that edaphic heterogeneity will be an important factor in predicting richness in SW Australia.

## Reproducibility checklist

- [ ] Data
    - [x] Cleaned species lists
        - [x] GCFR
        - [x] SWAFR
    - [ ] Species occurence matrix (QDS-scale, both regions together)
    - [x] CSVs (at 3x scales):
      | Column             | QDS | HDS | DS  |
      |--------------------|-----|-----|-----|
      | Region             | x   | x   | x   |
      | Grid-cell          | x   | x   | x   |
      | Lat.               | x   | x   | x   |
      | Lon.               | x   | x   | x   |
      |                    |     |     |     |
      | S                  | x   | x   | x   |
      | Mean S partition   | -   | x   | x   |
      | Turnover partition | -   | x   | x   |
      |                    |     |     |     |
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
      |                    |     |     |     |
      | Res. S [PC1]       | x   | x   | x   |
      | Res. S [MV]        | x   | x   | x   |
    - [x] Raster layers (at 4x scales):
      | Variable           | 0.01 | QDS  | HDS  | DS   |
      |--------------------|------|------|------|------|
      | S                  | -    | x    | x    | x    |
      | Mean S partition   | -    | -    | x    | x    |
      |                    |      |      |      |      |
      | Elev.              | x    | x    | x    | x    |
      | MAP                | x    | x    | x    | x    |
      | PDQ                | x    | x    | x    | x    |
      | Surf. T            | x    | x    | x    | x    |
      | NDVI               | x    | x    | x    | x    |
      | CEC                | x    | x    | x    | x    |
      | Clay               | x    | x    | x    | x    |
      | pH                 | x    | x    | x    | x    |
      | Soil C             | x    | x    | x    | x    |
      | PC1                | x    | x    | x    | x    |
      |                    |      |      |      |      |
      | Res. S [PC1]       | -    | x    | x    | x    |
      | Res. S [MV]        | -    | x    | x    | x    |
- [ ] R-scripts
    - [ ] Analyses
        - [x] 01_setup.R
            - [ ] Move data-processing/-used helper functions back there
        - [x] 02_generate-richness-data.R
        - [x] 03_generate-heterogeneity-data.R
        - [x] 04_compare-species-richness.R
        - [x] 05_compare-environmental-heterogeneity.R
        - [ ] 06_explaining-richness-w-heterogeneity.R
            - [ ] Move UVM-plots -> figure-scripts/
            - [ ] Move results' CSVs -> results/
            - [ ] Save ANOVA results to disc
            - [ ] Store residuals in rasters
            - [ ] Incl. univariate linearity checks
            - [ ] Incl. collinearity checks
            - [ ] Incl. refits w/o richness hotspots
            - [ ] Incl. correlation of PC1- and multivariate-model-based results
    - [ ] Data processing
        - ...
        - [x] *_create-Larsen-type-grid-rasters.R
            - [ ] Update paths
        - ...
- [ ] Larsen shapefile URL
    - [ ] In manuscript too
- [ ] data/raw/docs/SoilGrids250m.txt in this README
- [ ] Other data/raw/docs/*.txt in this README
- [ ] Range-size figure?

## Acknowledgments

This work was funded by the South African Department of Science and Technology (DST) and the National Research Foundation (NRF) under the DST-NRF Freestanding Innovation Honours Scholarship (to RvM), and by the South African Association of Botanists (SAAB) Honours Scholarship (to RvM). Thanks go to the Department of Biological Sciences, University of Cape Town, for providing a 2TB external hard drive for local GIS data storage.

For results used in earlier drafts of this work and then [conference presentation](SAAB-AMA-SASSB-2019-talk), any computations were performed using facilities provided by the University of Cape Town's ICTS High Performance Computing team (<http://hpc.uct.ac.za>).
