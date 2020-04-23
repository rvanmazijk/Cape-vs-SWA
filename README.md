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

## Acknowledgments

This work was funded by the South African Department of Science and Technology (DST) and the National Research Foundation (NRF) under the DST-NRF Freestanding Innovation Honours Scholarship (to RvM), and by the South African Association of Botanists (SAAB) Honours Scholarship (to RvM). Thanks go to the Department of Biological Sciences, University of Cape Town, for providing a 2TB external hard drive for local GIS data storage.

For results used in earlier drafts of this work and the [conference presentation](SAAB-AMA-SASSB-2019-talk), many computations were performed using facilities provided by the University of Cape Town's ICTS High Performance Computing team (<http://hpc.uct.ac.za>).

## TODO

### Reproducibility

- [ ] Trim out NA-rows from data CSVs for Dryad
- [ ] Export species occurence matrix (QDS-scale, both regions together)?
- Re: R-scripts
    - [ ] Smooth over scripts' general neatness, comments
    - [ ] Incl. univariate linearity checks in modelling script
    - Re: data processing scripts
        - [x] *_create-Larsen-type-grid-rasters.R
        - [ ] Other scripts as "example"-scripts, not fully functional ones
        - [ ] Move data-processing/-used helper functions back -> data-processing/*.R
        - [ ] Update paths in *_create-Larsen-type-grid-rasters.R
- [ ] Add Larsen shapefile URL to this README
- [ ] Add contents of data/raw/docs/SoilGrids250m.txt to this README
- [ ] Add contents of other data/raw/docs/*.txt to this README

### Manuscript

- [ ] Address negative heterogeneity-effects
- [ ] Other results + discussion smoothing over
- [ ] Comment more on figures
- [ ] Double check references
- [ ] Double check figures

### Submission to _J. Biogeog._

- [ ] Recirculate manuscript to Mike + Tony
- [ ] Finish cover-letter to ed.
- [ ] Ask Mike + Tony for advice on reviewers
- [ ] Submit!
