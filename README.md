# Linking patterns of plant species richness and turnover to environmental heterogeneity in two Mediterranean flora: the Cape and Southwest Australia

*Ruan van Mazijk[1,§], Michael D. Cramer[1] and G. Anthony Verboom[1]*

- [1] Department of Biological Sciences, University of Cape Town, Rondebosch, South Africa
- [§] Corresponding author: ruanvmazijk@gmail.com, +27 21 650 3684

<p>
  <img src="logos/UCT-logo.png" width=100>
  <img src="logos/BIO-logo.png" width=100>
</p>

## Preamble

This is an open access repository for data-sets, reproducible analyses and the manuscript for a publication in prep. Please see the [ResearchGate page](https://www.researchgate.net/project/Plant-species-richness-turnover-environmental-heterogeneity-in-the-Cape-and-SW-Australia) for more.

We aim to quantify the explanatory power of heterogeneity in predicting plant species richness and turnover in the Greater Cape Floristic Region and in the Southwest Australia Floristic Region. We compare the environmental heterogeneity in each region, how species richness and turnover interact in each region to produce the observed patterns of richness, and what different forms of environmental heterogeneity better predict richness in each region.

We expect the Cape to be more heterogeneous in most environmental axes, and at a finer grain, such that the consequent high levels of species turnover explain the Cape's greater species richness per unit area. We also conjecture that edaphic heterogeneity will be an important factor in predicting richness in SW Australia.

## Acknowledgments

This work is funded by the South African Department of Science and Technology (DST) and the National Research Foundation (NRF) under the DST-NRF Innovation Honours Scholarship (to RVM), and by the South African Association of Botanists (SAAB) Honours Scholarship (to RVM).

<p>
  <img src="logos/DST-logo.png" width=180>
  <img src="logos/NRF-logo.png" width=100>
  <img src="logos/SAAB-logo.png" width=100>
</p>

## Technical details

This manuscript was written using [`bookdown`](https://bookdown.org/), an open-source package for technical writing using R, R Markdown and pandoc.

All analyses were carried out in R. The workflow used to write this paper was roughly as follows (illustrated using [GraphViz](https://www.graphviz.org/)), outlining the roles of the various sub-directories of this repository:

![](docs/repo-structure.svg)

Raw-data is processed and analysed. Data-processing, analyses and figure production use R packages and bespoke code, loaded by `setup.R`. Derived data and analyses' outputs are used to produce figures. Analyses' outputs, figures and manuscript components are compiled using `bookdown`.

A more detailed layout of the relationships between this repository's components is described [here](https://rvanmazijk.github.io/Cape-vs-SWA/docs/repo-structure-detailed.svg), also using [GraphViz](https://www.graphviz.org/).
