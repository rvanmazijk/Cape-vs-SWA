# Linking patterns of plant species richness and turnover to environmental heterogeneity in two Mediterranean flora: the Cape and Southwest Australia

_Ruan van Mazijk, Michael D. Cramer and G. Anthony Verboom_

- Department of Biological Sciences, University of Cape Town, Rondebosch, South Africa
- Corresponding author: RVM (ruanvmazijk@gmail.com, +27 21 650 3684)

<p>
  <img src="logos/UCT-logo.png"       width=100/>
  <img src="logos/BIO-logo.png"       width=100/>
  <img src="logos/eResearch-logo.png" width=150/>
  <img src="logos/DST-logo.png"       width=200/>
  <img src="logos/NRF-logo.png"       width=150/>
  <img src="logos/SAAB-logo.png"      width=150/> 
</p>

This is an open access repository for data-sets, reproducible analyses and the manuscript for a publication in prep. Please see the [ResearchGate page](https://www.researchgate.net/project/Plant-species-richness-turnover-environmental-heterogeneity-in-the-Cape-and-SW-Australia) for more.

We aim to quantify the explanatory power of heterogeneity in predicting plant species richness and turnover in the Greater Cape Floristic Region and in the Southwest Australia Floristic Region. We compare the environmental heterogeneity in each region, how species richness and turnover interact in each region to produce the observed patterns of richness, and what different forms of environmental heterogeneity better predict richness in each region.

We expect the Cape to be more heterogeneous in most environmental axes, and at a finer grain, such that the consequent high levels of species turnover explain the Cape's greater species richness per unit area. We also conjecture that edaphic heterogeneity will be an important factor in predicting richness in SW Australia.

## Acknowledgments

This work was funded by the South African Department of Science and Technology (DST) and the National Research Foundation (NRF) under the DST-NRF Freestanding Innovation Honours Scholarship (to RVM), and by the South African Association of Botanists (SAAB) Honours Scholarship (to RVM). Thanks go to the Department of Biological Sciences, University of Cape Town, for providing a 2TB external hard drive for local GIS data storage. Many computations were performed using facilities provided by the University of Cape Town’s ICTS High Performance Computing team (<http://hpc.uct.ac.za>).

<p>
  <img src="logos/BIO-logo.png"       width=80/>
  <img src="logos/eResearch-logo.png" width=100/>
  <img src="logos/DST-logo.png"       width=180/>
  <img src="logos/NRF-logo.png"       width=100/>
  <img src="logos/SAAB-logo.png"      width=100/>
</p>

## Reproducing the analyses

All data processing and analyses were performed using R (see `R/`).

First, environmental and species occurrence data are processed (see [`R/data-processing/`](R/data-processing/)). Second the analyses are performed ([`R/analyses/`](R/analyses/)). Lastly, figures are produced for both the manuscript and conference presentation using outputs from the analyses and the processed data ([`R/figure-scripts/`](R/figure-scripts/)).

Most scripts in `R/` call on [`R/setup.R`](R/setup.R), and scripts in [`R/functions/`](R/functions/) as needed, to load necessary packages, environmental variables and helper-functions used through-out this project

## The manuscript

This manuscript was written using [`bookdown`](https://bookdown.org/), an open-source package for technical writing using R, R Markdown and pandoc. The source .Rmd-files are in [`manuscript/`](manuscript/). The rendered draft of the manuscript is produced in MS Word and PDF formats (see [`manuscript/_manuscript/`](manuscript/_manuscript/)).

The [conference presentation](https://www.researchgate.net/publication/330262656_Environmental_turnover_predicts_plant_species_richness_turnover_-_Comparing_the_Greater_Cape_Floristic_Region_the_Southwest_Australia_Floristic_Region) was presented at the 45th Annual SAAB, AMA and SASSB Joint Congress, held at the University of Johannesburg, Auckland Park, Johannesburg, South Africa in January 2019. RVM presented the core findings of this work, comparing macro-ecological models and environmental correlates of species richness in the Cape and SW Australia. This presentation was created using [`rmarkdown`](https://rmarkdown.rstudio.com/), an open source R-package for document preparation, using the beamer presentation output option.

Both the manuscript drafts and presentation slides are generated using a [Makefile](https://www.gnu.org/s/make/manual/html_node/Introduction.html). In bash or a similar Unix-like shell, simply run `make` with this repository as the working directory to generate the manuscript and slides from the source material. There are, however, some outputs from the analyses that are not automatically generated this way, so do run through the analyses and figure-scripts first if the [outputs-folder](outputs/) and figures-folders ([`manuscript/figures`](manuscript/figures), [`SAAB-AMA-SASSB-2019-talk/figures`](SAAB-AMA-SASSB-2019-talk/figures)) are empty.
