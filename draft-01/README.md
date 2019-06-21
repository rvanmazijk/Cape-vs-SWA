# First draft

<p>
  <img src="logos/UCT-logo.png"       width="100" />
  <img src="logos/BIO-logo.png"       width="100" />
  <img src="logos/eResearch-logo.png" width="150" />
  <img src="logos/DST-logo.png"       width="200" />
  <img src="logos/NRF-logo.png"       width="150" />
  <img src="logos/SAAB-logo.png"      width="150" />
</p>

**Note, all directory names references in code and markdown documents here have not been updated to reflect all these files' new positions within this directory (`draft-01/`), and will thus not run or render**

RVM performed these analyses, wrote this draft and the associate conference presentation.

This, the [first draft](draft-01/) of the manuscript (by RVM), was written using the R package [`bookdown`](https://bookdown.org/), an open-source package for technical writing [R Markdown](https://rmarkdown.rstudio.com/) and [pandoc](https://pandoc.org/). The source .Rmd-files are in [`manuscript/`](manuscript/). The rendered draft of the manuscript is produced in Microsoft Word and LaTeX-PDF formats (see [`manuscript/_manuscript/`](manuscript/_manuscript/)). 

The [conference presentation](https://www.researchgate.net/publication/330262656_Environmental_turnover_predicts_plant_species_richness_turnover_-_Comparing_the_Greater_Cape_Floristic_Region_the_Southwest_Australia_Floristic_Region), based on the [first draft](draft-01) of the manuscript, was presented at the 45th Annual SAAB, AMA and SASSB Joint Congress, held at the University of Johannesburg, Auckland Park, Johannesburg, South Africa in January 2019. RVM presented the core findings of this work, comparing macro-ecological models and environmental correlates of species richness in the Cape and SW Australia. This presentation was created using R Markdown, using the beamer presentation output option.

Both the manuscript drafts and presentation slides are generated using a [Makefile](https://www.gnu.org/s/make/manual/html_node/Introduction.html). In bash or a similar Unix-like shell, simply run `make` with this repository as the working directory to generate the manuscript and slides from the source material. There are, however, some outputs from the analyses that are not automatically generated this way, so do run through the analyses and figure-scripts first if the [outputs-folder](draft-01/outputs/) and figures-folders ([`manuscript/figures/`](draft-01/manuscript/figures/), [`SAAB-AMA-SASSB-2019-talk/figures/`](draft-01/SAAB-AMA-SASSB-2019-talk/figures/)) are empty.

## Acknowledgements

This draft and the conference presentation contains results based on computations performed using facilities provided by the University of Cape Town’s ICTS High Performance Computing team (<http://hpc.uct.ac.za>), for which we are grateful.

