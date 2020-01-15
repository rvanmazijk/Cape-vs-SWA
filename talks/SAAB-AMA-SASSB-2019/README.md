<p>
  <img src="../logos/UCT-logo.png"       height="100" />
  <img src="../logos/BIO-logo.png"       height="100" />
  <img src="../logos/DST-logo.png"       height="80"  />
  <img src="../logos/NRF-logo.png"       height="80"  />
  <img src="../logos/SAAB-logo.png"      height="80"  />
  <img src="../logos/eResearch-logo.png" height="80"  />
</p>

**Note, all directory names references in code and markdown documents here have not been updated to reflect all these files' new positions within this directory (`SAAB-AMA-SASSB-2019-talk/`), and will thus not run or render properly.**

RVM performed the analyses and wrote the draft associated with this [conference presentation](https://www.researchgate.net/publication/330262656_Environmental_turnover_predicts_plant_species_richness_turnover_-_Comparing_the_Greater_Cape_Floristic_Region_the_Southwest_Australia_Floristic_Region) was presented at the 45th Annual SAAB, AMA and SASSB Joint Congress, held at the University of Johannesburg, Auckland Park, Johannesburg, South Africa in January 2019. RVM presented the core findings of this work, comparing macro-ecological models and environmental correlates of species richness in the Cape and SW Australia. This presentation was created using R Markdown, using the beamer presentation output option.

Both the manuscript drafts and presentation slides are generated using a [Makefile](https://www.gnu.org/s/make/manual/html_node/Introduction.html). In bash or a similar Unix-like shell, simply run `make` with this repository as the working directory to generate the manuscript and slides from the source material. There are, however, some outputs from the analyses that are not automatically generated this way, so do run through the analyses and figure-scripts first if the [outputs-folder](outputs/) and figures-folders ([`manuscript/figures/`](manuscript/figures/), [`SAAB-AMA-SASSB-2019-talk/figures/`](SAAB-AMA-SASSB-2019-talk/figures/)) are empty.

## Acknowledgements

This results presented in this conference presentation contains relied on computations performed using facilities provided by the University of Cape Town’s ICTS High Performance Computing team (<http://hpc.uct.ac.za>), for which we are grateful.
