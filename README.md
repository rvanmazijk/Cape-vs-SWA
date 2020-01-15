Similar axes of environmental heterogeneity associated with plant
species richness in two hyper-diverse floras
================

*Ruan van Mazijk, Michael D. Cramer and G. Anthony Verboom*

  - Department of Biological Sciences, University of Cape Town,
    Rondebosch, South Africa
  - Corresponding author: RvM, <ruanvmazijk@gmail.com>

<p>

<img src="logos/UCT-logo.png"       height="100" />
<img src="logos/BIO-logo.png"       height="100" />
<img src="logos/DST-logo.png"       height="80"  />
<img src="logos/NRF-logo.png"       height="80"  />
<img src="logos/SAAB-logo.png"      height="80"  />

</p>

This is an open access repository for (some) data-sets, reproducible
analyses, conference slides and manuscript drafts for a publication
based on my BSc Hons project (in preparation for submission to the
Journal of Biogeography). Please see the [ResearchGate
page](https://www.researchgate.net/project/Plant-species-richness-turnover-environmental-heterogeneity-in-the-Cape-and-SW-Australia)
for more.

We aim to quantify the explanatory power of heterogeneity in predicting
plant species richness and turnover in the Greater Cape Floristic Region
and in the Southwest Australia Floristic Region. We compare the
environmental heterogeneity in each region, how species richness and
turnover interact in each region to produce the observed patterns of
richness, and what different forms of environmental heterogeneity better
predict richness in each region.

We expect the Cape to be more heterogeneous in most environmental axes,
and at a finer grain, such that the consequent high levels of species
turnover explain the Cape’s greater species richness per unit area. We
also conjecture that edaphic heterogeneity will be an important factor
in predicting richness in SW Australia.

## Acknowledgments

This work was funded by the South African Department of Science and
Technology (DST) and the National Research Foundation (NRF) under the
DST-NRF Freestanding Innovation Honours Scholarship (to RvM), and by the
South African Association of Botanists (SAAB) Honours Scholarship (to
RvM). Thanks go to the Department of Biological Sciences, University of
Cape Town, for providing a 2TB external hard drive for local GIS data
storage.

For results used in earlier drafts of this work and then [conference
presentation](SAAB-AMA-SASSB-2019-talk), any computations were performed
using facilities provided by the University of Cape Town’s ICTS High
Performance Computing team (<http://hpc.uct.ac.za>).

## Workflow & directory structure

<!--html_preserve-->

<div id="htmlwidget-07f50b1a6c5eaafba15b" class="grViz html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-07f50b1a6c5eaafba15b">{"x":{"diagram":"digraph workflow { rankdir = LR splines = true\n  subgraph R_scripts {\n    node [color = blue]\n    subgraph level0_level_R_scripts { rank = same\n      node [label = \"/data_processing/**/*.R\"] data_processing\n      node [label = \"01_setup.R\"]              setup\n    }\n    subgraph level1_analyses_R_scripts { rank = same\n      node\n        [label = \"02_generate-heterogeneity-data.R\"]\n        generate_heterogeneity_data\n      node\n        [label = \"04_compare-species-richness.R\"]\n        compare_species_richness\n      node\n        [label = \"05_compare-environmental-heterogeneity.R\"]\n        compare_environmental_heterogeneity\n      node\n        [label = \"07_correlate-PC1-MV-model-results.R\"]\n        correlate_PC1_MV_model_results\n      node\n        [label = \"08_re-explaining-richness-wo-outliers.R\"]\n        re_explaining_richness_wo_outliers\n      node\n        [label = \"09_check-univariate-linearity.R\"]\n        check_univariate_linearity\n      node\n        [label = \"10_check-collinearity.R\"]\n        check_collinearity\n    }\n    subgraph level2_analyses_R_scripts { rank = same\n      node\n        [label = \"03_generate-richness-and-collate-data.R\"]\n        generate_richness_and_collate_data\n      node\n        [label = \"06_explaining-richness-w-heterogeneity.R\"]\n        explaining_richness_w_heterogeneity\n      subgraph intermediate_data {\n        node [color = green]\n        node [label = \"heterogeneity.csv\"]             heterogeneity\n        node [label = \"data_{scales}.csv\"]             data_scales\n      }\n    }\n    subgraph figure_scripts { rank = same\n      node [label = \"plot-CLES.R\"]                plot_CLES\n      node [label = \"plot-multivariate-models.R\"] plot_multivariate_models\n      node [label = \"plot-PC1-models.R\"]          plot_PC1_models\n      node [label = \"maps.R\"]                     plot_maps\n      node\n        [label = \"plot-richness-distributions-partitions.R\"]\n        plot_richness_distributions_partitions\n    }\n  }\n  subgraph data {\n    subgraph TIFFs {\n      node [color = orange]\n      subgraph rawish_data { rank = same\n        node [label = \"raw-data/\"] raw_data\n        node\n          [label = \"{GCFR,SWAFR}_{var_names}_masked2.tif\"]\n          GCFR_SWAFR_var_names_masked2\n      }\n      subgraph derived_data { rank = same\n        node\n          [label = \"{GCFR,SWAFR}_{var_names}_masked2_{scales}_heterogeneity.tif\"]\n          GCFR_SWAFR_var_names_masked2_scales_heterogeneity\n        node\n          [label = \"{GCFR,SWAFR}_{scales}_richness.tif\"]\n          GCFR_SWAFR_scales_richness\n        node\n          [label = \"{GCFR,SWAFR}_{scales}_PC1.tif\"]\n          GCFR_SWAFR_scales_PC1\n        node\n          [label = \"{GCFR,SWAFR}_{scales}_multivariate_residuals.tif\"]\n          GCFR_SWAFR_scales_multivariate_residuals\n      }\n    }\n    subgraph CSVs {\n      node [color = green]\n      subgraph rawish_data { rank = same\n        node [label = \"/flora/\"]   flora\n        node [label = \"/borders/\"] borders\n      }\n      subgraph derived_data { rank = same\n        node [label = \"{GCFR,SWAFR}_species.csv\"]      GCFR_SWAFR_species\n        node [label = \"data-{scales}-w-residuals.csv\"] data_scales_w_residuals\n      }\n    }\n  }\n  subgraph results { rank = same\n    subgraph tables {\n      node [color = green]\n      node\n        [label = \"{responses}_univariate_model_results.csv\"]\n        responses_univariate_model_results\n      node [label = \"for-Figure-2.csv\"]           for_Figure_2\n      node [label = \"model-summary-for-Tony.csv\"] model_summary_for_Tony\n      node [label = \"model-ANOVA-for-Tony.csv\"]   model_ANOVA_for_Tony\n      node\n        [label = \"refit-model-summary-for-Tony.csv\"]\n        refit_model_summary_for_Tony\n      node\n        [label = \"sd-of-residuals-w-and-wo-outliers.csv\"]\n        sd_of_residuals_w_and_wo_outliers\n      node\n        [label = \"check-univariate-linearity{_best-only}.csv\"]\n        univariate_linearity\n    }\n    subgraph figures {\n      node [color = red]\n      node [label = \"plot-PC-biplots.{png,pdf}\"]          PC_biplots\n      node [label = \"plot-CLES.{pdf,png}\"]                CLES\n      node [label = \"plot-univariate-models.{pdf,png}\"]   univariate_models\n      node [label = \"plot-multivariate-models.{pdf,png}\"] multivariate_models\n      node [label = \"plot-PC1-models.{pdf,png}\"]          PC1_models\n      node\n        [label = \"plot-richness-distributions-partitions.{pdf,png}\"]\n        richness_distributions_partitions\n      node [label = \"maps.{pdf,png}\"]                  maps\n      node [label = \"map-mv-outliers.{pdf,png}\"]       mv_outliers\n      node [label = \"map-PC1-outliers.{pdf,png}\"]      PC1_outliers\n      node [label = \"plot-refit-PC1-models.{pdf,png}\"] refit_PC1_models\n      node\n        [label = \"plot-refit-multivariate-models.{pdf,png}\"]\n        refit_multivariate_models\n      node\n        [label = \"{scales}-log10-heterogeneity-pairs.pdf\"]\n        scales_log10_heterogeneity_pairs\n    }\n  }\n  node [color = black label = \"\"]\n\n  setup ->\n  generate_heterogeneity_data ->\n  generate_richness_and_collate_data\n\n  raw_data ->\n  data_processing ->\n  GCFR_SWAFR_var_names_masked2 ->\n  generate_heterogeneity_data ->\n  { GCFR_SWAFR_var_names_masked2_scales_heterogeneity\n    heterogeneity\n    PC_biplots }\n\n  { borders\n    flora } ->\n  data_processing\n\n  setup ->\n  data_processing ->\n  { borders\n    flora } ->\n  generate_richness_and_collate_data ->\n  { GCFR_SWAFR_species\n    data_scales\n    GCFR_SWAFR_scales_richness\n    GCFR_SWAFR_scales_PC1 }\n\n  { setup\n    data_scales } ->\n  compare_species_richness ->\n  for_Figure_2\n\n  { setup\n    data_scales\n    heterogeneity } ->\n  compare_environmental_heterogeneity ->\n  plot_CLES ->\n  CLES\n\n  { generate_heterogeneity_data\n    data_scales } ->\n  explaining_richness_w_heterogeneity ->\n  { responses_univariate_model_results\n    univariate_models\n    model_summary_for_Tony\n    model_ANOVA_for_Tony\n    data_scales_w_residuals\n    GCFR_SWAFR_scales_multivariate_residuals }\n\n  { setup\n    data_scales_w_residuals } ->\n  correlate_PC1_MV_model_results\n\n  explaining_richness_w_heterogeneity ->\n    plot_multivariate_models ->\n    multivariate_models\n\n  { setup\n    data_scales } ->\n  plot_PC1_models ->\n  PC1_models\n\n  { setup\n    data_scales } ->\n  plot_richness_distributions_partitions ->\n  richness_distributions_partitions\n\n  { setup\n    GCFR_SWAFR_scales_richness\n    GCFR_SWAFR_scales_PC1\n    GCFR_SWAFR_scales_multivariate_residuals } ->\n  plot_maps ->\n  maps\n\n  { setup\n    data_scales_w_residuals\n    borders } ->\n  re_explaining_richness_wo_outliers ->\n  { mv_outliers\n    PC1_outliers\n    refit_PC1_models\n    refit_model_summary_for_Tony\n    refit_multivariate_models\n    sd_of_residuals_w_and_wo_outliers }\n\n  { setup\n    data_scales_w_residuals } ->\n  check_univariate_linearity ->\n  univariate_linearity\n\n  { setup\n    GCFR_SWAFR_var_names_masked2_scales_heterogeneity } ->\n  check_collinearity ->\n  scales_log10_heterogeneity_pairs\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->
