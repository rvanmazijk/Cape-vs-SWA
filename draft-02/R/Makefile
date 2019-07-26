all: 00_analyses.pdf

source = \
  00_analyses.Rmd \
  01_roughness-analyses.Rmd \
  02_turnover-analyses.Rmd \
  03_model-analyses.Rmd \
  03.1_combined-regions-separate-variables.Rmd \
  03.2_combined-regions-combined-variables.Rmd \
  04_appendix.Rmd \
  setup.R \
  functions/*.R

00_analyses.pdf: $(source)
	Rscript -e "\
	library(rmarkdown);\
	render('$<')"
