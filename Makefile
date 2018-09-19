INDEX = manuscript/index.Rmd
BODY = $(wildcard manuscript/*_*.Rmd)
AFTER_BODY_RMD = manuscript/_after-body.Rmd
META = \
	manuscript/_bookdown.yml \
	manuscript/_output.yml \
	manuscript/Cape-vs-SWA.bib \
	manuscript/journal-of-biogeography.csl
FIGURES_R = $(wildcard R/figures/fig-*.R)
OUTPUTS = $(wildcard outputs/*/*.csv)
FUNCTIONS = $(wildcard R/functions/*.R)

FIGURES_PNG = $(wildcard figures/fig-*.R)
AFTER_BODY_TEX = $(AFTER_BODY_TEX:.Rmd=.tex)
PDF = manuscript/_manuscript/Van-Mazijk-et-al_in-prep.pdf

$(PDF): $(INDEX) $(BODY) $(AFTER_BODY_TEX) $(META) $(OUTPUTS) $(FUNCTIONS)
	Rscript -e "\
	setwd('manuscript'); \
	library(bookdown); \
	render_book('$<', 'bookdown::pdf_document2')"

$(AFTER_BODY_TEX): $(AFTER_BODY_RMD) $(FIGURES_PNG) $(OUTPUTS) $(FUNCTIONS)
	Rscript -e "\
	library(rmarkdown); \
	render('$<', 'latex_fragment')"
	# NOTE: setwd("manuscript") not needed for fragments

figures/fig-%.png: R/figures/fig-%.R
	Rscript -e "source('$<')"

# Outputs that are needed by the ms and figures are re-generated
# automatically by R within the ms and figure scripts
