# Inputs -----------------------------------------------------------------------

# Find all function scripts, output CSVs
FUNCTIONS = $(wildcard functions/*.R)
OUTPUTS = $(wildcard outputs/*/*.csv)

# Find all figure scripts
FIGURES_R = $(wildcard figures/fig-*.R)
# And assume figures have their names, but .R -> .png
FIGURES_PNG = $(FIGURES_R:.R=.png)

# Find all numbered ms .Rmd fils
BODY = $(wildcard manuscript/*_*.Rmd)

# Outputs ----------------------------------------------------------------------

# PDF output depends on
# 	- index.Rmd,
#		- the ms body,
#		- the rendered after-body,
# 	- ms meta-files,
# 	- analyses' outputs
#		- some functions (used in ms body and after-body)
manuscript/_manuscript/Van-Mazijk-et-al_in-prep.pdf: manuscript/index.Rmd $(BODY) manuscript/_after-body.tex manuscript/_bookdown.yml manuscript/_output.yml manuscript/Cape-vs-SWA.bib manuscript/journal-of-biogeography.csl $(OUTPUTS) $(FUNCTIONS)
	Rscript -e "setwd('manuscript'); library(bookdown); render_book('$<', 'bookdown::pdf_document2')"

# The rendered after-body depends on:
# 	- its source .Rmd file,
# 	- the rendered figures,
#  	- analyses' outputs
#		- some functions
manuscript/_after-body.tex: manuscript/_after-body.Rmd $(FIGURES_PNG) $(OUTPUTS) $(FUNCTIONS)
	Rscript -e "library(rmarkdown); render('$<', 'latex_fragment')"
	# NOTE: setwd("manuscript") not needed for fragments

# Remake any figures if their source scripts change
figures/fig-%.png: figures/fig-%.R
	Rscript -e "source('$<')"

# Outputs that are needed by the ms and figures are re-generated
# automatically by R within the ms and figure scripts
