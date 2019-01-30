# Manuscript
INDEX = manuscript/index.Rmd
BODY = $(wildcard manuscript/*_*.Rmd)
AFTER_BODY_RMD = manuscript/_after-body.Rmd
MS_META = \
	manuscript/_bookdown.yml \
	manuscript/_output.yml \
	manuscript/Cape-vs-SWA.bib \
	manuscript/journal-of-biogeography.csl \
	manuscript/style.sty
TABLES = $(wildcard manuscript/tables/*.csv)
MS_FIGURES_PNG = $(wildcard manuscript/figures/fig-*.png)
AFTER_BODY_TEX = $(AFTER_BODY_RMD:.Rmd=.tex)
OUTPUTS = $(wildcard outputs/*.csv)
SI_PDF = $(wildcard manuscript/supplementary-information/SI_*.pdf)
FUNCTIONS = $(wildcard R/functions/*.R)
MS_PDF = manuscript/_manuscript-pdf/Van-Mazijk-et-al_in-prep.pdf

# Slides
SLIDES_RMD = SAAB-AMA-SASSB-2019-talk/RvanMazijk_environmental-heterogeneity-species-richness_slides.Rmd
SLIDES_META = \
	SAAB-AMA-SASSB-2019-talk/_output.yml \
	SAAB-AMA-SASSB-2019-talk/style.sty
SLIDES_FIGURES_PNG = $(wildcard SAAB-AMA-SASSB-2019-talk/figures/fig-*.png)
SLIDES_PDF = $(SLIDES_RMD:.Rmd=.pdf)

all: manuscript slides $(SI_PDF)

manuscript: $(MS_PDF)

slides: $(SLIDES_PDF)

$(MS_PDF): $(INDEX) $(BODY) $(AFTER_BODY_TEX) $(MS_META) $(OUTPUTS) $(FUNCTIONS)
	Rscript -e "\
	setwd('manuscript');\
	library(bookdown);\
	render_book('$<', 'bookdown::pdf_document2')"

$(AFTER_BODY_TEX): $(AFTER_BODY_RMD) $(TABLES) $(MS_FIGURES_PNG) $(OUTPUTS) $(FUNCTIONS)
	Rscript -e "\
	library(rmarkdown);\
	render('$<', 'latex_fragment')"

$(SLIDES_PDF): $(SLIDES_RMD) $(SLIDES_META) $(SLIDES_FIGURES_PNG)
	Rscript -e "\
	library(rmarkdown);\
	render('$<', 'beamer_presentation')"

# TODO:
# $(MS_FIGURES_PNG): $(MS_FIGRUES_R)
# $(SI_PDF): $(SI_RMD)
