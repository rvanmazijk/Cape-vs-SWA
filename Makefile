# Paths to work in -------------------------------------------------------------

# Manuscript
MS_PATH = manuscript
SI_PATH = $(MS_PATH)/supplementary-information

# Slides
SLIDES_PATH = SAAB-AMA-SASSB-2019-talk

# Input files (components) -----------------------------------------------------

# Manuscript
INDEX = $(MS_PATH)/index.Rmd
BODY = $(wildcard $(MS_PATH)/*_*.Rmd)
AFTER_BODY_RMD = $(MS_PATH)/_after-body.Rmd
MS_META = \
	$(MS_PATH)/_bookdown.yml \
	$(MS_PATH)/_output.yml \
	$(MS_PATH)/Cape-vs-SWA.bib \
	$(MS_PATH)/journal-of-biogeography.csl \
	$(MS_PATH)/style.sty
TABLES = $(wildcard $(MS_PATH)/tables/*.csv)
MS_FIGURES_PNG = $(wildcard $(MS_PATH)/figures/fig-*.png)
AFTER_BODY_TEX = $(AFTER_BODY_RMD:.Rmd=.tex)
SI_RMD = $(wildcard $(MS_PATH)/supplementary-information/SI_*.Rmd)
OUTPUTS = $(wildcard outputs/*.csv)
FUNCTIONS = $(wildcard R/functions/*.R)

# Slides
SLIDES_RMD = \
	$(SLIDES_PATH)/RvanMazijk_environmental-heterogeneity-species-richness_slides.Rmd
SLIDES_META = \
	$(SLIDES_PATH)/_output.yml \
	$(SLIDES_PATH)/style.sty
SLIDES_FIGURES_PNG = $(wildcard $(SLIDES_PATH)/figures/fig-*.png)

# Output files (goals) ---------------------------------------------------------

# Manuscript
MS_PDF = manuscript/_manuscript-pdf/Van-Mazijk-et-al_in-prep.pdf
SI_PDF = $(SI_RMD:.Rmd=.pdf)

# Slides
SLIDES_PDF = $(SLIDES_RMD:.Rmd=.pdf)

# Rscript commands -------------------------------------------------------------
# (to use in recipes below)

RENDER_MS = Rscript -e "\
	setwd('manuscript');\
	library(bookdown);\
	render_book('$<', 'bookdown::pdf_document2')"

RENDER_AFTER_BODY_TEX = Rscript -e "\
	library(rmarkdown);\
	render('$<', 'latex_fragment')"

RENDER_SI = Rscript -e "\
	library(purrr);\
	library(rmarkdown);\
	SI_Rmd <- list.files('$(SI_PATH)',\
	  pattern = '.Rmd',\
	  full.names = TRUE\
	);\
	map(SI_Rmd, render)"

RENDER_SLIDES = Rscript -e "\
	library(rmarkdown);\
	render('$<', 'beamer_presentation')"

# Recipes (inputs -> outputs) --------------------------------------------------

all: manuscript slides

manuscript: $(MS_PDF) $(SI_PDF)

slides: $(SLIDES_PDF)

$(MS_PDF): $(INDEX) $(BODY) $(AFTER_BODY_TEX) $(MS_META) $(OUTPUTS) $(FUNCTIONS)
	$(RENDER_MS)

$(AFTER_BODY_TEX): \
	$(AFTER_BODY_RMD) $(TABLES) $(MS_FIGURES_PNG) $(OUTPUTS) $(FUNCTIONS)
	$(RENDER_AFTER_BODY_TEX)

$(SI_PDF): $(SI_RMD)
	$(RENDER_SI)

$(SLIDES_PDF): $(SLIDES_RMD) $(SLIDES_META) $(SLIDES_FIGURES_PNG)
	$(RENDER_SLIDES)

# TODO:
# $(MS_FIGURES_PNG): $(MS_FIGURES_R)
# 	...
