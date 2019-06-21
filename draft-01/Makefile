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
MS_META = $(wildcard $(MS_PATH)/*.{bib,csl,yml}) $(wildcard $(MS_PATH)/style.*)
TABLES = $(wildcard $(MS_PATH)/tables/*.csv)
MS_FIGURES_PNG = $(wildcard $(MS_PATH)/figures/fig-*.png)
SI_RMD = $(wildcard $(MS_PATH)/supplementary-information/SI_*.Rmd)
OUTPUTS = $(wildcard outputs/*.csv)
FUNCTIONS = $(wildcard R/functions/*.R)

# Slides
SLIDES_RMD = $(SLIDES_PATH)/RvanMazijk_environmental-heterogeneity-species-richness_slides.Rmd
SLIDES_META = $(SLIDES_PATH)/_output.yml $(SLIDES_PATH)/style.sty
SLIDES_FIGURES_PNG = $(wildcard $(SLIDES_PATH)/figures/fig-*.png)

# Output files (goals) ---------------------------------------------------------

# Manuscript
MS_DOCX = manuscript/_manuscript/Van-Mazijk-et-al_in-prep.docx
MS_PDF = manuscript/_manuscript/Van-Mazijk-et-al_in-prep.pdf
SI_PDF = $(SI_RMD:.Rmd=.pdf)

# Slides
SLIDES_PDF = $(SLIDES_RMD:.Rmd=.pdf)

# Rscript commands -------------------------------------------------------------
# (to use in recipes below)

RENDER_MS_DOCX = Rscript -e "\
	setwd('manuscript');\
	library(bookdown);\
	render_book('$<', 'bookdown::word_document2')"

RENDER_MS_PDF = Rscript -e "\
	setwd('manuscript');\
	library(bookdown);\
	render_book('$<', 'bookdown::pdf_document2')"

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

all: manuscript slides website

manuscript: $(MS_DOCX) $(MS_PDF) $(SI_PDF)

slides: $(SLIDES_PDF)

website: index.md

$(MS_DOCX): $(INDEX) $(BODY) $(MS_META) $(OUTPUTS) $(FUNCTIONS)
	$(RENDER_MS_DOCX)

$(MS_PDF): $(INDEX) $(BODY) $(MS_META) $(OUTPUTS) $(FUNCTIONS)
	$(RENDER_MS_PDF)

$(SI_PDF): $(SI_RMD)
	$(RENDER_SI)

$(SLIDES_PDF): $(SLIDES_RMD) $(SLIDES_META) $(SLIDES_FIGURES_PNG)
	$(RENDER_SLIDES)
