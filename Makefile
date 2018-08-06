# Setup ------------------------------------------------------------------------

# Input files
FIGURES_R = $(wildcard figures/fig-*.R)
FIGURES = $(FIGURES_R:.R=.png)
INDEX = manuscript/index.Rmd
META = \
	manuscript/_bookdown.yml \
	manuscript/_output.yml \
	manuscript/Cape-vs-SWA.bib \
	manuscript/journal-of-biogeography.csl
BODY = $(wildcard manuscript/*.Rmd)

# Output files
GITBOOK = \
	manuscript/_manuscript/index.html \
	manuscript/_manuscript/introduction.html \
	manuscript/_manuscript/materials-and-methods.html \
	manuscript/_manuscript/results.html \
	manuscript/_manuscript/discussion.html \
	manuscript/_manuscript/tables.html \
	manuscript/_manuscript/figures.html \
	manuscript/_manuscript/references.html
PDF = manuscript/_manuscript/_manuscript.pdf

# Define bookdown::render_book()-calls -----------------------------------------

# Body
RENDER_GITBOOK = Rscript -e \
	"setwd('manuscript'); \
	library(bookdown); \
	render_book('$<', 'bookdown::gitbook')"
RENDER_PDF = Rscript -e \
	"setwd('manuscript'); \
	library(bookdown); \
	render_book('$<', 'bookdown::pdf_document2')"

# After-body fragments
# (setwd("manuscript") not needed for fragments)
RENDER_AFTER_BODY_HTML = Rscript -e \
	"library(rmarkdown); \
	render('$<', 'html_fragment')"
RENDER_AFTER_BODY_TEX = Rscript -e \
	"library(rmarkdown); \
	render('$<', 'latex_fragment', )"

# Describe "MAKE" dependencies -------------------------------------------------

all: gitbook pdf

gitbook:
	echo "Not rendering properly at the moment (Re: table floats, figure sizes)"
  #$(GITBOOK)

pdf: $(PDF)

$(GITBOOK): $(INDEX) $(META) $(BODY) manuscript/_after-body.html $(FIGURES)
	$(RENDER_GITBOOK)

$(PDF): $(INDEX) $(META) $(BODY) manuscript/_after-body.tex $(FIGURES)
	$(RENDER_PDF)

manuscript/_after-body.html: manuscript/_after-body.Rmd
	$(RENDER_AFTER_BODY_HTML)

manuscript/_after-body.tex: manuscript/_after-body.Rmd
	$(RENDER_AFTER_BODY_TEX)

# TODO: $(BODY) $(FIGURES): $(OUTPUTS)
# At the moment, calls in figure-setup.R and analyses/ to make outputs

figures/fig-%.png: figures/fig-%.R
	Rscript -e "source('$<')"
