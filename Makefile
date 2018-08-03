# Setup ------------------------------------------------------------------------

# Input files
FIGURES_R = $(wildcard figures/fig-*.R)
FIGURES = $(FIGURES_R:.R=.png)
INDEX = manuscript/index.Rmd
META = $(wildcard manuscript/*.{yml,bib,csl})
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
	manuscript/_manuscript/appendix.html \
	manuscript/_manuscript/references.html
PDF = manuscript/_manuscript/manuscript.pdf

# Define bookdown::render_book()-calls -----------------------------------------

RENDER_GITBOOK = Rscript -e \
	"setwd('manuscript'); \
	require(bookdown); \
	render_book('$<', 'bookdown::gitbook')"

RENDER_PDF = Rscript -e \
	"setwd('manuscript'); \
	require(bookdown); \
	render_book('$<', 'bookdown::pdf_book')"

# Describe "MAKE" dependencies -------------------------------------------------

all: gitbook pdf

gitbook: $(GITBOOK)

pdf: $(PDF)

$(GITBOOK): $(INDEX) $(META) $(BODY) $(FIGURES)
	$(RENDER_GITBOOK)

$(PDF): $(INDEX) $(META) $(BODY) $(FIGURES)
	$(RENDER_PDF)

# TODO: $(BODY) $(FIGURES): $(OUTPUTS)
# At the moment, calls in figure-setup.R and analyses/ to make outputs

figures/fig-%.png: figures/fig-%.R
	Rscript -e "source('$<')"
