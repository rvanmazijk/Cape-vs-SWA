# Setup ------------------------------------------------------------------------

PROJECT = .

MANUSCRIPT = $(PROJECT)/manuscript
MANUSCRIPT_RMD = $(wildcard $(MANUSCRIPT)/*.{Rmd,bib,csl})
MANUSCRIPT_HTML = \
	$(MANUSCRIPT)/_book/index.html \
	$(MANUSCRIPT)/_book/introduction.html \
	$(MANUSCRIPT)/_book/materials-and-methods.html \
	$(MANUSCRIPT)/_book/results.html \
	$(MANUSCRIPT)/_book/discussion.html \
	$(MANUSCRIPT)/_book/biosketches.html \
	$(MANUSCRIPT)/_book/orcid-nos-.html \
	$(MANUSCRIPT)/_book/references.html \
	$(MANUSCRIPT)/_book/tables.html \
	$(MANUSCRIPT)/_book/figures.html \
	$(MANUSCRIPT)/_book/appendix.html
MANUSCRIPT_PDF = $(MANUSCRIPT)/_book/_main.pdf

# TODO
#FIGURES = $(wildcard $(PROJECT)/figures/*.png)
#FIGURES_R = $(wildcard $(FIGURES)/*.R)
#OUTPUTS = $(PROJECT)/outputs/*.{csv,RDS,R}

# Define bookdown::render_book()-calls -----------------------------------------

RENDER_HTML = Rscript -e " \
	setwd('$(MANUSCRIPT)'); \
	require(bookdown); \
	render_book('$<', 'bookdown::gitbook')"

RENDER_PDF = Rscript -e " \
	setwd('$(MANUSCRIPT)'); \
	require(bookdown); \
	render_book('$<', 'bookdown::pdf_book')"

# Describe "MAKE" dependencies -------------------------------------------------

all: gitbook pdf
gitbook: $(MANUSCRIPT_HTML)
pdf: $(MANUSCRIPT_PDF) $(MANUSCRIPT_RMD)

$(MANUSCRIPT)/%.html: $(MANUSCRIPT)/index.Rmd $(MANUSCRIPT_RMD)
	$(RENDER_HTML)

$(MANUSCRIPT_PDF): $(MANUSCRIPT)/index.Rmd
	$(RENDER_PDF)

# TODO
#$(MANUSCRIPT)/%.html: $(FIGURES) $(OUTPUTS)
#$(FIGURES)/%.png: $(FIGURES_R)/%.R
#	Rscript -e "source('$<')"
#$(FIGURES): $(OUTPUTS)
#$(OUTPUTS): calls in figure-setup.R and analyses/ to make outputs
