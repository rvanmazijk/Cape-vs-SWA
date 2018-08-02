# Setup ------------------------------------------------------------------------

# Input files
INDEX = manuscript/index.Rmd
META = \
	manuscript/_output.yml \
	manuscript/*.bib \
	manuscript/*.csl
BODY = manuscript/*.Rmd

# Output files
OUT = manuscript/_book
GITBOOK = \
	$(OUT)/index.html \
	$(OUT)/introduction.html \
	$(OUT)/materials-and-methods.html \
	$(OUT)/results.html \
	$(OUT)/discussion.html \
	$(OUT)/biosketches.html \
	$(OUT)/orcid-nos-.html \
	$(OUT)/references.html \
	$(OUT)/tables.html \
	$(OUT)/figures.html \
	$(OUT)/appendix.html
PDF = $(OUT)/*.pdf

#FIGURES = $(wildcard figures/*.png)
#FIGURES_R = $(wildcard figures/*.R)
#OUTPUTS = $(outputs/*.{csv,RDS,R})

# Define bookdown::render_book()-calls -----------------------------------------

RENDER_GITBOOK = Rscript -e " \
	setwd('manuscript'); \
	require(bookdown); \
	render_book('$<', 'bookdown::gitbook')"

RENDER_PDF = Rscript -e " \
	setwd('manuscript'); \
	require(bookdown); \
	render_book('$<', 'bookdown::pdf_book')"

# Describe "MAKE" dependencies -------------------------------------------------

all: gitbook pdf

gitbook: $(GITBOOK)

pdf: $(PDF)

$(OUT)/%.html: $(INDEX) $(META) $(BODY)
	$(RENDER_GITBOOK)

$(OUT)/%.pdf: $(INDEX) $(META) $(BODY)
	$(RENDER_PDF)

#manuscript/%.html: $(FIGURES) $(OUTPUTS)
#figures/%.png: figures/%.R
#	Rscript -e "source('$<')"
#$(FIGURES): $(OUTPUTS)
#$(OUTPUTS):  # calls in figure-setup.R and analyses/ to make outputs
