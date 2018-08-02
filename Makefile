# Setup ------------------------------------------------------------------------

PROJECT = .

MANUSCRIPT = $(PROJECT)/manuscript
MANUSCRIPT_RMD = $(wildcard $(MANUSCRIPT)/*.{Rmd,yml,bib,csl})
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
# TODO
#MANUSCRIPT_PDF = $(MANUSCRIPT)/.*pdf

# TODO
#FIGURES = $(wildcard $(PROJECT)/figures/*.png)
#FIGURES_R = $(wildcard $(FIGURES)/*.R)
#OUTPUTS = $(PROJECT)/outputs/*.{csv,RDS,R}

# Define bookdown::render_book()-call ------------------------------------------

RENDER = Rscript -e " \
	setwd('$(MANUSCRIPT)'); \
	require(bookdown); \
	render_book('$<')"

# TODO
#RENDER_PDF =

# Describe "MAKE" dependencies -------------------------------------------------

manuscript: $(MANUSCRIPT_HTML)

$(MANUSCRIPT)/%.html: $(MANUSCRIPT)/index.Rmd
	$(RENDER)

# TODO
#$(MANUSCRIPT)/%.pdf: $(MANUSCRIPT)/index.Rmd
#	$(RENDER_PDF)

# TODO
#$(MANUSCRIPT)/%.html: $(FIGURES) $(OUTPUTS)

#$(FIGURES)/%.png: $(FIGURES_R)/%.R
#	Rscript -e "source('$<')"

#$(FIGURES): $(OUTPUTS)

#$(OUTPUTS): calls in figure-setup.R and analyses/ to make outputs
