SLIDES_PATH = SAAB-AMA-SASSB-2019-talk

SLIDES_RMD = $(SLIDES_PATH)/RvanMazijk_environmental-heterogeneity-species-richness_slides.Rmd
SLIDES_META = $(SLIDES_PATH)/_output.yml $(SLIDES_PATH)/style.sty
SLIDES_FIGURES_PNG = $(wildcard $(SLIDES_PATH)/figures/fig-*.png)

SLIDES_PDF = $(SLIDES_RMD:.Rmd=.pdf)

RENDER_SLIDES = Rscript -e "\
	library(rmarkdown);\
	render('$<', 'beamer_presentation')"

all: $(SLIDES_PDF)

$(SLIDES_PDF): $(SLIDES_RMD) $(SLIDES_META) $(SLIDES_FIGURES_PNG)
	$(RENDER_SLIDES)
