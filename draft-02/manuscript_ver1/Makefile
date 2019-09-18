all: 02_materials-and-methods_rvm.docx 03_results_rvm.docx

02_materials-and-methods_rvm.docx: 02_materials-and-methods_rvm.Rmd Cape-vs-SWA.bib journal-of-biogeography.csl style.docx
	Rscript -e "\
	library(rmarkdown);\
	render('$<')"

03_results_rvm.docx: 03_results_rvm.Rmd Cape-vs-SWA.bib journal-of-biogeography.csl style.docx
	Rscript -e "\
	library(rmarkdown);\
	render('$<')"
