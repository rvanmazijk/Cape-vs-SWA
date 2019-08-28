all: analyses.pdf analyses.html

analyses.pdf: analyses.Rmd
	Rscript -e "\
	library(rmarkdown);\
	render('$<', 'pdf_document')"

analyses.html: analyses.Rmd
	Rscript -e "\
	library(rmarkdown);\
	render('$<', 'html_document')"
