default: da.pdf


da.pdf : da.tex intro.tex legal.tex main.tex concl.tex da.bib appendix.tex
	pdflatex da
	bibtex da
	pdflatex da
	pdflatex da
