default: da.pdf


da.pdf : da.tex intro.tex legal.tex main.tex concl.tex bibl.tex da.bib
	pdflatex da
	bibtex da
	pdflatex da
	pdflatex da
