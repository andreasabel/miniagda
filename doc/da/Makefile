default: da.pdf


da.pdf : da.tex intro.tex legal.tex main.tex prelim.tex concl.tex bibl.tex da.bib
	pdflatex da
	bibtex da
	pdflatex da