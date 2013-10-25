all: paper.pdf

paper.pdf: paper.dvi
	dvipdf paper.dvi
paper.dvi: paper.tex Images/fig1.eps Images/fig2.eps plots/qsize-util-err.eps plots/allan-dev.eps paper.bib paper.tex TexFiles/*.tex
	latex paper.tex 
	bibtex paper
	latex paper.tex
	latex paper.tex 
Images/fig1.eps:
	make -C Images/
Images/fig2.eps:
	make -C Images/
plots/qsize-util-err.eps:
	make -C plots/
plots/allan-dev.eps:
	make -C plots/
.PHONY: clean
clean:
	rm paper.pdf paper.dvi plots/*.eps Images/*.eps paper.aux paper.bbl paper.blg paper.log paper.toc TexFiles/*.aux
	 
