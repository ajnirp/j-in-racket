all: overview clean

overview: overview.tex
	latex overview.tex
	dvipdf overview.dvi

clean:
	rm -f overview.dvi
	rm -f overview.aux
	rm -f overview.log
	rm -f overview.out

cleanpdf:
	rm -f overview.pdf
