DOCUMENT = report
DOCUMENT_RNW = $(DOCUMENT).Rnw
DOCUMENT_TEX = $(DOCUMENT).tex
DOCUMENT_PDF = $(DOCUMENT).pdf

# Adding in custom args to pdflatex
# The minted package requires -shell-escape
# default is just PDFLATEX_ARGS = 
PDFLATEX_ARGS = -shell-escape

all:
	-rm $(DOCUMENT_PDF)
	R CMD Sweave $(DOCUMENT_RNW)
	-rm Rplots.pdf
	find . -name "*.svg" -maxdepth 1 -exec inkscape --export-area-page -z --file={} --export-pdf={}.pdf \;
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	bibtex $(DOCUMENT)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	-rm $(DOCUMENT_TEX) $(DOCUMENT).log $(DOCUMENT).out $(DOCUMENT).aux $(DOCUMENT).toc $(DOCUMENT).lof $(DOCUMENT).lol $(DOCUMENT).lot $(DOCUMENT).bbl $(DOCUMENT).blg *.svg*

show:
	-rm $(DOCUMENT_PDF)
	R CMD Sweave $(DOCUMENT_RNW)
	-rm Rplots.pdf
	find . -name "*.svg" -maxdepth 1 -exec inkscape --export-area-page -z --file={} --export-pdf={}.pdf \;
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	bibtex $(DOCUMENT)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	-rm $(DOCUMENT_TEX) $(DOCUMENT).log $(DOCUMENT).out $(DOCUMENT).aux $(DOCUMENT).toc $(DOCUMENT).lof $(DOCUMENT).lol $(DOCUMENT).lot $(DOCUMENT).bbl $(DOCUMENT).blg *.svg*
	evince $(DOCUMENT_PDF) &

all-dirty:
	-rm $(DOCUMENT_PDF)
	R CMD Sweave $(DOCUMENT_RNW)
	-rm Rplots.pdf
	find . -name "*.svg" -maxdepth 1 -exec inkscape --export-area-page -z --file={} --export-pdf={}.pdf \;
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	bibtex $(DOCUMENT)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)

show-dirty:
	-rm $(DOCUMENT_PDF)
	R CMD Sweave $(DOCUMENT_RNW)
	-rm Rplots.pdf
	find . -name "*.svg" -maxdepth 1 -exec inkscape --export-area-page -z --file={} --export-pdf={}.pdf \;
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	bibtex $(DOCUMENT)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	evince $(DOCUMENT_PDF) &

clean:
	-rm $(DOCUMENT_TEX) $(DOCUMENT).log $(DOCUMENT).out $(DOCUMENT).aux $(DOCUMENT).toc Rplots.pdf $(DOCUMENT).lof $(DOCUMENT).lol $(DOCUMENT).lot $(DOCUMENT).bbl $(DOCUMENT).blg $(DOCUMENT).pyg $(DOCUMENT_PDF) *.svg*
