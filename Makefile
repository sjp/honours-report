DOCUMENT = dissertation
DOCUMENT_TEX = $(DOCUMENT).tex
DOCUMENT_PDF = $(DOCUMENT).pdf

# Adding in custom args to pdflatex
# The minted package requires -shell-escape
# default is just PDFLATEX_ARGS = 
PDFLATEX_ARGS = -shell-escape

all:
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	bibtex $(DOCUMENT)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	-rm $(DOCUMENT).log $(DOCUMENT).out $(DOCUMENT).aux $(DOCUMENT).toc $(DOCUMENT).lof $(DOCUMENT).lot $(DOCUMENT).bbl $(DOCUMENT).blg

show:
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	bibtex $(DOCUMENT)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	-rm $(DOCUMENT).log $(DOCUMENT).out $(DOCUMENT).aux $(DOCUMENT).toc $(DOCUMENT).lof $(DOCUMENT).lot $(DOCUMENT).bbl $(DOCUMENT).blg
	evince $(DOCUMENT_PDF) &

all-dirty:
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	bibtex $(DOCUMENT)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)

show-dirty:
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	bibtex $(DOCUMENT)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	pdflatex $(PDFLATEX_ARGS) $(DOCUMENT_TEX)
	evince $(DOCUMENT_PDF) &

clean:
	-rm $(DOCUMENT).log $(DOCUMENT).out $(DOCUMENT).aux $(DOCUMENT).toc $(DOCUMENT).lof $(DOCUMENT).lot $(DOCUMENT).bbl $(DOCUMENT).blg $(DOCUMENT_PDF)
