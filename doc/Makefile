# Wyrd documentation makefile
all:	manual.pdf manual.html post-build-cleanup wyrd.1 wyrdrc.5

src/manual.tex: src/manual.tex.in
	latex2man -CLATEX -L $< $@

manual.pdf: src/manual.tex
	cd src && pdflatex manual.tex && pdflatex manual.tex && pdflatex manual.tex
	mv src/manual.pdf .

manual.html: src/manual.tex
	cd src && hevea -fix manual.tex
	mv src/manual.html .

src/manual.tex.stripped: src/manual.tex.in
	python src/remove-tt.py $< $@

wyrd.1: src/manual.tex.stripped
	latex2man -M $< $@

wyrdrc.5: src/manual.tex.stripped
	latex2man -CWYRDRC -M $< $@

post-build-cleanup: manual.pdf manual.html wyrd.1 wyrdrc.5
	cd src && rm -f *.aux *.log *.toc *.haux *.htoc *.stripped *.html manual.tex

clean:
	rm -f manual.tex *.aux *.log *.toc *.haux *.htoc *.pdf *.html *.stripped wyrd.1 wyrdrc.5
	cd src && rm -f manual.tex *.aux *.log *.toc *.haux *.htoc *.pdf *.html *.stripped wyrd.1 wyrdrc.5


