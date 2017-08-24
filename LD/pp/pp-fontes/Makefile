literate = /Users/rbonifacio/Library/Haskell/bin/lhs2TeX

pdftex = /usr/texbin/pdflatex

lhs:
	$(literate) -o slides.tex slides.lhs

slides: lhs
	$(pdftex) slides.tex

doc-tex: 
	$(literate) -o doc.tex PrettyPrinter.lhs

doc-pdf: doc-tex
	$(pdftex) doc.tex