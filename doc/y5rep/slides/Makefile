%.pdf: %.tex
	pdflatex $<
	pdfcrop $@ crop-$@
	mv crop-$@ $@

%.svg: %.pdf
	pdf2svg $< $@

all: opt-hier.svg area-mindmap.svg \
    opt-terms.svg \
	bg-depmod-bn-example.svg bg-depmod-mrf.svg \
	greedy-sensor-metaphor.svg greedy-algorithm.svg \
	thr-markov-chain.svg \
	dumb-mouse.svg smart-mouse.svg \
	ackley-x.svg himmelblau-x.svg svm-x.svg \
	svm-shape-usps-x.svg svm-shape-svmguide2-x.svg \
	wafer-model.svg \
	app-solver.svg app-toolkit-components.svg

clean:
	rm -f *.aux *.log *.eps *.dvi *.pdf *.nav *.out *.snm

dumb-mouse.dvi: dumb-mouse.tex mouse.tex
smart-mouse.dvi: smart-mouse.tex mouse.tex


# 3d svg figures from R need cropping

%-x.svg: %.svg
	sed '/<svg/ {s/height="504pt"/height="252pt"/; s/viewBox="0 0 504 504"/viewBox="0 126 504 252"/}' < $<  > $@

