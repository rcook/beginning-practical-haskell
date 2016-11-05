OUTDIR := doc
PANDOCOPTS := \
  --highlight-style=tango \
  --latex-engine=xelatex \
  --variable mainfont="Times New Roman" \
  --variable monofont=Menlo
INPUTFILES := \
  index.md \
  part01.md \
  part02.md \
  part03.md \
  part04.md

${OUTDIR}/notes.html: ${INPUTFILES}
	pandoc ${PANDOCOPTS} -s -o $@ $^

${OUTDIR}/notes.pdf: ${INPUTFILES}
	pandoc ${PANDOCOPTS} -s -o $@ $^

${OUTDIR}/notes.tex: ${INPUTFILES}
	pandoc ${PANDOCOPTS} -s -o $@ $^

.PHONY: clean
clean:
	rm -rf ${OUTDIR}/notes.html
	rm -rf ${OUTDIR}/notes.pdf
	rm -rf ${OUTDIR}/notes.tex
