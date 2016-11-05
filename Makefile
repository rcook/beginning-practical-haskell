OUTDIR := doc
PANDOCOPTS := \
  --highlight-style=tango \
  --latex-engine=xelatex \
  --variable mainfont="Times New Roman" \
  --variable monofont=Menlo
BASENAME := notes
INPUTFILES := \
  index.md \
  part01.md \
  part02.md \
  part03.md \
  part04.md

${OUTDIR}/${BASENAME}.html: ${INPUTFILES}
	pandoc ${PANDOCOPTS} -s -o $@ $^

${OUTDIR}/${BASENAME}.pdf: ${INPUTFILES}
	pandoc ${PANDOCOPTS} -s -o $@ $^

${OUTDIR}/${BASENAME}.tex: ${INPUTFILES}
	pandoc ${PANDOCOPTS} -s -o $@ $^

.PHONY: clean
clean:
	rm -rf ${OUTDIR}/${BASENAME}*
