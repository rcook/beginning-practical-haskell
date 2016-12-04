OUTDIR := doc
PANDOCOPTS := \
  --highlight-style=tango \
  --latex-engine=xelatex \
  --variable mainfont="Times New Roman" \
  --variable monofont=Menlo \
  -c buttondown.css \
  --self-contained
BASENAME := notes
INPUTFILES := \
  index.md \
  part01.md \
  part02.md \
  part03.md \
  part04.md \
  part05.md \
  part06.md \
  q-and-a.md

.PHONY: all
all: clean ${OUTDIR}/${BASENAME}.docx ${OUTDIR}/${BASENAME}.html ${OUTDIR}/${BASENAME}.pdf ${OUTDIR}/${BASENAME}.tex

${OUTDIR}/${BASENAME}.docx: ${INPUTFILES}
	pandoc ${PANDOCOPTS} -s -o $@ $^

${OUTDIR}/${BASENAME}.html: ${INPUTFILES}
	pandoc ${PANDOCOPTS} -s -o $@ $^

${OUTDIR}/${BASENAME}.pdf: ${INPUTFILES}
	pandoc ${PANDOCOPTS} -s -o $@ $^

${OUTDIR}/${BASENAME}.tex: ${INPUTFILES}
	pandoc ${PANDOCOPTS} -s -o $@ $^

.PHONY: clean
clean:
	rm -rf ${OUTDIR}/${BASENAME}*
