PANDOCOPTS := \
  --highlight-style=tango \
  --latex-engine=xelatex \
  --variable mainfont="Times New Roman" \
  --variable monofont=Menlo
PANDOCHTMLOPTS := \
  -c css/buttondown.css \
	--mathjax \
	--include-in-header templates/header.html \
	--include-before-body templates/nav.html \
	--include-after-body templates/footer.html \
	--toc \
	-N
MDFILES := \
  src/index.md \
  src/part01.md \
  src/part02.md \
  src/part03.md \
  src/part04.md \
  src/part05.md \
  src/part06.md \
  src/under_development.md \
  src/q-and-a.md
HTMLFILES := \
  index.html \
  part01.html \
  part02.html \
  part03.html \
  part04.html \
  part05.html \
  part06.html \
  under_development.html \
  q-and-a.html
SOURCEDIR := src
STRICTMODE := set -euo pipefail; IFS=$$'\n\t';
RESOURCES := css/buttondown.css images/region-of-abysmal-pain.png
BASENAME := notes
OUTDIR := out

.PHONY: web
web: cleanweb ${HTMLFILES}

.PHONY: all
all: cleanall ${HTMLFILES} ${OUTDIR}/${BASENAME}.docx ${OUTDIR}/${BASENAME}.pdf ${OUTDIR}/${BASENAME}.tex

css/buttondown.css: ${SOURCEDIR}/css/buttondown.css
	@${STRICTMODE} cp $< $@

images/region-of-abysmal-pain.png: ${SOURCEDIR}/images/region-of-abysmal-pain.png
	@${STRICTMODE} cp $< $@

%.html: src/%.md ${RESOURCES}
	${STRICTMODE} pandoc ${PANDOCOPTS} ${PANDOCHTMLOPTS} -s $< | sed 's/href="\([^.]*\)\.md"/href="\1.html"/g' > $@

${OUTDIR}/${BASENAME}.docx: ${MDFILES}
	${STRICTMODE} pandoc ${PANDOCOPTS} -s -o $@ $^

${OUTDIR}/${BASENAME}.pdf: ${MDFILES}
	${STRICTMODE} pandoc ${PANDOCOPTS} -s -o $@ $^

${OUTDIR}/${BASENAME}.tex: ${MDFILES}
	${STRICTMODE} pandoc ${PANDOCOPTS} -s -o $@ $^

.PHONY: cleanweb
cleanweb:
	@${STRICTMODE} rm -f ${HTMLFILES} ${RESOURCES}

.PHONY: cleanall
cleanall:
	@${STRICTMODE} rm -f ${HTMLFILES} ${RESOURCES}
	@${STRICTMODE} rm -f ${OUTDIR}/${BASENAME}.*
