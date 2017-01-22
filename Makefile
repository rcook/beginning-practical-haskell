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
ALL_OBJS :=

.PHONY: all
all: web ${OUTDIR}/${BASENAME}.docx ${OUTDIR}/${BASENAME}.pdf ${OUTDIR}/${BASENAME}.tex

.PHONY: rebuild
rebuild: clean all

.PHONY: web
web: ${HTMLFILES}
ALL_OBJS += ${HTMLFILES}

%.html: src/%.md ${RESOURCES}
	@echo Making $@
	@${STRICTMODE} pandoc ${PANDOCOPTS} ${PANDOCHTMLOPTS} -s $< | sed 's/href="\([^.]*\)\.md"/href="\1.html"/g' > $@

css/buttondown.css: ${SOURCEDIR}/css/buttondown.css
	@echo Making $@
	@${STRICTMODE} cp $< $@
ALL_OBJS += css/buttondown.css

images/region-of-abysmal-pain.png: ${SOURCEDIR}/images/region-of-abysmal-pain.png
	@echo Making $@
	@${STRICTMODE} cp $< $@
ALL_OBJS += images/region-of-abysmal-pain.png

${OUTDIR}/${BASENAME}.docx: ${MDFILES} images/region-of-abysmal-pain.png
	@echo Making $@
	@${STRICTMODE} pandoc ${PANDOCOPTS} -s -o $@ ${MDFILES}
ALL_OBJS += ${OUTDIR}/${BASENAME}.docx

${OUTDIR}/${BASENAME}.pdf: ${MDFILES} images/region-of-abysmal-pain.png
	@echo Making $@
	@${STRICTMODE} pandoc ${PANDOCOPTS} -s -o $@ ${MDFILES}
ALL_OBJS += ${OUTDIR}/${BASENAME}.pdf

${OUTDIR}/${BASENAME}.tex: ${MDFILES}
	@echo Making $@
	@${STRICTMODE} pandoc ${PANDOCOPTS} -s -o $@ $^
ALL_OBJS += ${OUTDIR}/${BASENAME}.tex

.PHONY: clean
clean:
	@echo Cleaning everything
	@rm -rf ${ALL_OBJS}
