PANDOCOPTS := \
  --highlight-style=tango \
  --latex-engine=xelatex \
  --variable mainfont="Times New Roman" \
  --variable monofont=Menlo \
  -c css/buttondown.css
HTMLFILES := \
  index.html \
  part01.html \
  part02.html \
  part03.html \
  part04.html \
  part05.html \
  part06.html \
  q-and-a.html
SOURCEDIR := src
STRICTMODE := set -euo pipefail; IFS=$$'\n\t';
RESOURCES := css/buttondown.css images/region-of-abysmal-pain.png

.PHONY: all
all: clean ${HTMLFILES}

css/buttondown.css: ${SOURCEDIR}/css/buttondown.css
	@${STRICTMODE} cp $< $@

images/region-of-abysmal-pain.png: ${SOURCEDIR}/images/region-of-abysmal-pain.png
	@${STRICTMODE} cp $< $@

%.html: src/%.md ${RESOURCES}
	@${STRICTMODE} pandoc ${PANDOCOPTS} -s $< | sed 's/href="\([^.]*\)\.md"/href="\1.html"/g' > $@

.PHONY: clean
clean:
	@${STRICTMODE} rm -f ${HTMLFILES} ${RESOURCES}
