OUT_DIR := doc
PANDOC_OPTS := --highlight-style=tango

${OUT_DIR}/notes.html: index.md part01.md part02.md part03.md part04.md
	pandoc ${PANDOC_OPTS} -s -o $@ $^

.PHONY: clean
clean:
	rm -rf ${OUT_DIR}/notes.html
