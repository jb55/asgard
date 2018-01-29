
asgard: Main.hs
	ghc -O -isrc -o $@ $<

tags: TAGS

TAGS: FORCE
	hasktags -b -c src

.PHONY: FORCE
