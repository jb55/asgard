
rustydiced: Main.hs
	ghc -O -o $@ $<

tags: TAGS

TAGS: FORCE
	hasktags -b -c src

.PHONY: FORCE
