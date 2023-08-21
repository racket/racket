RACO=raco
RUN_SCRIBBLE=$(RACO) scribble --text --dest-name LICENSE.txt
DOC=scribblings/main/license.scrbl

.PHONY: render
render:
	$(RUN_SCRIBBLE) ++arg "--repo-root-dir" --dest ../.. $(DOC)
	$(RUN_SCRIBBLE) ++arg "--repo-src-dir" --dest ../../racket/src $(DOC)
