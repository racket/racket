# This makefile can be used directly or driven by other makefiles.
# See "../expander/Makefile" for more notes.

RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Ignoring functions from `#%read` works beause they won't appear in
# the simplified expansion, and declaring "collect.rkt" pure works
# around a limitation of the flattener:
IGNORE = ++knot read - ++pure ../../collects/racket/private/collect.rkt

regexp-src:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) regexp-src-generate

GENERATE_ARGS = -t main.rkt \
                --check-depends $(BUILDDIR)compiled/regexp-dep.rktd \
                ++depend-module ../expander/bootstrap-run.rkt \
                --depends $(BUILDDIR)compiled/regexp-dep.rktd \
                --makefile-depends $(DEPENDSDIR)compiled/regexp.rktl $(BUILDDIR)compiled/regexp.d \
                -c $(BUILDDIR)compiled/cache-src \
                -k ../.. $(IGNORE) -s -x \
                -o $(BUILDDIR)compiled/regexp.rktl

# This target can be used with a `RACKET` that builds via `-l- setup --chain ...`
regexp-src-generate:
	$(RACKET) ../expander/bootstrap-run.rkt $(GENERATE_ARGS)

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

# Writes the extracted, compiled, decompiled expander to compiled/regexp.rkt
decompile:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) ../expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src $(IGNORE) -s -x -D -o compiled/regexp.rkt

.PHONY: regexp-src regexp-src-generate demo decompile
