# This makefile can be used directly or driven by other makefiles.
# See "../expander/Makefile" for more notes.

RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

regexp-src:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) regexp-src-generate

GENERATE_ARGS = -t main.rkt \
                --check-depends $(BUILDDIR)compiled/regexp-dep.rktd \
                ++depend-module ../expander/bootstrap-run.rkt \
                --depends $(BUILDDIR)compiled/regexp-dep.rktd \
                --makefile-depends $(DEPENDSDIR)compiled/regexp.rktl $(BUILDDIR)compiled/regexp.d \
                -c $(BUILDDIR)compiled/cache-src \
                -k ../.. -s -x --no-global \
                -o $(BUILDDIR)compiled/regexp.rktl

# This target can be used with a `RACKET` that builds via `-l- setup --chain ...`
regexp-src-generate:
	$(RACKET) ../expander/bootstrap-run.rkt $(GENERATE_ARGS)

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

# Writes the extracted, compiled, decompiled regexp to compiled/regexp.rkt
decompile:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) ../expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src -s -x -D -o compiled/regexp.rkt

# Writes the extracted, compiled regexp to compiled/exp.zo
bytecode:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) ../expander/bootstrap-run.rkt -t main.rkt -c compiled/cache-src -s -x -B -o compiled/regexp.zo

.PHONY: regexp-src regexp-src-generate demo decompile
