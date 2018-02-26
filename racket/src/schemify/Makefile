# This makefile can be used directly or driven by other makefiles.
# See "../expander/Makefile" for more notes.

RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Ignoring functions from `#%read` works beause they won't appear in
# the simplified expansion. Make annotation references direct to
# improve performance. Declaring "collect.rkt" pure works around a
# limitation of the flattener.
IGNORE = ++knot read - ++direct kernel ++pure ../../collects/racket/private/collect.rkt

schemify-src:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) schemify-src-generate

known-src:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) known-src-generate


S_GENERATE_ARGS = -t main.rkt \
                  --check-depends $(BUILDDIR)compiled/schemify-dep.rktd \
                  ++depend-module ../expander/bootstrap-run.rkt \
                  --depends $(BUILDDIR)compiled/schemify-dep.rktd \
                  --makefile-depends $(DEPENDSDIR)compiled/schemify.rktl $(BUILDDIR)compiled/schemify.d \
                  -c $(BUILDDIR)compiled/cache-src \
                  -k ../.. $(IGNORE) -s -x \
                  -o $(BUILDDIR)compiled/schemify.rktl

schemify-src-generate:
	$(RACKET) ../expander/bootstrap-run.rkt $(S_GENERATE_ARGS)


K_GENERATE_ARGS = -t known.rkt \
                  --check-depends $(BUILDDIR)compiled/known-dep.rktd \
                  ++depend-module ../expander/bootstrap-run.rkt \
                  --depends $(BUILDDIR)compiled/known-dep.rktd \
                  --makefile-depends $(DEPENDSDIR)compiled/known.rktl $(BUILDDIR)compiled/known.d \
                  -c $(BUILDDIR)compiled/cache-src \
                  -k ../.. $(IGNORE) -s -x \
                  -o $(BUILDDIR)compiled/known.rktl


known-src-generate:
	$(RACKET) ../expander/bootstrap-run.rkt $(K_GENERATE_ARGS)

.PHONY: schemify-src schemify-src-generate known-src known-src-generate
