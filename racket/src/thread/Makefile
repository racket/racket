# This makefile can be used directly or driven by other makefiles.
# See "../expander/Makefile" for more notes.

RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Ignoring functions from `#%read` works beause they won't appear in
# the simplified expansion, and declaring "collect.rkt" pure works
# around a limitation of the flattener:
IGNORE = ++knot read - ++direct pthread ++pure ../../collects/racket/private/collect.rkt

thread-src:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) thread-src-generate

GENERATE_ARGS = -t main.rkt --submod main \
                --check-depends $(BUILDDIR)compiled/thread-dep.rktd \
                ++depend-module ../expander/bootstrap-run.rkt \
                --depends $(BUILDDIR)compiled/thread-dep.rktd \
                --makefile-depends $(DEPENDSDIR)compiled/thread.rktl $(BUILDDIR)compiled/thread.d \
                -c $(BUILDDIR)compiled/cache-src \
                -k ../.. $(IGNORE) -s -x \
                -o $(BUILDDIR)compiled/thread.rktl

# This target can be used with a `RACKET` that builds via `-l- setup --chain ...`
thread-src-generate:
	$(RACKET) ../expander/bootstrap-run.rkt $(GENERATE_ARGS)

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

.PHONY: thread-src thread-src-generate demo
