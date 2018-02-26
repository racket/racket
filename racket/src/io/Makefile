# This makefile can be used directly or driven by other makefiles.
# See "../expander/Makefile" for more notes.

RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Ignoring functions from `#%read` works beause they won't appear in
# the simplified expansion, and declaring "collect.rkt" pure works
# around a limitation of the flattener:
IGNORE = ++knot read - ++pure ../../collects/racket/private/collect.rkt

# Can be set to empty to avoid building rktio
RKTIO_DEP=../build/so-rktio/Makefile

io-src: $(RKTIO_DEP)
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) io-src-generate

GENERATE_ARGS = -t main.rkt --submod main \
                --check-depends $(BUILDDIR)compiled/io-dep.rktd \
                ++depend-module ../expander/bootstrap-run.rkt \
                ++depend ../rktio/rktio.rktl \
                --depends $(BUILDDIR)compiled/io-dep.rktd \
                --makefile-depends $(DEPENDSDIR)compiled/io.rktl $(BUILDDIR)compiled/io.d \
                -c $(BUILDDIR)compiled/cache-src \
                -k ../.. $(IGNORE) -s -x \
                -o $(BUILDDIR)compiled/io.rktl

# This target can be used with a `RACKET` that builds via `-l- setup --chain ...`
io-src-generate:
	$(RACKET) ../expander/bootstrap-run.rkt $(GENERATE_ARGS)

demo: compiled/rktio.rktl
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

demo-thread: compiled/rktio.rktl
	$(RACO) make demo-thread.rkt
	$(RACKET) demo-thread.rkt


../build/so-rktio/Makefile: ../rktio/configure ../rktio/Makefile.in ../rktio/rktio_config.h.in
	mkdir -p ../build/so-rktio
	$(MAKE) build-rktio RACKET="`$(RACKET) ../cs/absify.rkt --exec $(RACKET)`" PREFIX="`$(RACKET) ../cs/absify.rkt ../..`"

build-rktio:
	cd ../build/so-rktio; ../../rktio/configure --enable-standalone --prefix=$(PREFIX)
	cd ../build/so-rktio; make install-shared-object


.PHONY: io-src io-src-generate demo rktio build-rktio
