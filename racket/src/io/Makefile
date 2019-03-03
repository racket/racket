# This makefile can be used directly or driven by other makefiles.
# See "../expander/Makefile" for more notes.

RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Can be set to empty to avoid building rktio
RKTIO_DEP=../build/so-rktio/Makefile

# When flattening, replace a dynamic lookup from a primitive table to
# a direct use of the primitive name:
DIRECT = ++direct thread ++direct pthread

# Enable the sanity check for global state (to be avoided in
# favor of place-local state), but declare some initialized-once
# global state to be ok:
GLOBALS = --no-global \
          ++global-ok installed-read \
          ++global-ok installed-read-syntax \
          ++global-ok installed-read-accept-reader \
          ++global-ok installed-read-accept-lang \
          ++global-ok maybe-raise-missing-module \
          ++global-ok "string->number?" \
          ++global-ok "printable-regexp?" \
          ++global-ok do-global-print \
          ++global-ok simplify-path/dl \
          ++global-ok exec-file \
          ++global-ok run-file \
          ++global-ok collects-dir \
          ++global-ok config-dir \
          ++global-ok addon-dir \
          ++global-ok host-collects-dir \
          ++global-ok host-config-dir

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
                -k ../.. -s -x $(DIRECT) $(GLOBALS) \
                -o $(BUILDDIR)compiled/io.rktl

# This target can be used with a `RACKET` that builds via `-l- setup --chain ...`
io-src-generate:
	$(RACKET) ../expander/bootstrap-run.rkt $(GENERATE_ARGS)

demo: $(RKTIO_DEP)
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

demo-thread: $(RKTIO_DEP)
	$(RACO) make demo-thread.rkt
	$(RACKET) demo-thread.rkt


../build/so-rktio/Makefile: ../rktio/configure ../rktio/Makefile.in ../rktio/rktio_config.h.in
	mkdir -p ../build/so-rktio
	$(MAKE) build-rktio RACKET="`$(RACKET) ../cs/absify.rkt --exec $(RACKET)`" PREFIX="`$(RACKET) ../cs/absify.rkt ../..`"

build-rktio:
	cd ../build/so-rktio; ../../rktio/configure --enable-standalone --prefix=$(PREFIX)
	cd ../build/so-rktio; make install-shared-object


.PHONY: io-src io-src-generate demo rktio build-rktio
