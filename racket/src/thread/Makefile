# This makefile can be used directly or driven by other makefiles.
# See "../expander/Makefile" for more notes.

RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

thread-src:
	$(RACO) make ../expander/bootstrap-run.rkt
	$(MAKE) thread-src-generate

# When flattening, replace a dynamic lookup from a primitive table to
# a direct use of the primitive name:
DIRECT = ++direct pthread

# Enable the sanity check for global state (to be avoided in
# favor of place-local state), but declare some initialized-once
# global state to be ok:
GLOBALS = --no-global \
          ++global-ok the-sandman \
          ++global-ok sync-on-channel \
          ++global-ok post-shutdown-action \
          ++global-ok get-subprocesses-time \
          ++global-ok force-atomic-timeout-callback

GENERATE_ARGS = -t main.rkt --submod main \
                --check-depends $(BUILDDIR)compiled/thread-dep.rktd \
                ++depend-module ../expander/bootstrap-run.rkt \
                --depends $(BUILDDIR)compiled/thread-dep.rktd \
                --makefile-depends $(DEPENDSDIR)compiled/thread.rktl $(BUILDDIR)compiled/thread.d \
                -c $(BUILDDIR)compiled/cache-src \
                -k ../.. -s -x $(DIRECT) $(GLOBAL) \
                -o $(BUILDDIR)compiled/thread.rktl

# This target can be used with a `RACKET` that builds via `-l- setup --chain ...`
thread-src-generate:
	$(RACKET) ../expander/bootstrap-run.rkt $(GENERATE_ARGS)

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

.PHONY: thread-src thread-src-generate demo
