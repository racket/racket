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

# Make sure that the flattened form doesn't use `error`:
DISALLOW = ++disallow error

# Enable the sanity check for global state (to be avoided in
# favor of place-local state), but declare some initialized-once
# global state to be ok:
GLOBALS = --no-global \
          ++global-ok the-sandman \
          ++global-ok post-shutdown-action \
          ++global-ok get-subprocesses-time \
          ++global-ok pre-poll-callbacks \
          ++global-ok queued-shutdowns \
          ++global-ok compute-memory-sizes \
          ++global-ok "computed-memory-sizes?" \
          ++global-ok custodians-with-limits \
          ++global-ok make-place-ports+fds \
          ++global-ok pthread-count \
          ++global-ok "logging-future-events?" \
          ++global-ok log-future-event \
          ++global-ok "logging-place-events?" \
          ++global-ok log-place-event \
          ++global-ok thread-engine-for-roots \
          ++global-ok TICKS

GENERATE_ARGS = -t main.rkt \
                --check-depends $(BUILDDIR)compiled/thread-dep.rktd \
                ++depend-module ../expander/bootstrap-run.rkt \
                --depends $(BUILDDIR)compiled/thread-dep.rktd \
                --makefile-depends $(DEPENDSDIR)compiled/thread.rktl $(BUILDDIR)compiled/thread.d \
                -c $(BUILDDIR)compiled/cache-src \
                -k ../.. -s -x $(DIRECT) $(DISALLOW) $(GLOBALS) \
                -o $(BUILDDIR)compiled/thread.rktl

# This target can be used with a `RACKET` that builds via `-l- setup --chain ...`
thread-src-generate:
	$(RACKET) ../expander/bootstrap-run.rkt $(GENERATE_ARGS)

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

.PHONY: thread-src thread-src-generate demo
