# This makefile can be used directly, in which case it writes to a
# "compiled" subdirectory, or it can be driven by other makefiles that
# redirect to a different build dierctory by setting `BUILDDIR` and
# other variables.

# Beware that this makefile is used both for GNU make and for nmake!

RACKET = ../../bin/racket
RACO = $(RACKET) -N raco -l- raco

# Tree for collections:
TREE = ../..

# See "boot/read-primitive.rkt" for more info:
KNOT = ++knot read read/api.rkt \
       ++knot read read/primitive-parameter.rkt \
       ++knot read read/readtable-parameter.rkt \
       ++knot read read/readtable.rkt \
       ++knot read read/number.rkt

# When flattening, replace a dynamic lookup from a primitive table to
# a direct use of the primitive name:
DIRECT = ++direct linklet ++direct kernel

# Make sure that the flattened form doesn't use
# `make-optional-keyword-procedure`:
DISALLOW = ++disallow make-optional-keyword-procedure

# Enable the sanity check for global state (to be avoided in
# favor of place-local state), but declare some initialized-once
# global state to be ok:
GLOBALS = --no-global \
          ++global-ok core-forms \
          ++global-ok core-primitives \
          ++global-ok built-in-symbols \
          ++global-ok default-read-handler \
          ++global-ok current-previously-unbound \
          ++global-ok "keep-source-locations?" \
          ++global-ok rx:path-list

# Set `BUILDDIR` as a prefix on "compiled" output (defaults to empty).
# Set `DEPENDSDIR` as the same sort of prefix in the generated
# makefile-dependency file (also defaults to empty). The `BUILDDIR`
# and `DEPENDSDIR` settings are different, because `BUILDDIR` is
# relative to here, while makefile dependencies may be needed relative
# to makefile driving the one.

expander:
	$(RACO) make bootstrap-run.rkt
	$(RACKET) bootstrap-run.rkt -c compiled/cache-src $(KNOT) $(DIRECT) $(DISALLOW) $(GLOBALS) --local-rename -O $(TREE)

expander-src:
	$(RACO) make bootstrap-run.rkt
	$(MAKE) expander-src-generate

GENERATE_ARGS = -c $(BUILDDIR)compiled/cache-src \
                --check-depends $(BUILDDIR)compiled/expander-dep.rktd \
                ++depend-module bootstrap-run.rkt \
                --depends $(BUILDDIR)compiled/expander-dep.rktd \
                --makefile-depends $(DEPENDSDIR)compiled/expander.rktl $(BUILDDIR)compiled/expander.d \
                $(KNOT) $(DIRECT) $(DISALLOW) $(GLOBALS) -k $(TREE) -s -x \
                -o $(BUILDDIR)compiled/expander.rktl

# This target can be used with a `RACKET` that builds via `-l- setup --chain ...`
expander-src-generate:
	$(RACKET) bootstrap-run.rkt $(GENERATE_ARGS)

demo:
	$(RACO) make demo.rkt
	$(RACKET) demo.rkt

run:
	$(RACO) make run.rkt
	$(RACKET) $(RKT_ARGS) run.rkt -c compiled/cache $(ARGS)

# Like `run`, but with source as compiled (as used for flattening)
run-src:
	$(RACO) make bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) bootstrap-run.rkt -s -c compiled/cache-src $(ARGS)

# Like `run`, but without using a cache for expanded and compiled linklets
run-no-cache:
	$(RACO) make run.rkt
	$(RACKET) $(RKT_ARGS) run.rkt $(ARGS)

# Writes the extracted, compiled, decompiled expander to compiled/exp.rkt
decompile:
	$(RACO) make bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) bootstrap-run.rkt -c compiled/cache-src $(KNOT) -s -x -D -o compiled/exp.rkt

# Writes the extracted, compiled expander to compiled/exp.zo
bytecode:
	$(RACO) make bootstrap-run.rkt
	$(RACKET) $(RKT_ARGS) bootstrap-run.rkt -c compiled/cache-src $(KNOT) -s -x -B -o compiled/exp.zo

.PHONY: expander expander-src expander-src-generate demo run run-no-cache
