
FWVERSION = $(shell grep ' MZSCHEME_VERSION ' $(mainsrcdir)/racket/src/schvers.h | cut -d '"' -f 2)
