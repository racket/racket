
FWVERSION = $(shell grep ' MZSCHEME_VERSION ' $(mainsrcdir)/mzscheme/src/schvers.h | cut -d '"' -f 2)
