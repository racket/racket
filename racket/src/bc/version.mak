
FWVERSION_X = $(shell grep 'define MZSCHEME_VERSION_X ' $(mainsrcdir)/version/racket_version.h | cut -d ' ' -f 3)
FWVERSION_Y = $(shell grep 'define MZSCHEME_VERSION_Y ' $(mainsrcdir)/version/racket_version.h | cut -d ' ' -f 3)
FWVERSION_Z = $(shell grep 'define MZSCHEME_VERSION_Z ' $(mainsrcdir)/version/racket_version.h | cut -d ' ' -f 3)
FWVERSION_W = $(shell grep 'define MZSCHEME_VERSION_W ' $(mainsrcdir)/version/racket_version.h | cut -d ' ' -f 3)

ifneq ($(FWVERSION_W),0)
FWVERSION = $(FWVERSION_X).$(FWVERSION_Y).$(FWVERSION_Z).$(FWVERSION_W)
else ifneq ($(FWVERSION_Z),0)
FWVERSION = $(FWVERSION_X).$(FWVERSION_Y).$(FWVERSION_Z)
else
FWVERSION = $(FWVERSION_X).$(FWVERSION_Y)
endif
