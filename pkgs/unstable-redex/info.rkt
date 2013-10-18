#lang info

(define collection "unstable")

(define scribblings
  '(("gui/unstable-redex.scrbl" (multi-page) (experimental))))

(define deps '("base"
               "pict-lib"
               "redex-gui-lib"
               "scribble-lib"
               "unstable-lib"))

(define build-deps '("pict-doc"
                     "redex-doc"
                     "racket-doc"
                     "unstable-doc"))

(define pkg-desc "Experimental libraries for typesetting PLT Redex models")

(define pkg-authors '(ryanc))
