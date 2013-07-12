#lang info

(define collection "unstable")

(define scribblings
  '(("gui/unstable-redex.scrbl" (multi-page) (experimental))))

(define deps '("base"
               "pict-lib"
               "redex-lib"
               "scribble-lib"
               "unstable"))

(define build-deps '("pict-doc"
                     "redex-doc"
                     "racket-doc"))
