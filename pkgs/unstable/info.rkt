#lang setup/infotab

(define collection "unstable")

(define scribblings
  '(("scribblings/unstable.scrbl" (multi-page) (experimental))))
(define deps '("base"
               "draw-lib"
               "gui-lib"
               "pict-lib"
               "redex"
               "scribble-lib"
               "slideshow-lib"))
(define build-deps '("at-exp-lib"
                     "compatibility-lib"
                     "macro-debugger"
                     "racket-doc"
                     "rackunit-lib"
                     "typed-racket-lib"
                     "unstable-contract-lib"
                     "unstable-list-lib"
                     "unstable-options-lib"
                     "unstable-parameter-group-lib"))
