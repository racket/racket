#lang info

(define collection "unstable")

(define scribblings
  '(("scribblings/unstable.scrbl" (multi-page) (experimental))
    ("scribblings/gui/unstable-gui.scrbl" (multi-page) (experimental))))
(define deps '("base"
               "draw-lib"
               "gui-lib"
               "pict-lib"
               "redex-lib"
               "scribble-lib"
               "slideshow-lib"))
(define build-deps '("scribble-doc"
                     "plot"
                     "at-exp-lib"
                     "compatibility-lib"
                     "macro-debugger"
                     "racket-doc"
                     "rackunit-lib"
                     "typed-racket-lib"
                     "unstable-contract-lib"
                     "unstable-flonum-lib"
                     "unstable-list-lib"
                     "unstable-options-lib"
                     "unstable-debug-lib"
                     "unstable-pretty-lib"
                     "unstable-parameter-group-lib"))
