#lang info

(define collection "unstable")

(define scribblings
  '(("scribblings/unstable.scrbl" (multi-page) (experimental))
    ("scribblings/gui/unstable-gui.scrbl" (multi-page) (experimental))))
(define deps '("base"
               "draw-lib"
               "gui-lib"
               "pict-lib"
               "scribble-lib"
               "slideshow-lib"
               "unstable-macro-testing-lib"))
(define imples '("unstable-macro-testing-lib"))
(define build-deps '("eli-tester"
                     "racket-test"
                     "srfi-lib"
                     "syntax-color-lib"
                     "draw-doc"
                     "gui-doc"
                     "pict-doc"
                     "slideshow-doc"
                     "scribble-doc"
                     "at-exp-lib"
                     "compatibility-lib"
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
