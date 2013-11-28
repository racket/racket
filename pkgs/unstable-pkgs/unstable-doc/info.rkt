#lang info

(define collection "unstable")

(define scribblings
  '(("scribblings/unstable.scrbl" (multi-page) (experimental))
    ("scribblings/gui/unstable-gui.scrbl" (multi-page) (experimental))))
(define deps '("base"))
(define build-deps '("rackunit-doc"
                     "scheme-lib"
                     "at-exp-lib"
                     "compatibility-lib"
                     "draw-lib"
                     "gui-lib"
                     "pict-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-lib"
                     "slideshow-lib"
                     "typed-racket-lib"
                     "unstable-contract-lib"
                     "unstable-debug-lib"
                     "unstable-lib"
                     "unstable-list-lib"
                     "unstable-macro-testing-lib"
                     "unstable-options-lib"
                     "unstable-parameter-group-lib"
                     "unstable-pretty-lib"
                     "unstable-2d"
                     "draw-doc"
                     "gui-doc"
                     "pict-doc"
                     "scribble-doc"
                     "slideshow-doc"))

(define pkg-desc "documentation part of \"unstable\"")

(define pkg-authors '(jay samth cce ryanc))
