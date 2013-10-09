#lang info

(define collection 'multi)

(define deps '("base"
               "plot-compat"
               "plot-gui-lib"
               "plot-lib"
               "plot-doc"
               "draw-lib"
               "pict-lib"
               "rackunit-lib"
               "scribble-lib"
               "slideshow-lib"
               "typed-racket-lib"
               "typed-racket-more"
               "unstable-flonum-lib"))

(define build-deps '())

(define pkg-desc "Plot tests")

(define pkg-authors '(ntoronto))
