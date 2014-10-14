#lang info

(define collection 'multi)

(define deps '("base" "racket-doc"))
(define build-deps '("draw-doc"
                     "gui-doc"
                     "htdp-doc"
                     "pict-doc"
                     "slideshow-doc"
                     "at-exp-lib"
                     "scribble-lib"
                     "gui-lib"
                     "htdp-lib"
                     "pict-lib"
                     "redex-gui-lib"
                     "redex-benchmark"
                     "rackunit-lib"))

(define pkg-desc "documentation part of \"redex\"")

(define pkg-authors '(robby bfetscher))
