#lang info

(define collection "unstable")

(define deps '("base"
               "draw-lib"
               "gui-lib"
               "pict-lib"
               "scribble-lib"
               "slideshow-lib"
               "unstable-macro-testing-lib"))
(define implies '("unstable-macro-testing-lib"))
(define build-deps '())

(define pkg-desc "implementation (no documentation) part of \"unstable\"")

(define pkg-authors '(jay samth cce ryanc))
