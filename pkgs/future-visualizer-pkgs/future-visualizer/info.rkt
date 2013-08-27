#lang info

(define collection 'multi)

(define deps '("base"
               "data-lib"
               "draw-lib"
               "pict-lib"
               "gui-lib"))
(define build-deps '("scheme-lib"
                     "scribble-lib"
                     "racket-doc"
                     "rackunit-lib"))

(define pkg-desc "Graphical performance tools for using futures")

(define pkg-authors '(jamesswaine))
