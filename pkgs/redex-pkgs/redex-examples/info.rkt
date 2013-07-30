#lang info

(define collection 'multi)

(define deps '("base"
               "compiler-lib"
               "rackunit-lib"
               "redex-lib"
               "slideshow-lib"))
(define build-deps '("at-exp-lib"
                     "scribble-lib"))

(define pkg-desc "PLT Redex examples")

(define pkg-authors '(robby bfetscher))
