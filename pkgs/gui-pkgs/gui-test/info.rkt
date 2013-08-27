#lang info

(define collection 'multi)

(define deps '("base"
               "gui-lib"
               "rackunit-lib"
               "scribble-lib"
               "pconvert-lib"
               "compatibility-lib"))
(define build-deps '("scheme-lib"
                     "draw-lib"
                     "racket-test"
                     "sgl"
                     "snip-lib"
                     "wxme-lib"
                     ))

(define pkg-desc "tests for \"gui\"")

(define pkg-authors '(mflatt))
