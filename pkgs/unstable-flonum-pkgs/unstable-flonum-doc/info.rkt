#lang info

(define collection "unstable")

(define scribblings
  '(("unstable-flonum.scrbl" (multi-page) (experimental))))

(define deps '("base"
               "scribble-lib"
               "unstable"
               "unstable-flonum-lib"))

(define build-deps '("plot" ; used for an example
                     "racket-doc"))
