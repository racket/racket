#lang info

(define collection "eopl")
(define deps '("base"
               "compatibility-lib"))
(define scribblings '(("eopl.scrbl" () (teaching -20))))
(define build-deps '("rackunit-lib"
                     "racket-doc"
                     "scribble-lib"))
