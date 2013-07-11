#lang info

(define collection "mysterx")

(define scribblings '(("scribblings/mysterx.scrbl" (multi-page) (legacy))))
(define deps '("base"))
(define build-deps '("racket-doc"
                     "at-exp-lib"
                     "scribble-lib"))
