#lang info

(define collection "mysterx")

(define scribblings '(("scribblings/mysterx.scrbl" (multi-page) (legacy))))
(define deps '("scheme-lib"
               "base"))
(define build-deps '("racket-doc"
                     "at-exp-lib"
                     "scribble-lib"))

(define pkg-desc "Legacy library for working with COM on Windows")

(define pkg-authors '(mflatt))
