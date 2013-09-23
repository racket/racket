#lang info

(define collection "make")

(define scribblings '(("make.scrbl" (multi-page) (tool-library))))
(define deps '("scheme-lib"
               "base"
               "cext-lib"
               "compiler-lib"
               "compatibility-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))

(define pkg-desc "Simple timestamp- and dependency-triggered actions")

(define pkg-authors '(mflatt))
