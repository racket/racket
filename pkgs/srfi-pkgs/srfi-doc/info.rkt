#lang info

(define collection 'multi)

(define build-deps '("mzscheme-doc"
                     "scheme-lib"
                     "base"
                     "scribble-lib"
                     "srfi-lib"
                     "racket-doc"
                     "r5rs-doc"
                     "r6rs-doc"
                     "compatibility-lib"))
(define update-implies '("srfi-lib"))

(define pkg-desc "documentation part of \"srfi\"")

(define pkg-authors '(mflatt noel chongkai jay))
