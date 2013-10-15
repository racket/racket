#lang info

(define collection 'multi)

(define build-deps '("mzscheme-doc"
                     "scheme-lib"
                     "scribble-lib"
                     "r5rs-lib"
                     "compatibility-lib"
                     "racket-doc"))
(define deps '("base"))

(define pkg-desc "documentation part of \"r5rs\"")

(define pkg-authors '(mflatt))
