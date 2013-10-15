#lang info

(define collection 'multi)

(define deps '("planet-lib"
               "scribble-lib"
               "base"))

(define pkg-desc "documentation part of \"planet\"")

(define pkg-authors '(mflatt))
(define build-deps '("racket-doc"
                     "scribble-doc"))
