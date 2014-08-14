#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "documentation part of \"data\"")

(define pkg-authors '(ryanc))
(define build-deps '("data-lib"
                     "racket-doc"
                     "scribble-lib"))
(define update-implies '("data-lib"))
