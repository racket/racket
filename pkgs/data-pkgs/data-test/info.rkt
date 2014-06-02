#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "tests for \"data-lib\"")

(define pkg-authors '(ryanc))
(define build-deps '("racket-index"
                     "data-lib"
                     "rackunit-lib"))
