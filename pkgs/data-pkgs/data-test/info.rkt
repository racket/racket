#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "tests for \"data-lib\"")

(define pkg-authors '(ryanc))
(define build-deps '("data-lib"
                     "rackunit-lib"
                     "scribble-lib"))
