#lang info

(define collection 'multi)

(define deps '("base"
               "math-lib"
               "racket-test"
               "rackunit-lib"
               "typed-racket-lib"
               "typed-racket-more"))

(define build-deps '())
(define update-implies '("math-lib"))

(define pkg-desc "Math library tests")

(define pkg-authors '(ntoronto))
